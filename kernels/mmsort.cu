/*
 * (C) Copyright 2016-2018 Ben Karsin, Nodari Sitchinava
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */


#include<stdio.h>
#include<iostream>
#include<fstream>
#include<vector>
#include<cmath>
#include<random>
#include<algorithm>
#include "params.h"
#include "cmp.hxx"
#include "basecase/squareSort.hxx"

#define K 4 // Branching factor (lists merged per round)
#define PL 2 // Log of K
#define PIPELINE 1 // Set to 1 to enable pipelining heap merging

#define M 1024 // Size of base case (keep at 1024)
#define THREADS 32 // Threads per block
#define W 32
//#define B 32
//#define LOGB 5
#define RANGE 1048576 // Range of randomly generated values
#define ELTS 32

// Print out current contents of heap.  Used for debugging
__global__ void printHeap(int* heap) {
  if(threadIdx.x==0) {
    for(int i=1; i<=K; i*=2) {
      for(int j=0; j<i; j++) {
        for(int k=0; k<32; k++) {
          printf("%d ", heap[(i-1+j)*32+k]);
        }
        printf("- ");
      }
      printf("\n");
    }
printf("\n\n");
  }
}

// Merge two nodes of our minBlockHeap.  Each node contains 32 elements and the merge
//is done with 32 threads (1 warp) using a bitonic merge network with shfl 

__global__ _Bool xorMergeNodes(int* a, int* b, int* out) {
  int tid = threadIdx.x%W;
  _Bool direction=1;
  int aVal = a[tid];
  int bVal = b[32-1-tid];
  int aTemp;
  int bTemp;
  _Bool down;
  _Bool testA;
  _Bool testB;

  aTemp = myMin(aVal,bVal); 
  bVal = myMax(aVal,bVal);
  aVal = aTemp;

  direction = f(b[32-1], a[32-1]);


  for(int i=32/2; i>0; i=(i>>1)) {
    aTemp = shfl_wrapper(aVal, i, W);
    bTemp = shfl_wrapper(bVal, i, W);

    testA = f(aVal, aTemp);
    testB = f(bVal, bTemp);
    if (tid % (i * 2) < i) {
    	testA = 1 - testA;
    	testB = 1 - testB;
    }
    if(testA) aVal = aTemp;
    if(testB) bVal = bTemp;
  }
  out[tid] = aVal;

  if(direction) {
    a[tid] = bVal;
    b[tid] = MAXVAL;
  }
  else {
    b[tid] = bVal;
    a[tid] = MAXVAL;
  }
  return direction;
}

// merge two nodes and store the resulting values in registers

__global__ int mergeIntoReg(int* heap, int minNode, int maxNode) {
  int tid = threadIdx.x%W;
  int* a = heap+(32*minNode);
  int* b = heap+(32*maxNode);
  int aVal = a[tid];
  int bVal = b[32-1-tid];
  int aTemp;
  int bTemp;
  _Bool down;

  aTemp = myMin(aVal,bVal); 
  bVal = myMax(aVal,bVal);
  aVal = aTemp;


  for(int i=32/2; i>0; i=(i>>1)) {
    down = g(tid); // XXX (tid < (tid ^ ((i<<1)-1)));

    aTemp = shfl_wrapper(aVal, i, W);
    bTemp = shfl_wrapper(bVal, i, W);

//    aTemp = __shfl_xor(aVal, i, W);
//    bTemp = __shfl_xor(bVal, i, W);

    if(down) {
      aVal = myMin(aVal,aTemp);
      bVal = myMin(bVal,bTemp);
    }
    else {
      aVal = myMax(aVal,aTemp);
      bVal = myMax(bVal,bTemp);
    }
  }

  b[tid] = bVal;
  return aVal;
}

// Method to read a block of B elements from an input list and storing it in the leaf of our heap

__global__ void fillEmptyLeaf(int* input, int* heap, int listNum, int* start, int* end, int size, int tid) {
  heap[((K-1+listNum)*32)+tid] = MAXVAL;
  if(start[listNum]+tid < end[listNum]) {
    heap[((K-1+listNum)*32)+tid] = input[start[listNum]+(size*listNum)+tid];
    if(tid==0)
      start[listNum] += 32;
  }
}

// Each thread searches down path to find node to work on

__global__ int findNodeInPath(int* heap, int nodeGroup, int LOGK) {
  int nodeIdx=0;
  for(int i=0; i<nodeGroup; i++) {
    nodeIdx ++;
    nodeIdx += (cmp(heap[(2*nodeIdx+1)*32+(32-1)], heap[(2*nodeIdx+2)*32+(32-1)]));
  }
  return nodeIdx;
}


__global__ int fillRegsFromPathBit(int* elts, int* heap, int tid) {
  int path=1;

  for(int i=0; i<PL<<1; i+=2) {
    path = path << 1;
    elts[i] = heap[(path-1)*32+tid];
    elts[i+1] = heap[(path)*32+tid];
    if (heap[(path-1)*32+(32-1)] > heap[(path)*32+(32-1)]) 
      path = path + 1;
  }
  return path;
}

// Each thread searches down path to find node to work on

__global__ void fillRegsFromPath(int* elts, int* heap, int* path, int tid) {
  int idx=0;
  path[0]=0;


  for(int i=0; i<PL; i++) {
    elts[idx] = heap[((2*path[i]+1)<<5)+tid];
    elts[idx+1] = heap[((2*path[i]+2)<<5)+(32-1)-tid];
    idx+=2;

    path[i+1] = path[i]*2+1;
    path[i+1] += 1 - f(heap[(path[i+1]<<5)+(32-1)], heap[((path[i+1]+1)<<5)+(32-1)]);
  }
}


__global__ void xorMergeGeneral(int* elts, int* heap, int tid) {

  int temp[2*PL];
  _Bool down;


  for(int j=0; j<PL<<1; j+=2) {
      temp[j]=myMin(elts[j],elts[j+1]);
      elts[j+1] = myMax(elts[j],elts[j+1]);
      elts[j]=temp[j];
  }


  for(int i=32/2; i>0; i=(i>>1)) {


    for(int j=0; j<PL<<1; j++) {
      temp[j] = shfl_wrapper(elts[j], i, W);
    }
    //down = (tid & i);



    for(int j=0; j<PL<<1; j++) {
      if(tid % (i * 2) >= i) { // XXX
        elts[j] = myMax(temp[j],elts[j]);
      }
      else {
        elts[j] = myMin(temp[j],elts[j]);
      }
    }
  }     
}

// Fill an empty node with the merge of its children
// Pipelined version to increase ILP

__global__ int heapifyEmptyNodePipeline(int* heap, int* path, int tid) {
  int elts[2*PL];
  fillRegsFromPath(elts, heap, path, tid);

  xorMergeGeneral(elts, heap, tid);

  int idx=0;

  for(int i=0; i<PL; i++) {
    heap[(path[i]<<5)+tid] = elts[idx];
    heap[((path[i+1]-1+((path[i+1]%2)<<1))<<5)+tid] = elts[idx+1];
    idx+=2;
  }
  return path[PL];

}

// Fill an empty node with the merge of its children

__global__ int heapifyEmptyNode(int* node, int nodeIdx, int altitude, int tid) {
  _Bool direction;
  for(int i=0; i<altitude; i++) {
    direction = xorMergeNodes(node+((2*nodeIdx+1)*32), node+((2*nodeIdx+2)*32), node+(nodeIdx*32));
    nodeIdx+=(nodeIdx+1);
    if(direction) //If new empty is right child
      nodeIdx++;
  }
  // Return index of leaf node that needs to be filled from global memory read
  return nodeIdx; 
}

// Build a minBlockHeap bottom-up by merging pairs of nodes and filling empty nodes from global memory
__global__ void buildHeap(int* input, int* heap, int* start, int* end, int size, int tid) {
  int nodeIdx;
  // Fill leaf nodes

  for(int i=0; i<K; i++) {
    if(start[i]+tid < end[i])
      heap[((K-1+i)<<5)+tid] = input[start[i]+(size*i)+tid]; 
    else
      heap[((K-1+i)<<5)+tid] = MAXVAL;
  }
  if(tid < K)
    start[tid] += 32;

// Go through each level of the tree, merging children to make new nodes
// each merge propagates down to leaf where a new block is taken from input
  int nodesAtLevel=K>>1;

  for(int i=1; nodesAtLevel > 0; i++) {

    for(int j=0; j<nodesAtLevel; j++) {
      nodeIdx = (nodesAtLevel-1+j);
      nodeIdx = heapifyEmptyNode(heap, nodeIdx, i, tid);
      fillEmptyLeaf(input, heap, nodeIdx-(K-1), start, end, size, tid);
    }
    nodesAtLevel = nodesAtLevel >> 1;
  }
}

// Merge K lists into one using 1 warp

__global__ void multimergePipeline(int* input, int* output, int* start, int* end, int size, int outputOffset) {
  int warpInBlock = threadIdx.x>>5;
  int tid = threadIdx.x % W;
  __shared__ int heapData[32*(2*K-1)*(THREADS>>5)]; // Each warp in the block needs its own shared memory
  int* heap = heapData+((32*(2*K-1))*warpInBlock);


  int path[PL+1];


  buildHeap(input, heap, start, end, size,tid);

  int outputIdx=tid+outputOffset;
  int nodeIdx;

  while(heap[32-1] != MAXVAL) {
    output[outputIdx] = heap[tid];
    outputIdx += 32;
    nodeIdx = heapifyEmptyNodePipeline(heap, path, tid);
    fillEmptyLeaf(input, heap, nodeIdx-(K-1), start, end, size, tid);
  }

  if(heap[tid] != MAXVAL) {
    output[outputIdx] = heap[tid];
  }

}

// Merge K lists into one using 1 warp

__global__ void multimerge(int* input, int* output, int* start, int* end, int size, int outputOffset) {
  int warpInBlock = threadIdx.x>>5;
  int tid = threadIdx.x % W;
  __shared__ int heapData[32*(2*K-1)*(THREADS/W)]; // Each warp in the block needs its own shared memory
  int* heap = heapData+((32*(2*K-1))*warpInBlock);

  int LOGK=PL;

  buildHeap(input, heap, start, end, size, tid);
  int outputIdx=tid+outputOffset;
  int nodeIdx;

  while(heap[32-1] != MAXVAL) {
    output[outputIdx] = heap[tid];
    outputIdx += 32;
    nodeIdx = heapifyEmptyNode(heap, 0, LOGK, tid);
    fillEmptyLeaf(input, heap, nodeIdx-(K-1), start, end, size, tid);
  }

  if(heap[tid] != MAXVAL) {
    output[outputIdx] = heap[tid];
  }
}


__global__ void blockBuildHeap(int* input, int* heap, int* start, int* end, int size) {
  
}


__global__ int findListToRead(int* heap, int LOGK) {
  int nodeIdx=0;
  for(int i=0; i<LOGK-1; i++) {
    nodeIdx = 2*nodeIdx + 1;
    if (heap[nodeIdx*32 + (32-1)] > heap[(nodeIdx+1)*32 + (32-1)])
    	nodeIdx++;
  }
  return nodeIdx - (K-1);
}


__global__ int findMergeNode(int* heap, int warpInBlock, _Bool* dir) {
  int nodeIdx=0;
  _Bool tempDir = 0;
  for(int i=0; i<warpInBlock; i++) {
    nodeIdx = 2*nodeIdx + 1;
    if (heap[nodeIdx*32 + (32-1)] > heap[(nodeIdx+1)*32 + (32-1)])
      tempDir = 1;
    nodeIdx += tempDir;
  }
  *dir = 1 - tempDir;
  return nodeIdx;
}

__global__ void blockMultimerge(int* input, int* output, int* start, int* end, int size, int outputOffset) {
  int warpInBlock = threadIdx.x/W;
  int tid = threadIdx.x%W;
  __shared__ int heap[32*(2*K-1)]; // Each block shares a single heap

  int LOGK=1;
  int tempK=K;
  while(tempK > 0) { tempK = tempK >> 1; LOGK++; }

  if(warpInBlock == 0) {
    buildHeap(input, heap, start, end, size, tid);
  }
  int outputIdx = tid+outputOffset;
__syncthreads(); 
  if(warpInBlock==0) {
int count=0;
    while(heap[tid] != MAXVAL) {
    // write out root
      output[outputIdx] = heap[tid];
      outputIdx+=W;
count++;
      __syncthreads();
      __syncthreads();
    }
  } 
  else if(warpInBlock == LOGK){
    int listNum;
    int readVal;
    while(heap[0]!=MAXVAL) {
      readVal = MAXVAL;
      listNum = findListToRead(heap, LOGK); // search down to leaf
    // read in new leaf to register
      if(start[listNum]+tid < end[listNum]) {
        readVal = input[start[listNum]+(size*listNum)+tid];
      } 
        start[listNum] += 32;
    __syncthreads();

    heap[(K-1+listNum)*32+tid] = readVal;   // write register to heap node
    __syncthreads();
    }
  }
  else {
    int minNode;
    int maxNode;
    int regVal;
    _Bool dir;
    while(heap[0] != MAXVAL) {
      // search down path
      minNode = findMergeNode(heap, warpInBlock, &dir);
      maxNode = minNode-1 + dir + dir;
    // merge nodes & write max 
      regVal = mergeIntoReg(heap, minNode, maxNode);
      __syncthreads();
      heap[((maxNode-1)/2)*32 + tid] = regVal; // write register to heap node
      __syncthreads();
    }
  }
}

__global__ void multimergeLevel(int* data, int* output, int* pivots, long size, int tasks, int P) {
  int totalWarps = P*(THREADS/W);
  int warpInBlock = threadIdx.x/W;
  int warpIdx = (blockIdx.x)*(THREADS/W)+warpInBlock;
  int tid = threadIdx.x%W;


  __shared__ int startRaw[K*(THREADS/W)];
  int* start = startRaw+(warpInBlock*K);
  __shared__ int endRaw[K*(THREADS/W)];
  int* end = endRaw+(warpInBlock*K);

  int warpsPerTask = totalWarps/tasks;
  if(warpsPerTask == 0) warpsPerTask=1;
  int myTask = warpIdx/warpsPerTask;
  long taskOffset = size*K*myTask;

  if(myTask < tasks) {

    if(tid<K) {
      start[tid] = pivots[(warpIdx*K)+tid];
//      if(start[tid]%32 != 0)
//        start[tid] = start[tid] -start[tid]%32;
    }

    if(tid<K) 
      end[tid] = pivots[(totalWarps*K)+tid];
    if(warpIdx % warpsPerTask < warpsPerTask-1 && tid < K)
      end[tid] = pivots[((warpIdx+1)*K)+tid];

    int outputOffset=0;
    for(int i=0; i<K; i++)
      outputOffset+=start[i];

    multimerge(data+taskOffset, output+taskOffset, start, end, size, outputOffset);
  } 
}

