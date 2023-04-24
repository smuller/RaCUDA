// LEADS TO A MEMORY ACCESS ERROR

#include <stdio.h>
#include <stdlib.h>
#include <cuda_runtime.h>
#include <assert.h>
#include <math.h>
#include <time.h>

#define BLOCKSIZE 16
// Include the original and optimized kernel definitions here
// ...
__global__ void matrixColumnNorm(float* d_in, float* d_out, float* d_mean, float* d_sd, int N, int nt,int c)
{  
  unsigned int thread_id = threadIdx.y;
  
  d_out[thread_id+blockIdx.x*N] = (d_in[thread_id+blockIdx.x*N] - d_mean[blockIdx.x]) / d_sd[blockIdx.x];	
	
	for(int i=0;i<c;i++){
		if((nt+thread_id)+blockIdx.x*N < N*N){
			d_out[(nt+thread_id)+blockIdx.x*N] = (d_in[(nt+thread_id)+blockIdx.x*N] - d_mean[blockIdx.x])/d_sd[blockIdx.x];
		}	
  	}
}

__global__ void matrixColumnNormOptimized ( float *d_in,  float *d_out,  float *d_mean,  float *d_sd,  int N,  int nt,  int c) {

    signed long int array_idx_d_in1;
    signed long int lower_bound_d_in1;
    lower_bound_d_in1 = nt + (N * blockIdx.x);
    array_idx_d_in1 = ((blockIdx.x * blockDim.x) + threadIdx.x) - lower_bound_d_in1;
    __shared__ float d_in1[BLOCKSIZE];
    d_in1[threadIdx.x] = d_in[(blockIdx.x * blockDim.x) + threadIdx.x];
    signed long int array_idx_d_in2;
    signed long int lower_bound_d_in2;
    lower_bound_d_in2 = blockIdx.x * N;
    array_idx_d_in2 = ((blockIdx.x * blockDim.x) + threadIdx.x) - lower_bound_d_in2;
    __shared__ float d_in2[BLOCKSIZE];
    d_in2[threadIdx.x] = d_in[(blockIdx.x * blockDim.x) + threadIdx.x];
    signed long int array_idx_d_out1;
    signed long int lower_bound_d_out1;
    lower_bound_d_out1 = nt + (blockIdx.x * N);
    array_idx_d_out1 = ((blockIdx.x * blockDim.x) + threadIdx.x) - lower_bound_d_out1;
    __shared__ float d_out1[BLOCKSIZE];
    d_out1[threadIdx.x] = d_out[(blockIdx.x * blockDim.x) + threadIdx.x];
    signed long int array_idx_d_out2;
    signed long int lower_bound_d_out2;
    lower_bound_d_out2 = N * blockIdx.x;
    array_idx_d_out2 = ((blockIdx.x * blockDim.x) + threadIdx.x) - lower_bound_d_out2;
    __shared__ float d_out2[BLOCKSIZE];
    d_out2[threadIdx.x] = d_out[(blockIdx.x * blockDim.x) + threadIdx.x];
    signed long int array_idx_d_mean1;
    signed long int lower_bound_d_mean1;
    lower_bound_d_mean1 = blockIdx.x;
    array_idx_d_mean1 = ((blockIdx.x * blockDim.x) + threadIdx.x) - lower_bound_d_mean1;
    __shared__ float d_mean1[BLOCKSIZE];
    d_mean1[threadIdx.x] = d_mean[(blockIdx.x * blockDim.x) + threadIdx.x];
    signed long int array_idx_d_sd1;
    signed long int lower_bound_d_sd1;
    lower_bound_d_sd1 = blockIdx.x;
    array_idx_d_sd1 = ((blockIdx.x * blockDim.x) + threadIdx.x) - lower_bound_d_sd1;
    __shared__ float d_sd1[BLOCKSIZE];
    d_sd1[threadIdx.x] = d_sd[(blockIdx.x * blockDim.x) + threadIdx.x];
    __syncthreads();
    unsigned int thread_id;
    thread_id = threadIdx.y;
    d_out2[array_idx_d_out2] = (d_in2[array_idx_d_in2] - d_mean1[array_idx_d_mean1]) / d_sd1[array_idx_d_sd1];
    int i;
    for (i = 0; i < c; i = i + 1) {

        if ((nt + thread_id) + (blockIdx.x * N) < N * N) {

            d_out1[array_idx_d_out1] = (d_in1[array_idx_d_in1] - d_mean1[array_idx_d_mean1]) / d_sd1[array_idx_d_sd1];
        } else {


        }
    }
    d_out[(blockIdx.x * blockDim.x) + threadIdx.x] = d_out1[threadIdx.x];
    d_out[(blockIdx.x * blockDim.x) + threadIdx.x] = d_out2[threadIdx.x];
}

void check_cuda_error(cudaError_t err) {
    if (err != cudaSuccess) {
        fprintf(stderr, "Error: %s\n", cudaGetErrorString(err));
        exit(EXIT_FAILURE);
    }
}

int main(int argc, char **argv) {
    int N = 1024; // Matrix size (change as needed)
    int blockSize = 16; // Number of threads per block in the Y dimension
    int gridSize = (N + blockSize - 1) / blockSize;
    int c = (N + blockSize - 1) / blockSize;

    // Allocate and initialize host memory
    float *h_in = (float *)malloc(N * N * sizeof(float));
    float *h_out = (float *)malloc(N * N * sizeof(float));
    float *h_mean = (float *)malloc(N * sizeof(float));
    float *h_sd = (float *)malloc(N * sizeof(float));
    for (int i = 0; i < N * N; i++) {
        h_in[i] = rand() / (float)RAND_MAX;
    }
    for (int i = 0; i < N; i++) {
        h_mean[i] = rand() / (float)RAND_MAX;
        h_sd[i] = rand() / (float)RAND_MAX;
    }

    // Allocate device memory
    float *d_in, *d_out, *d_mean, *d_sd;
    check_cuda_error(cudaMalloc((void **)&d_in, N * N * sizeof(float)));
    check_cuda_error(cudaMalloc((void **)&d_out, N * N * sizeof(float)));
    check_cuda_error(cudaMalloc((void **)&d_mean, N * sizeof(float)));
    check_cuda_error(cudaMalloc((void **)&d_sd, N * sizeof(float)));

    // Copy data from host to device
    check_cuda_error(cudaMemcpy(d_in, h_in, N * N * sizeof(float), cudaMemcpyHostToDevice));
    check_cuda_error(cudaMemcpy(d_mean, h_mean, N * sizeof(float), cudaMemcpyHostToDevice));
    check_cuda_error(cudaMemcpy(d_sd, h_sd, N * sizeof(float), cudaMemcpyHostToDevice));

    // Create and start CUDA events for timing
    cudaEvent_t start, stop;
    check_cuda_error(cudaEventCreate(&start));
    check_cuda_error(cudaEventCreate(&stop));
    float elapsedTime;

    // Launch the original kernel
    check_cuda_error(cudaEventRecord(start, 0));
    matrixColumnNorm<<<gridSize, blockSize>>>(d_in, d_out, d_mean, d_sd, N, blockSize, c);
    check_cuda_error(cudaEventRecord(stop, 0));
    check_cuda_error(cudaEventSynchronize(stop));
    check_cuda_error(cudaEventElapsedTime(&elapsedTime, start, stop));
    printf("Original kernel execution time: %f ms\n", elapsedTime);

    // Launch the optimized kernel
    check_cuda_error(cudaEventRecord(start, 0));
    // Replace with the optimized kernel function name
    matrixColumnNormOptimized<<<gridSize, blockSize>>>(d_in, d_out, d_mean, d_sd, N, blockSize, c);
    check_cuda_error(cudaEventRecord(stop, 0));
    check_cuda_error(cudaEventSynchronize(stop));
    check_cuda_error(cudaEventElapsedTime(&elapsedTime, start, stop));
    printf("Optimized kernel execution time: %f ms\n", elapsedTime);

    // Clean up resources
    check_cuda_error(cudaEventDestroy(start));
    check_cuda_error(cudaEventDestroy(stop));
    check_cuda_error(cudaFree(d_in));
    check_cuda_error(cudaFree(d_out));
    check_cuda_error(cudaFree(d_mean));
    check_cuda_error(cudaFree(d_sd));
    free(h_in);
    free(h_out);
    free(h_mean);
    free(h_sd);

    return 0;
}
