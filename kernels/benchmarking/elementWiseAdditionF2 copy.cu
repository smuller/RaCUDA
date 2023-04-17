#include <iostream>
#include <vector>
#include <chrono>
#include <cuda_runtime.h>
#include <device_launch_parameters.h>

// Use a constant for TILE_DIM, e.g., 32
#define TILE_DIM 32

__global__ void elementWiseAddition ( float *a,  float *b,  float *c,  int N) {

    signed long int lower_bound_a1;
    lower_bound_a1 = 0;
    __shared__ float a1[TILE_DIM];
    a1[threadIdx.x] = a[((blockIdx.x * blockDim.x) + threadIdx.x) + lower_bound_a1];
    signed long int lower_bound_b1;
    lower_bound_b1 = 0;
    __shared__ float b1[TILE_DIM];
    b1[threadIdx.x] = b[((blockIdx.x * blockDim.x) + threadIdx.x) + lower_bound_b1];
    signed long int lower_bound_c1;
    lower_bound_c1 = 0;
    __shared__ float c1[TILE_DIM];
    c1[threadIdx.x] = c[((blockIdx.x * blockDim.x) + threadIdx.x) + lower_bound_c1];
    int i;
    for (i = 0; i < N; i = i + 1) {
    
        int j;
        for (j = 0; j < N; j = j + 1) {
        
            c1[threadIdx.x - lower_bound_c1] = a1[threadIdx.x - lower_bound_a1]+ b1[threadIdx.x - lower_bound_b1]; 
        } 
    }
    a[((blockIdx.x* blockDim.x) + threadIdx.x) + lower_bound_a1] = a1[threadIdx.x];
    b[((blockIdx.x* blockDim.x) + threadIdx.x) + lower_bound_b1] = b1[threadIdx.x];
    c[((blockIdx.x* blockDim.x) + threadIdx.x) + lower_bound_c1] = c1[threadIdx.x]; 
}


void checkCudaError(cudaError_t err, const char *msg)
{
  if (err != cudaSuccess)
  {
    std::cerr << "CUDA error: " << msg << ": " << cudaGetErrorString(err) << std::endl;
    exit(EXIT_FAILURE);
  }
}

int main()
{
  int N = 1 << 20; // Array size (e.g., 2^20 elements)

  // Allocate and initialize host vectors a, b, and c
  std::vector<float> a(N, 1.0f);
  std::vector<float> b(N, 2.0f);
  std::vector<float> c(N, 0.0f);

  // Allocate device memory for a, b, and c
  float *d_a;
  float *d_b;
  float *d_c;
  checkCudaError(cudaMalloc(&d_a, N * sizeof(float)), "allocating d_a");
  checkCudaError(cudaMalloc(&d_b, N * sizeof(float)), "allocating d_b");
  checkCudaError(cudaMalloc(&d_c, N * sizeof(float)), "allocating d_c");

  // Copy host memory to device
  checkCudaError(cudaMemcpy(d_a, a.data(), N * sizeof(float), cudaMemcpyHostToDevice), "copying a to d_a");
  checkCudaError(cudaMemcpy(d_b, b.data(), N * sizeof(float), cudaMemcpyHostToDevice), "copying b to d_b");

  // Configure the kernel launch parameters
  int blockSize = TILE_DIM;
  int gridSize = (N + blockSize - 1) / blockSize;

  // Measure kernel execution time
  cudaEvent_t start, stop;
  checkCudaError(cudaEventCreate(&start), "creating start event");
  checkCudaError(cudaEventCreate(&stop), "creating stop event");

  // Launch the kernel
  checkCudaError(cudaEventRecord(start), "recording start event");
  elementWiseAddition<<<gridSize, blockSize>>>(d_a, d_b, d_c, N);
  checkCudaError(cudaEventRecord(stop), "recording stop event");

  // Synchronize and check for errors
  checkCudaError(cudaEventSynchronize(stop), "synchronizing on stop event");
  checkCudaError(cudaGetLastError(), "launching elementWiseAddition kernel");

  // Calculate elapsed time
  float elapsedTime;
  checkCudaError(cudaEventElapsedTime(&elapsedTime, start, stop), "calculating elapsed time");

  // Copy the result from device to host
  checkCudaError(cudaMemcpy(c.data(), d_c, N * sizeof(float), cudaMemcpyDeviceToHost), "copying d_c to c");

  // Verify the result
  bool success = true;
  for (int i = 0; i < N; i++)
  {
    if (c[i] != a[i] + b[i])
    {
      success = false;
      break;
    }
  }

  if (success)
  {
    std::cout << "Element-wise addition succeeded!" << std::endl;
    std::cout << "Elapsed time: " << elapsedTime << " ms" << std::endl;
  }
  else
  {
    std::cerr << "Element-wise addition failed!" << std::endl;
  }

  // Clean up resources
  checkCudaError(cudaEventDestroy(start), "destroying start event");
  checkCudaError(cudaEventDestroy(stop), "destroying stop event");
  checkCudaError(cudaFree(d_a), "freeing d_a");
  checkCudaError(cudaFree(d_b), "freeing d_b");
  checkCudaError(cudaFree(d_c), "freeing d_c");

  return (success ? EXIT_SUCCESS : EXIT_FAILURE);
}