#include <iostream>
#include <vector>
#include <chrono>
#include <cuda_runtime.h>
#include <device_launch_parameters.h>

#define TILE_DIM 32

__global__ void sumMatrixOnGPUMix(float *MatA, float *MatB, float *MatC, int nx,
                                  int ny)
{
    unsigned int nxthreads = gridDim.x * blockDim.x;
    unsigned int iy = blockIdx.y;
    unsigned int ix = threadIdx.x + blockIdx.x * blockDim.x;
    unsigned int ix2 = ix + nxthreads;

    unsigned int idx = iy * nx + ix;
    unsigned int idx2 = iy * nx + ix2;

    if (iy < ny)
    {
        if (ix < nx)
            MatC[idx] = MatA[idx] + MatB[idx];
        if (ix2 < nx)
            MatC[idx2] = MatA[idx2] + MatB[idx2];
    }
}

void checkCudaError(cudaError_t err, const char* msg) {
    if (err != cudaSuccess) {
        std::cerr << "CUDA error: " << msg << ": " << cudaGetErrorString(err) << std::endl;
        exit(EXIT_FAILURE);
    }
}

int main() {
    int nx = 1 << 13; // Matrix width (e.g., 2^10 columns)
    int ny = 1 << 13; // Matrix height (e.g., 2^10 rows)

    // Allocate and initialize host matrices A, B, and C
    std::vector<float> h_A(ny * nx, 1.0f);
    std::vector<float> h_B(ny * nx, 2.0f);
    std::vector<float> h_C(ny * nx, 0.0f);

    // Allocate device memory for A, B, and C
    float* d_A;
    float* d_B;
    float* d_C;
    checkCudaError(cudaMalloc(&d_A, ny * nx * sizeof(float)), "allocating d_A");
    checkCudaError(cudaMalloc(&d_B, ny * nx * sizeof(float)), "allocating d_B");
    checkCudaError(cudaMalloc(&d_C, ny * nx * sizeof(float)), "allocating d_C");

    // Copy host memory to device
    checkCudaError(cudaMemcpy(d_A, h_A.data(), ny * nx * sizeof(float), cudaMemcpyHostToDevice), "copying h_A to d_A");
    checkCudaError(cudaMemcpy(d_B, h_B.data(), ny * nx * sizeof(float), cudaMemcpyHostToDevice), "copying h_B to d_B");

    // Configure the kernel launch parameters
    dim3 blockSize(TILE_DIM, 1);
    dim3 gridSize((nx + blockSize.x - 1) / blockSize.x, ny);

    // Measure kernel execution time
    cudaEvent_t start, stop;
    checkCudaError(cudaEventCreate(&start), "creating start event");
    checkCudaError(cudaEventCreate(&stop), "creating stop event");

    // Launch the kernel
    
    checkCudaError(cudaEventRecord(start), "recording start event");
    sumMatrixOnGPUMix<<<gridSize, blockSize>>>(d_A, d_B, d_C, nx, ny);
    checkCudaError(cudaEventRecord(stop), "recording stop event");

    // Synchronize and check for errors
    checkCudaError(cudaEventSynchronize(stop), "synchronizing on stop event");
    checkCudaError(cudaGetLastError(), "launching sumMatrixOnGPUMix kernel");

    // Calculate elapsed time
    float elapsedTime;
    checkCudaError(cudaEventElapsedTime(&elapsedTime, start, stop), "calculating elapsed time");

    // Copy the result from device to host
    checkCudaError(cudaMemcpy(h_C.data(), d_C, ny * nx * sizeof(float), cudaMemcpyDeviceToHost), "copying d_C to h_C");

    // Clean up resources
    checkCudaError(cudaEventDestroy(start), "destroying start event");
    checkCudaError(cudaEventDestroy(stop), "destroying stop event");
    checkCudaError(cudaFree(d_A), "freeing d_A");
    checkCudaError(cudaFree(d_B), "freeing d_B");
    checkCudaError(cudaFree(d_C), "freeing d_C");

// Verify the result
bool success = true;
for (int i = 0; i < ny; i++) {
    for (int j = 0; j < nx; j++) {
        if (h_C[i * nx + j] != h_A[i * nx + j] + h_B[i * nx + j]) {
            success = false;
            break;
        }
    }
}

if (success) {
    std::cout << "Test PASSED" << std::endl;
} else {
    std::cout << "Test FAILED" << std::endl;
}

std::cout << "Kernel execution time: " << elapsedTime << " ms" << std::endl;

return 0;
}