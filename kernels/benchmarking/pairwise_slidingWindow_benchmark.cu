#include <cuda_runtime.h>
#include <iostream>
#include <chrono>

#define BLOCKSIZE 256

// Error checking macro
#define gpuErrchk(ans)                        \
    {                                         \
        gpuAssert((ans), __FILE__, __LINE__); \
    }


__global__ void pairwiseProductWithWindow_Opt ( float *A,  float *B,  float *C,  int input_size,  int window_size) {
    signed long int array_idx_A1;
    signed long int lower_bound_A1;
    lower_bound_A1 = blockIdx.x * blockDim.x;
    array_idx_A1 = ((blockIdx.x * blockDim.x) + threadIdx.x) - lower_bound_A1;
    __shared__ float A1[BLOCKSIZE];
    A1[threadIdx.x] = A[(blockIdx.x * blockDim.x) + threadIdx.x];
    __syncthreads();
    int idx;
    idx = (blockIdx.x * blockDim.x) + threadIdx.x;
    if (idx < input_size) {

        int i;
        for (i = 0; i < window_size; i = i + 1) {

            C[(idx * window_size) + i] = A1[array_idx_A1] * B[i];
        }
    } else {


    }
}

__global__ void pairwiseProductWithWindow(float* A, float* B, float* C, int input_size, int window_size) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;

    if (idx < input_size) {
        for (int i = 0; i < window_size; i++) {
            C[idx * window_size + i] = A[idx] * B[i];
        }
    }
}


void init_data_float(float *data, int size) {
    for (int i = 0; i < size; i++) {
        data[i] = static_cast<float>(rand()) / static_cast<float>(RAND_MAX);
    }
}

inline void gpuAssert(cudaError_t code, const char *file, int line, bool abort = true)
{
    if (code != cudaSuccess)
    {
        fprintf(stderr, "GPUassert: %s %s %d\n", cudaGetErrorString(code), file, line);
        if (abort)
            exit(code);
    }
}

int main()
{
    int input_size = 4096;
    int window_size = 256;
    const int blockSize = 256;
    const int gridSize = (input_size + blockSize - 1) / blockSize;
    const int A_size = input_size;
    const int B_size = window_size;
    const int C_size = input_size * window_size;
    size_t bytes_A = A_size * sizeof(float);
    size_t bytes_B = B_size * sizeof(float);
    size_t bytes_C = C_size * sizeof(float);

    float *h_A = (float *)malloc(bytes_A);
    float *h_B = (float *)malloc(bytes_B);
    float *h_C = (float *)malloc(bytes_C);

    init_data_float(h_A, A_size);
    init_data_float(h_B, B_size);

    float *d_A, *d_B, *d_C;
    gpuErrchk(cudaMalloc((void **)&d_A, bytes_A));
    gpuErrchk(cudaMalloc((void **)&d_B, bytes_B));
    gpuErrchk(cudaMalloc((void **)&d_C, bytes_C));

    gpuErrchk(cudaMemcpy(d_A, h_A, bytes_A, cudaMemcpyHostToDevice));
    gpuErrchk(cudaMemcpy(d_B, h_B, bytes_B, cudaMemcpyHostToDevice));

    auto start_orig = std::chrono::high_resolution_clock::now();

    for (int i = 0; i < 10000; i++)
    {
        pairwiseProductWithWindow<<<gridSize, blockSize>>>(d_A, d_B, d_C, input_size, window_size);
        gpuErrchk(cudaPeekAtLastError());
        gpuErrchk(cudaDeviceSynchronize());
    }

    auto end_orig = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed_orig = end_orig - start_orig;

    auto start_opt = std::chrono::high_resolution_clock::now();

    for (int i = 0; i < 10000; i++)
    {
        pairwiseProductWithWindow_Opt<<<gridSize, blockSize>>>(d_A, d_B, d_C, input_size, window_size);
        gpuErrchk(cudaPeekAtLastError());
        gpuErrchk(cudaDeviceSynchronize());
    }

    auto end_opt = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed_opt = end_opt - start_opt;

    gpuErrchk(cudaMemcpy(h_C, d_C, bytes_C, cudaMemcpyDeviceToHost));

    std::cout << "Elapsed time (Original kernel): " << elapsed_orig.count() << " seconds" << std::endl;
    std::cout << "Elapsed time (Optimized kernel): " << elapsed_opt.count() << " seconds" << std::endl;

    cudaFree(d_A);
    cudaFree(d_B);
    cudaFree(d_C);
    free(h_A);
    free(h_B);
    free(h_C);

    return 0;
}