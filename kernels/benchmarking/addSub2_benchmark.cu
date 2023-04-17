#include <cuda_runtime.h>
#include <iostream>
#include <chrono>

#define BLOCKSIZE 256

// Error checking macro
#define gpuErrchk(ans)                        \
    {                                         \
        gpuAssert((ans), __FILE__, __LINE__); \
    }


__global__ void addSubArray2_Opt ( int *A,  int *B,  int w,  int h) {

    signed long int array_idx_A1;
    signed long int lower_bound_A1;
    lower_bound_A1 = blockIdx.x * blockDim.x;
    array_idx_A1 = ((blockIdx.x * blockDim.x) + threadIdx.x) - lower_bound_A1;
    __shared__ int A1[BLOCKSIZE];
    A1[threadIdx.x] = A[(blockIdx.x * blockDim.x) + threadIdx.x];
    __syncthreads();
    int i;
    i = (blockIdx.x * blockDim.x) + threadIdx.x;
    int j;
    for (j = 0; j < h; j = j + 2) {

        B[(j * w) + i] = B[(j * w) + i] + A1[array_idx_A1];
        B[((j + 1) * w) + i] = B[((j + 1) * w) + i] - A1[array_idx_A1];
    }
}


__global__ void addSubArray2 (int *A, int *B, int w, int h) {
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  for (int j = 0; j < h; j += 2) {
    B[j * w + i] += A[i];
    B[(j + 1) * w + i] -= A[i];
  }
}

// Add any necessary helper functions, such as init_data() and gpuErrchk()...
void init_data(int *data, int size)
{
    for (int i = 0; i < size; i++)
    {
        data[i] = rand() % 100;
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
    int w = 4096; // Increase the input width
    int h = 4096; // Increase the input height
    const int blockSize = 256;
    const int gridSize = (h + blockSize - 1) / blockSize;
    const int A_size = w;
    const int B_size = w * 2 * h;
    size_t bytes_A = A_size * sizeof(int);
    size_t bytes_B = B_size * sizeof(int);

    int *h_A = (int *)malloc(bytes_A);
    int *h_B = (int *)malloc(bytes_B);

    init_data(h_A, A_size);
    init_data(h_B, B_size);

    int *d_A, *d_B;
    gpuErrchk(cudaMalloc((void **)&d_A, bytes_A));
    gpuErrchk(cudaMalloc((void **)&d_B, bytes_B));

    gpuErrchk(cudaMemcpy(d_A, h_A, bytes_A, cudaMemcpyHostToDevice));
    gpuErrchk(cudaMemcpy(d_B, h_B, bytes_B, cudaMemcpyHostToDevice));

    auto start_opt = std::chrono::high_resolution_clock::now();

    for (int i = 0; i < 10000; i++)
    {
        addSubArray2_Opt<<<gridSize, blockSize>>>(d_A, d_B, w, h);
        gpuErrchk(cudaPeekAtLastError());
        gpuErrchk(cudaDeviceSynchronize());
    }

    auto end_opt = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed_opt = end_opt - start_opt;

    auto start_orig = std::chrono::high_resolution_clock::now();

    for (int i = 0; i < 10000; i++)
    {
        addSubArray2<<<gridSize, blockSize>>>(d_A, d_B, w, h);
        gpuErrchk(cudaPeekAtLastError());
        gpuErrchk(cudaDeviceSynchronize());
    }

    auto end_orig = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed_orig = end_orig - start_orig;

    gpuErrchk(cudaMemcpy(h_B, d_B, bytes_B, cudaMemcpyDeviceToHost));

    std::cout << "Elapsed time (Optimized kernel): " << elapsed_opt.count() << " seconds" << std::endl;
    std::cout << "Elapsed time (Original kernel): " << elapsed_orig.count() << " seconds" << std::endl;

    cudaFree(d_A);
    cudaFree(d_B);
    free(h_A);
    free(h_B);

    return 0;
}