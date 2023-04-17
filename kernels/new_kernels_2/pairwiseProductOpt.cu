#define BLOCKSIZE 256

__global__ void pairwiseProduct_opt(float* A, float* B, float* C, int input_size) {
    // Allocate shared memory for A and B
    __shared__ float shared_A[BLOCKSIZE];
    __shared__ float shared_B[BLOCKSIZE];

    int idx = blockIdx.x * blockDim.x + threadIdx.x;

    // Load A and B values into shared memory
    if (idx < input_size) {
        shared_A[threadIdx.x] = A[idx];
        shared_B[threadIdx.x] = B[idx];
    }
    __syncthreads();

    // Perform the pairwise product using shared memory
    if (idx < input_size) {
        C[idx] = shared_A[threadIdx.x] * shared_B[threadIdx.x];
    }
}