#define BLOCKSIZE 256
#define WINDOW_MULTIPLIER 10

__global__ void pairwiseProductWithWindow_opt(float* A, float* B, float* C, int input_size, int window_size) {
    // Allocate shared memory for A and B
    __shared__ float shared_A[BLOCKSIZE];
    __shared__ float shared_B[BLOCKSIZE * WINDOW_MULTIPLIER];

    int idx = blockIdx.x * blockDim.x + threadIdx.x;

    // Load A values into shared memory
    if (idx < input_size) {
        shared_A[threadIdx.x] = A[idx];
    }

    // Load B values into shared memory
    for (int i = 0; i < WINDOW_MULTIPLIER; i++) {
        int shared_B_idx = threadIdx.x * WINDOW_MULTIPLIER + i;
        if (shared_B_idx < window_size) {
            shared_B[shared_B_idx] = B[shared_B_idx];
        }
    }
    __syncthreads();

    // Perform the pairwise product using shared memory
    if (idx < input_size) {
        for (int i = 0; i < window_size; i++) {
            C[idx * window_size + i] = shared_A[threadIdx.x] * shared_B[i];
        }
    }
}
