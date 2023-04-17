__global__ void pairwiseProductWithWindow2D_opt(float* A, float* B, float* C, int rows, int cols, int window_size) {
    // Allocate shared memory for A and B
    __shared__ float shared_A[BLOCKSIZE][BLOCKSIZE];
    __shared__ float shared_B[BLOCKSIZE * WINDOW_MULTIPLIER];

    int row = blockIdx.y * blockDim.y + threadIdx.y;
    int col = blockIdx.x * blockDim.x + threadIdx.x;

    // Load A values into shared memory
    if (row < rows && col < cols) {
        shared_A[threadIdx.y][threadIdx.x] = A[row * cols + col];
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
    if (row < rows && col < cols) {
        for (int i = 0; i < window_size; i++) {
            C[row * cols * window_size + col * window_size + i] = shared_A[threadIdx.y][threadIdx.x] * shared_B[i];
        }
    }
}
