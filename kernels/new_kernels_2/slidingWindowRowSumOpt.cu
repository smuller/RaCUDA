__global__ void slidingWindowRowSum_opt(float* A, float* B, int rows, int cols, int window_size) {
    // Allocate shared memory for A
    __shared__ float shared_A[BLOCKSIZE][BLOCKSIZE * WINDOW_MULTIPLIER];

    int row = blockIdx.y * blockDim.y + threadIdx.y;
    int col = blockIdx.x * blockDim.x + threadIdx.x;

    // Load A values into shared memory
    for (int i = 0; i < WINDOW_MULTIPLIER; i++) {
        int shared_A_col = threadIdx.x * WINDOW_MULTIPLIER + i;
        if (row < rows && shared_A_col < cols) {
            shared_A[threadIdx.y][shared_A_col] = A[row * cols + shared_A_col];
        }
    }
    __syncthreads();

    // Perform the sum of elements within a sliding window using shared memory
    if (row < rows && col < cols - window_size + 1) {
        float sum = 0.0;
        for (int i = 0; i < window_size; i++) {
            sum += shared_A[threadIdx.y][threadIdx.x * WINDOW_MULTIPLIER + i];
        }
        B[row * (cols - window_size + 1) + col] = sum;
    }
}
