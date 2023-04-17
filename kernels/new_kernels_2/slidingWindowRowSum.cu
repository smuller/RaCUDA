__global__ void slidingWindowRowSum(float* A, float* B, int rows, int cols, int window_size) {
    int row = blockIdx.y * blockDim.y + threadIdx.y;
    int col = blockIdx.x * blockDim.x + threadIdx.x;

    if (row < rows && col < cols - window_size + 1) {
        float sum = 0.0;
        for (int i = 0; i < window_size; i++) {
            sum += A[row * cols + col + i];
        }
        B[row * (cols - window_size + 1) + col] = sum;
    }
}
