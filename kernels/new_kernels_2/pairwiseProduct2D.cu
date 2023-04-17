__global__ void pairwiseProductWithWindow2D(float* A, float* B, float* C, int rows, int cols, int window_size) {
    int row = blockIdx.y * blockDim.y + threadIdx.y;
    int col = blockIdx.x * blockDim.x + threadIdx.x;

    if (row < rows && col < cols) {
        for (int i = 0; i < window_size; i++) {
            C[row * cols * window_size + col * window_size + i] = A[row * cols + col] * B[i];
        }
    }
}