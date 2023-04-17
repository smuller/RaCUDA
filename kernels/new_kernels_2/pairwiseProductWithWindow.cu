__global__ void pairwiseProductWithWindow(float* A, float* B, float* C, int input_size, int window_size) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;

    if (idx < input_size) {
        for (int i = 0; i < window_size; i++) {
            C[idx * window_size + i] = A[idx] * B[i];
        }
    }
}