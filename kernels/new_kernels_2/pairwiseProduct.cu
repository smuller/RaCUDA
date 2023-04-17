__global__ void pairwiseProduct(float* A, float* B, float* C, int input_size) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;

    if (idx < input_size) {
        C[idx] = A[idx] * B[idx];
    }
}

