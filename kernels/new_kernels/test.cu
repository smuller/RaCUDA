__global__ void example_kernel(float *A, float *B, float *C, int n, int m) {
    int row = blockIdx.y * blockDim.y + threadIdx.y;
    int col = blockIdx.x * blockDim.x + threadIdx.x;

    if (row < n && col < m) {
        float sum = 0;

        // Flaw 1: Accessing global memory directly in a loop
        for (int i = 0; i < m; ++i) {
            float a = A[row * m + i];
            float b = B[i * m + col];

            // Flaw 2: Branching based on input values
            if (a < 0 && b < 0) {
                sum += a * b * 2;
            } else if (a >= 0 && b < 0) {
                sum += a * b * 0.5;
            } else {
                sum += a * b;
            }
        }

        // Flaw 3: Writing to global memory directly
        C[row * m + col] = sum;
    }
}