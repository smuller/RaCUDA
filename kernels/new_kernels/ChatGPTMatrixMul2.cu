// Kernel function to perform matrix multiplication on the device
__global__ void matrixMultiplication(float *A, float *B, float *C, int m, int n, int k) {
    // Calculate the row and column indices of the element to be computed
    int row = blockIdx.y * blockDim.y + threadIdx.y;
    int col = blockIdx.x * blockDim.x + threadIdx.x;

    float Cvalue = 0.0;

    // Loop over the elements of the input matrices
    for (int i = 0; i < n; i++) {
        if (row < m && i < n && col < k) {
            Cvalue += A[row*n + i] * B[i*k + col];
        }
    }

    // Write the computed element to the output matrix
    if (row < m && col < k) {
        C[row*k + col] = Cvalue;
    }
}