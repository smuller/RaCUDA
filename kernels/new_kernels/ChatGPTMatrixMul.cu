__global__ void matrixMultiplication(float *A, float *B, float *C, int m, int n, int k) {
    // Calculate the row and column indices of the element to be computed
    int row = blockIdx.y * blockDim.y + threadIdx.y;
    int col = blockIdx.x * blockDim.x + threadIdx.x;

    // Allocate shared memory for the tiles of matrices A and B
    __shared__ float As[16][16];
    __shared__ float Bs[16][16];

    float Cvalue = 0.0;

    // Loop over the tiles of the input matrices
    for (int tile = 0; tile < (n-1)/16 + 1; tile++) {
        // Load the tiles of matrix A and B into shared memory
        if (row < m && tile*16+threadIdx.x < n) {
            As[threadIdx.y][threadIdx.x] = A[row*n + tile*16+threadIdx.x];
        } else {
            As[threadIdx.y][threadIdx.x] = 0.0;
        }
        if (col < k && tile*16+threadIdx.y < n) {
            Bs[threadIdx.y][threadIdx.x] = B[(tile*16+threadIdx.y)*k + col];
        } else {
            Bs[threadIdx.y][threadIdx.x] = 0.0;
        }
        __syncthreads();

        // Multiply the tiles of matrices A and B together
        for (int i = 0; i < 16; i++) {
            Cvalue += As[threadIdx.y][i] * Bs[i][threadIdx.x];
        }
        __syncthreads();
    }

        // Write the computed element to the output matrix
    if (row < m && col < k) {
        C[row*k + col] = Cvalue;
    }
}
