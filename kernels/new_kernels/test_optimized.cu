__global__ void optimized_example_kernel(float *A, float *B, float *C, int n, int m) {
  int row = blockIdx.y * blockDim.y + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;

  // Allocate shared memory for A and B
  __shared__ float sA[16][16];
  __shared__ float sB[16][16];

  float sum = 0;

  // Loop over tiles
  for (int t = 0; t < (m - 1) / 16 + 1; ++t) {
    // Load data into shared memory
    sA[threadIdx.y][threadIdx.x] = A[row * m + t * 16 + threadIdx.x];
    sB[threadIdx.y][threadIdx.x] = B[(t * 16 + threadIdx.y) * m + col];


    __syncthreads();

    // Compute sum
    for (int i = 0; i < 16; ++i) {
      float a = sA[threadIdx.y][i];
      float b = sB[i][threadIdx.x];

      if (a < 0 && b < 0) {
        sum += a * b * 2;
      } else if (a >= 0 && b < 0) {
        sum += a * b * 0.5;
      } else {
        sum += a * b;
      }
    }

    __syncthreads();
  }

  // Write the result to global memory
  if (row < n && col < m) {
    C[row * m + col] = sum;
  }
}