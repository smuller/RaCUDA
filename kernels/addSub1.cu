__global__ void addSubArray1 (int *A, int *B, int w, int h) {
  __syncthreads();
  int i = 0;
  while (i < w) {
    int j = blockIdx.x * blockDim.x + threadIdx.x;
    B[2 * j * w + i] += A[i];
    B[(2 * j + 1) * w + i] -= A[i];
    i = i+1;
  }
}