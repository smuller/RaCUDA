__global__ void addSubArray1 (int *A, int *B, int w, int h) {
  for (int i = 0; i < w; i++) {
    if (i == 3) {
    int j = blockIdx.x * blockDim.x + threadIdx.x;
      B[2 * j * w + i] += A[i];
      B[(2 * j + 1) * w + i] -= A[i];
    } else {
      int j = blockIdx.x * blockDim.x + threadIdx.x;
      B[2 * j * w + i] += A[i];
      B[(2 * j + 1) * w + i] -= A[i];
    }
    
  }
}