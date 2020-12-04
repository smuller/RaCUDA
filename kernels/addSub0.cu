__global__ void addSubArray0 (int *A, int *B, int w, int h) {
  for (int i = 0; i < w; i++) {
    int j = blockIdx.x * blockDim.x + threadIdx.x;
    if (j % 2 == 0) {
      B[j * w + i] += A[i];
    } else {
      B[j * w + i] -= A[i];
    }
  }
}