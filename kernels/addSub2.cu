__global__ void addSubArray2 (int *A, int *B, int w, int h) {
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  for (int j = 0; j < h; j += 2) {
    B[j * w + i] += A[i];
    B[(j + 1) * w + i] -= A[i];
  }
}
