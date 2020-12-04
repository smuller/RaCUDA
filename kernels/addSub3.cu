__global__ void addSubArray3 (int *A, int *B, int w, int h) {
  __shared__ int As[blockDim.x];
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  As[threadIdx.x] = A[i];
  for (int j = 0; j < h; j += 2) {
    B[j * w + i] += As[i];
    B[(j + 1) * w + i] -= As[i];
  }
}
