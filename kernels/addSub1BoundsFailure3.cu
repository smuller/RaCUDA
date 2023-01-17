__global__ void addSubArray1 (int *A, int *B, int w, int h) {

  //if (h > 4) {
    for (int i = 0; i < w; i++) {
      int j = blockIdx.x * blockDim.x + threadIdx.x;
      B[2 * j * w + i] += A[i];
      B[(2 * j + 1) * w + i] -= A[i];
    }
  //} else {
    for (int i = 0; i < 5; i++) {
      //int k = 2 + 1 - 3 * 5 + w;
      //int j = 9 - 3 + k * 2;
      //A[i] = 1;
      //B[i] = 1;
      B[i] += A[i];
      B[i] -= A[i];
    }
  //}
  //B[w] = 0;
}
