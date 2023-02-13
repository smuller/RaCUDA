__global__ brdis (int N, int M, int val) {
  __shared__ int A[M];
  __shared__ int B[M];
  for (int i = 0; i < N; ++i) {
    if ((threadIdx.x + i) % 2 == 0) {
      A[i] += 1;
      B[i] += 1;
      //A[i] += 1;
    } else {
      A[i] -= 1;
      B[i] += 1;
      //A[i] -= 1;
    }
  }
}
