__global__ brdis (int N, int M, int val) {
  __shared__ int A[M];
  __shared__ int B[M];
  for (int i = 0; i < N; ++i) {
    if ((threadIdx.x + i) % 2 == 0) {
      for (int j = 0; j < M; ++j) {
        A[j] = val;
      }
      int tot = 0;
      for (int j = 0; j < M; ++j) {
        tot += A[j];
        B[j] = tot;
      }
      for (int j = 0; j < M; ++j) {
        B[j] += val;
      }
    } else {
      for (int j = 0; j < M; ++j) {
        A[j] = val * 2;
      }
      int tot = 0;
      for (int j = 0; j < M; ++j) {
        tot += A[j];
        B[j] = tot;
      }
      for (int j = 0; j < M; ++j) {
        B[j] *= B[j];
      }
    } 
  }
}
