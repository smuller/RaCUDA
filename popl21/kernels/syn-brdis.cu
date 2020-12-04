__global__ brdis (int N, int M, int val) {
  __shared__ int A[M];
  __shared__ int B[M];
  for (int i = 0; i < N; ++i) {
    if ((threadIdx.x + i) % 2 == 0) {
      for (int j1 = 0; j1 < M; ++j1) {
        A[j1] = val;
      }
      int tot = 0;
      for (int j2 = 0; j2 < M; ++j2) {
        tot += A[j2];
        B[j2] = tot;
      }
      for (int j3 = 0; j3 < M; ++j3) {
        B[j3] += val;
      }
    } else {
      for (int j4 = 0; j4 < M; ++j4) {
        A[j4] = val * 2;
      }
      int tot = 0;
      for (int j5 = 0; j5 < M; ++j5) {
        tot += A[j5];
        B[j5] = tot;
      }
      for (int j6 = 0; j6 < M; ++j6) {
        B[j6] *= B[j6];
      }
    } 
  }
}
