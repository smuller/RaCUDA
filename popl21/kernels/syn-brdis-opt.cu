__global__ int brdisOpt (int N, int M, int val) {
  __shared__ int A[M];
  __shared__ int B[M];
  for (int i = 0; i < N; ++i) {
    if ((threadIdx.x + i) % 2 == 0) {
      for (int j1 = 0; j1 < M; ++j1) {
        A[j1] = val;
      }
    } else {
      for (int j2 = 0; j2 < M; ++j2) {
        A[j2] = val * 2;
      }
    }

    int tot = 0;
    for (int j3 = 0; j3 < M; ++j3) {
      tot += A[j3];
      B[j3] = tot;
    }

    if ((threadIdx.x + i) % 2 == 0) {
      for (int j4 = 0; j4 < M; ++j4) {
        B[j4] += val;
      }
    } else {
      for (int j5 = 0; j5 < M; ++j5) {
        B[j5] *= B[j5];
      }
    }
  }
}
