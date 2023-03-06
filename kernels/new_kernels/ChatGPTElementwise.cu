__global__ void elementWiseAddition(float *a, float *b, float *c, int N) {
    // Loop over the elements of the vectors
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            c[i] = a[i] + b[i];
        }
    }
}
