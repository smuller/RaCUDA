__global__ void convolution(float *x, float *h, float *y, int N, int M) {
    // Calculate the index of the element to be computed
    int idx = threadIdx.x;

    float yvalue = 0.0;

    // Loop over the elements of the filter
    for (int i = 0; i < M; i++) {
        if (idx-i >= 0 && idx-i < N) {
            yvalue += x[idx-i] * h[i];
        }
    }

    // Write the computed element to the output signal
    y[idx] = yvalue;
}