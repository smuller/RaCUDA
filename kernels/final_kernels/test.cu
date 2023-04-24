__global__ void matrixNormOptimized(float *temp_a, float *temp_b, int N) {
    int col = blockIdx.x * blockDim.x + threadIdx.x;
    int tx = threadIdx.x;

    // Allocate shared memory for storing a column of the input matrix
    __shared__ float shared_col[256]; // Assuming blockDim.x is 256; adjust accordingly

    if (col < N) {
        int row;
        float mu, sigma;

        // Load the column into shared memory
        for (row = 0; row < N; row++) {
            shared_col[row] = temp_a[row * N + col];
        }
        __syncthreads(); // Synchronize threads to make sure the column is loaded

        // Compute mean
        mu = 0.0;
        for (row = 0; row < N; row++) {
            mu += shared_col[row];
        }
        mu /= (float)N;

        // Compute standard deviation
        sigma = 0.0;
        for (row = 0; row < N; row++) {
            sigma += powf(shared_col[row] - mu, 2.0);
        }
        sigma /= (float)N;
        sigma = sqrt(sigma);

        // Normalize the column and store the result in temp_b
        for (row = 0; row < N; row++) {
            if (sigma == 0.0) {
                temp_b[row * N + col] = 0.0;
            } else {
                temp_b[row * N + col] = (shared_col[row] - mu) / sigma;
            }
        }
    }
}
