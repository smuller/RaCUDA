__global__ void elementwise_pow(const float *d_input, float scalar, float *d_output, int array_size) {
    int index = blockIdx.x * blockDim.x + threadIdx.x;

    // Issue 1: No check for out-of-bounds access
    float value = d_input[index];

    // Issue 2: Using a loop for exponentiation instead of the pow function
    float result = 1;
    for (int i = 0; i < scalar; ++i) {
        result *= value;
    }

    // Issue 3: Global memory access instead of shared memory
    d_output[index] = result;
}
