__global__ void sumCommSingleBlock(const int *a, int *out, int w) {

    static const int arraySize = 10000;
    static const int blockSize = 1024;

    int idx = threadIdx.x;
    int sum = 0;
    for (int i = 0; i < arraySize ; i += blockSize)
        sum += a[threadIdx.x];
    __shared__ int r[blockSize];
    r[idx] = sum;
    __syncthreads();
    for (int size = 1; size<blockDim.x; size*=2) { //uniform
        if ((idx % (2*size)) == 0)
            r[idx] += r[idx+size];
        __syncthreads();
    }
    if (idx == 0)
        *out = r[0];
}
