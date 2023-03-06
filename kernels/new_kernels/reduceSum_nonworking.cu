__global__ void ReduceSumWarpShuffle(int *input, int *output, int size, int width) {
    int tid = threadIdx.x;
    int block_offset = blockIdx.x * blockDim.x * 2;
    int index = block_offset + tid;

    int *i_data = input + block_offset;

    if ((index + blockDim.x) < size) {
        input[index] += input[index + blockDim.x];
    }
    __syncthreads();

    // Loop unrolling
    for (int offset = blockDim.x / 2; offset >= 32; offset = offset / 2) {
        if (tid < offset) {
            i_data[tid] += i_data[tid + offset];
        }
        __syncthreads();
    }

    int sum = i_data[tid];
    // Warp unrolling
    if (tid < 32) {
        sum += __shfl_down_sync(MASK, sum, 16, width);
        sum += __shfl_down_sync(MASK, sum, 8, width);
        sum += __shfl_down_sync(MASK, sum, 4, width);
        sum += __shfl_down_sync(MASK, sum, 2, width);
        sum += __shfl_down_sync(MASK, sum, 1, width);
    }

    if (tid == 0) {
        output[blockIdx.x] = sum;
    }
}