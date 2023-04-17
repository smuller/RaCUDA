__global__ void sumMatrixOnGPU1D(float *MatA, float *MatB, float *MatC, int nx, int ny)
{

    signed long int lower_bound_MatA1;
    lower_bound_MatA1 = threadIdx.x + (blockIdx.x * blockDim.x);
    __shared__ float MatA1[TILE_DIM];
    MatA1[threadIdx.x] = MatA[((blockIdx.x *blockDim.x) +threadIdx.x) +lower_bound_MatA1];
    signed long int lower_bound_MatB1;
    lower_bound_MatB1 = threadIdx.x + (blockIdx.x * blockDim.x);
    __shared__ float MatB1[TILE_DIM];
    MatB1[threadIdx.x] = MatB[((blockIdx.x *blockDim.x) +threadIdx.x) +lower_bound_MatB1];
    signed long int lower_bound_MatC1;
    lower_bound_MatC1 = threadIdx.x + (blockIdx.x * blockDim.x);
    __shared__ float MatC1[TILE_DIM];
    MatC1[threadIdx.x] = MatC[((blockIdx.x *blockDim.x) +threadIdx.x) + lower_bound_MatC1];
    unsigned int ix;
    ix = threadIdx.x + (blockIdx.x * blockDim.x);
    if (ix < nx)
    {

        int iy;
        for (iy = 0; iy < ny; iy = iy + 1)
        {

            int idx;
            idx = (iy * nx) + ix;
            MatC1[idx - lower_bound_MatC1] = MatA1[idx -lower_bound_MatA1] +MatB1[idx -lower_bound_MatB1];
        }
    }
    MatA1[((blockIdx.x * blockDim.x) + threadIdx.x) + lower_bound_MatA1] = MatA[(blockIdx.x * blockDim.x) + threadIdx.x];
    MatB1[((blockIdx.x * blockDim.x) + threadIdx.x) + lower_bound_MatB1] = MatB[(blockIdx.x * blockDim.x) + threadIdx.x];
    MatC1[((blockIdx.x * blockDim.x) + threadIdx.x) + lower_bound_MatC1] = MatC[(blockIdx.x * blockDim.x) + threadIdx.x];
}
