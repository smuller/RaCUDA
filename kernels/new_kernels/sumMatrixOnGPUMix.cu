__global__ void sumMatrixOnGPUMix(float *MatA, float *MatB, float *MatC, int nx,
                                  int ny)
{
    unsigned int nxthreads = gridDim.x * blockDim.x;
    unsigned int iy = blockIdx.y;
    unsigned int ix = threadIdx.x + blockIdx.x * blockDim.x;
    unsigned int ix2 = ix + nxthreads;

    unsigned int idx = iy * nx + ix;
    unsigned int idx2 = iy * nx + ix2;

    if (iy < ny)
    {
        if (ix < nx)
            MatC[idx] = MatA[idx] + MatB[idx];
        if (ix2 < nx)
            MatC[idx2] = MatA[idx2] + MatB[idx2];
    }
}