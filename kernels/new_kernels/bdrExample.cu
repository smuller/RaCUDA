__global__ void adjustIncrement(unsigned int* d, unsigned int* incr, int input_size){
	int pos = blockIdx.x * blockDim.x*2 + threadIdx.x * 2 + 1;
	if (pos< input_size)
	{
		d[pos] += incr[blockIdx.x];
		d[pos-1] += incr[blockIdx.x];
	}
	else
	{
		d[pos-1] += incr[blockIdx.x];
	}
}