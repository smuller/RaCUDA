// https://github.com/Nten0/CUDA---Principal-Direction-Divisive-Partitioning/blob/master/part1.cu

__global__ void pddp1(double* objects, int m, int n, double *w,double *x,double *tmp)
{
	int row = blockIdx.y * blockDim.y + threadIdx.y;
	if (row < n ) {

	double Cvalue = 0;
	
	for(int i = 0; i < m; i++) {
		Cvalue += (objects[row * m + i] - w[row]) * x [i];
    }
	tmp[row] = Cvalue;

	__syncthreads();
    };
}
