__global__ void addSubArray1 (int *A, int *B, int w, int h) {


   signed long int size_A1;
    signed long int lower_bound_A1;
    signed long int upper_bound_A1;
    upper_bound_A1 = w + -1;
    lower_bound_A1 = 0;
    size_A1 = upper_bound_A1 - lower_bound_A1;
    __shared__ int A1[size_A1];
    for (signed long int __itertemp = 0; __itertemp <= size_A1; __itertemp = __itertemp + blockSize.x) {

        A1[__itertemp] = A[__itertemp];
    }

  for (int i = 0; i < w; i++) {
    int j = blockIdx.x * blockDim.x + threadIdx.x;
    B[2 * j * w + i] += A[i];
    B[(2 * j + 1) * w + i] -= A[i];
  }

  for (signed long int __itertemp =  0; __itertemp <= size_A1; __itertemp = __itertemp
                                                                    + blockSize.x) {

        A[__itertemp] = A1[__itertemp];
    }
}