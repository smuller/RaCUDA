__global__ addSubArray1 ( int *A,  int *B,  int w,  int h) {

    signed long int size_A1;
    signed long int lower_bound_A1;
    upper_bound_A1 = w + -1;
    lower_bound_A1 = 0;
    size_A1 = upper_bound_A1 - lower_bound_A1;
    __shared__ int A1[size_A1];
    for (signed long int __itertemp =  0; __itertemp <= size_A1; __itertemp = __itertemp + blockSize.x) {

        A1[__itertemp] = A[__itertemp + lower_bound_A1];
    }
    signed long int size_B1;
    signed long int lower_bound_B1;
    upper_bound_B1 = (w * 2)
                     + (((w * threadIdx.x) * 2)
                        + (((w * (blockIdx.x * blockDim.x)) * 2) + -1));
    lower_bound_B1 = w
                     + (((w * threadIdx.x) * 2)
                        + ((w * (blockIdx.x * blockDim.x)) * 2));
    size_B1 = upper_bound_B1 - lower_bound_B1;
    __shared__ int B1[size_B1];
    for (signed long int __itertemp =  0; __itertemp <= size_B1; __itertemp = __itertemp + blockSize.x) {

        B1[__itertemp] = B[__itertemp + lower_bound_B1];
    }
    signed long int size_B2;
    signed long int lower_bound_B2;
    upper_bound_B2 = w
                     + (((w * threadIdx.x) * 2)
                        + (((w * (blockIdx.x * blockDim.x)) * 2) + -1));
    lower_bound_B2 = ((w * threadIdx.x) * 2)
                     + ((w * (blockIdx.x * blockDim.x)) * 2);
    size_B2 = upper_bound_B2 - lower_bound_B2;
    __shared__ int B2[size_B2];
    for (signed long int __itertemp =  0; __itertemp <= size_B2; __itertemp = __itertemp + blockSize.x) {

        B2[__itertemp] = B[__itertemp + lower_bound_B2];
    }

    for (int i=0; i < w; i = i + 1) {

        int j;
        j = (blockIdx.x * blockDim.x) + threadIdx.x;
        B2[(((2 * j) * w) + i) - lower_bound_B2] = B2[(((2 * j) * w) + i) - lower_bound_B2] + A1[i - lower_bound_A1];
        B1[((((2 * j) + 1) * w) + i) - lower_bound_B1] = B1[((((2 * j) + 1) * w)
                                                             + i)
                                                            - lower_bound_B1]
                                                           - A1
                                                             [i
                                                              - lower_bound_A1];
    }
    for (signed long int __itertemp =  0; __itertemp <= size_A1; __itertemp = __itertemp + blockSize.x) {

        A[__itertemp + lower_bound_A1] = A1[__itertemp];
    }
    for (signed long int __itertemp =  0; __itertemp <= size_B1; __itertemp = __itertemp + blockSize.x) {

        B[__itertemp + lower_bound_B1] = B1[__itertemp];
    }
    for (signed long int __itertemp =  0; __itertemp <= size_B2; __itertemp = __itertemp + blockSize.x) {

        B[__itertemp + lower_bound_B2] = B2[__itertemp];
    }
}