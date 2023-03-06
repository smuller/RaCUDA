__global__ void cdf(unsigned int *in, float *out, int size1, int size2){
  //loading
  __shared__ float blockSum[HISTOGRAM_LENGTH];
  unsigned int tx = threadIdx.x;
  unsigned int start = 2 * blockIdx.x * blockDim.x;
  if (start + 2*tx < size1)
    blockSum[2*tx] = in[start + 2*tx]/((float) size2);
  else
    blockSum[2*tx] = 0.;
    
  if (start + 2*tx + 1< size1)
    blockSum[2*tx + 1] = in[start + 2*tx + 1]/((float) size2);
  else
    blockSum[2*tx + 1] = 0.;  
  
  //reduction
  int stride = 1;
  while (stride < 2*blockDim.x){
  __syncthreads();
  int index = (tx + 1)*stride*2 - 1;
  if (index < 2*blockDim.x && (index - stride) >= 0){
    blockSum[index] += blockSum[index - stride];
  }
  stride *= 2;
  }
 //post scan
 stride = blockDim.x / 2;
 while (stride > 0){
   __syncthreads();
   int index = (tx + 1)*stride*2 - 1;
   if (index + stride < 2 * blockDim.x){
     blockSum[index + stride] += blockSum[index];
   }
   stride /= 2;
 }
 __syncthreads();
 //output
 if (start + 2*tx < size1){
   out[start + 2*tx] = blockSum[2 * tx];
 }
 if (start + 2*tx + 1< size1){
   out[start + 2*tx + 1] = blockSum[2 * tx + 1];
 }
}