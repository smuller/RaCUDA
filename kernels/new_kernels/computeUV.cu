__global__ void computeUV(int height, int width, float *uvxArr, float *uvyArr)
{
    const int x = blockIdx.x * blockDim.x + threadIdx.x;
    const int y = blockIdx.y * blockDim.y + threadIdx.y;

    float widthFloat = width;
    float heightFloat = height;

    float uvx = (tanf(3.14159265 / 4.0)) * (2.0*x - widthFloat) /  widthFloat;
    float uvy = (tanf(3.14159265 / 4.0)) * ( heightFloat /  widthFloat) * (2.0*y- heightFloat) /  heightFloat;
    uvxArr[(y*width)+x] = uvx;
    uvyArr[(y*width)+x] = uvy;
}