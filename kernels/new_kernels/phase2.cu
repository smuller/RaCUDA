// https://github.com/Nten0/CUDA---Principal-Direction-Divisive-Partitioning/blob/master/part1.cu

__global__ void Phase2(int *dist, int Round, int n) {
    __shared__ int both[64][64];
    __shared__ int row_blocks[64][64];
    __shared__ int col_blocks[64][64];

    const int i = threadIdx.y;
    const int j = threadIdx.x;
    const int offset = 64 * Round;
    const int block_i = blockIdx.x;


    row_blocks[i   ][j   ] = dist[block_i*64*n + offset + i*n + j];
    row_blocks[i+32][j   ] = dist[block_i*64*n + offset + (i+32)*n + j];
    row_blocks[i   ][j+32] = dist[block_i*64*n + offset + i*n + j + 32];
    row_blocks[i+32][j+32] = dist[block_i*64*n + offset + (i+32)*n + j+32];

    col_blocks[i   ][j   ] = dist[offset*n + block_i*64 + i*n + j];
    col_blocks[i+32][j   ] = dist[offset*n + block_i*64 + (i+32)*n + j];
    col_blocks[i   ][j+32] = dist[offset*n + block_i*64 + i*n + j+32];
    col_blocks[i+32][j+32] = dist[offset*n + block_i*64 + (i+32)*n + j+32];

    both[i   ][j   ] = dist[offset*(n+1) + i*n + j];
    both[i+32][j   ] = dist[offset*(n+1) + (i+32)*n + j];
    both[i   ][j+32] = dist[offset*(n+1) + i*n + j+32];
    both[i+32][j+32] = dist[offset*(n+1) + (i+32)*n + j+32];
  
    __syncthreads();

    #pragma unroll 32
    for (int k = 0; k < 64; k++) {

        row_blocks[i   ][j   ] = min(row_blocks[i][j], row_blocks[i][k] + both[k][j]);
        row_blocks[i+32][j   ] = min(row_blocks[i+32][j], row_blocks[i+32][k] + both[k][j]);
        row_blocks[i   ][j+32] = min(row_blocks[i][j+32], row_blocks[i][k] + both[k][j+32]);
        row_blocks[i+32][j+32] = min(row_blocks[i+32][j+32], row_blocks[i+32][k] + both[k][j+32]);

        col_blocks[i   ][j   ] = min(col_blocks[i][j], both[i][k] + col_blocks[k][j]);
        col_blocks[i+32][j   ] = min(col_blocks[i+32][j], both[i+32][k] + col_blocks[k][j]);
        col_blocks[i   ][j+32] = min(col_blocks[i][j+32], both[i][k] + col_blocks[k][j+32]);
        col_blocks[i+32][j+32] = min(col_blocks[i+32][j+32], both[i+32][k] + col_blocks[k][j+32]);
    }

    dist[block_i*64*n + offset + i*n + j]         = row_blocks[i   ][j   ];
    dist[block_i*64*n + offset + (i+32)*n + j]    = row_blocks[i+32][j   ];
    dist[block_i*64*n + offset + i*n + j + 32]    = row_blocks[i   ][j+32];
    dist[block_i*64*n + offset + (i+32)*n + j+32] = row_blocks[i+32][j+32];

    dist[offset*n + block_i*64 + i*n + j]         = col_blocks[i   ][j   ];
    dist[offset*n + block_i*64 + (i+32)*n + j]    = col_blocks[i+32][j   ];
    dist[offset*n + block_i*64 + i*n + j+32]      = col_blocks[i   ][j+32];
    dist[offset*n + block_i*64 + (i+32)*n + j+32] = col_blocks[i+32][j+32];
}
