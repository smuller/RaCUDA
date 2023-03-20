void computeGold(float* P, // Resultant matrix data
   const float* M, // Matrix M
   const float* N, // Matrix N
   int Mh, // Matrix M height
   int Mw, // Matrix M width
   int Nw) // Matrix N width
{
   int block_idx_x,block_idx_y,sblk_idx_x;
   int block_dim_y = (Mh+32-1)/32; // ceil(Mh/32)
   int block_dim_x = (Mh+32-1)/32; // square blocks assumed

   float sum, a, b;
   int i, j, k, Row,Col,k_global;

   for (block_idx_y=0; block_idx_y < block_dim_y;block_idx_y++) {
      for (block_idx_x=0;block_idx_x < block_dim_x;block_idx_x++) {

         // processing a block of the M and N matrix to take advantage
         // of temporal locality of reference that is exploitable in the cache
         for (sblk_idx_x=0;sblk_idx_x < block_dim_x; sblk_idx_x++) {
    
            // for each block compute the partial sums store in targeted location
            // for each block that is being computed
            for (i = 0; i < 32; i++) {
               Row = 32*block_idx_y+i;
               for (j = 0; j < 32; j++){
                  Col =  32*block_idx_x+j;
                  sum = 0;
                  for (k = 0; k < 32; k++){
                     k_global = 32*sblk_idx_x+k;
                     a = M[Row*Mw + k_global];
                     b = N[k_global*Nw+Col];
                     sum += a * b;
                  }
                  if (sblk_idx_x==0)
                     P[Row*Mw + Col]=sum;
                  else
                     P[Row*Mw + Col]+=sum;
               }
            }
         }
      }
   }
}