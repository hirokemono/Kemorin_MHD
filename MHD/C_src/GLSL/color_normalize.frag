/*
// color_normalize.frag
//     float color_normalize(int num_tbl,
//                           float d_in[MAX_NORMALIZATION_POINT],
//                           float d_norm[MAX_NORMALIZATION_POINT],
//                           float x)
*/

#define MAX_NORMALIZATION_POINT 16

float color_normalize(int num_tbl,
                      float d_in[MAX_NORMALIZATION_POINT],
                      float d_norm[MAX_NORMALIZATION_POINT],
                      float x){
    float c = 0.0;
    if(x < d_in[0]){
        c = d_norm[0];
    }else if(x >= d_in[num_tbl-1]){
        c = d_norm[num_tbl-1];
        
    }else if(x < d_in[ 1]){
        c = d_norm[ 0] + (d_norm[ 1] - d_norm[ 0])
            * (x - d_in[ 0]) / (d_in[ 1] - d_in[ 0]);
    }else if(x < d_in[ 2]){
        c = d_norm[ 1] + (d_norm[ 2] - d_norm[ 1])
            * (x - d_in[ 1]) / (d_in[ 2] - d_in[ 1]);
    }else if(x < d_in[ 3]){
        c = d_norm[ 2] + (d_norm[ 3] - d_norm[ 2])
            * (x - d_in[ 2]) / (d_in[ 3] - d_in[ 2]);
    }else if(x < d_in[ 4]){
        c = d_norm[ 3] + (d_norm[ 4] - d_norm[ 3])
            * (x - d_in[ 3]) / (d_in[ 4] - d_in[ 3]);
    }else if(x < d_in[ 5]){
        c = d_norm[ 4] + (d_norm[ 5] - d_norm[ 4])
            * (x - d_in[ 4]) / (d_in[ 5] - d_in[ 4]);
    }else if(x < d_in[ 6]){
        c = d_norm[ 5] + (d_norm[ 6] - d_norm[ 5])
            * (x - d_in[ 5]) / (d_in[ 6] - d_in[ 5]);
    }else if(x < d_in[ 7]){
        c = d_norm[ 6] + (d_norm[ 7] - d_norm[ 6])
            * (x - d_in[ 6]) / (d_in[ 7] - d_in[ 6]);
    }else if(x < d_in[ 8]){
        c = d_norm[ 7] + (d_norm[ 8] - d_norm[ 7])
            * (x - d_in[ 7]) / (d_in[ 8] - d_in[ 7]);
    }else if(x < d_in[ 9]){
        c = d_norm[ 8] + (d_norm[ 9] - d_norm[ 8])
            * (x - d_in[ 8]) / (d_in[ 9] - d_in[ 8]);
    }else if(x < d_in[10]){
        c = d_norm[ 9] + (d_norm[10] - d_norm[ 9])
            * (x - d_in[ 9]) / (d_in[10] - d_in[ 9]);
    }else if(x < d_in[11]){
        c = d_norm[10] + (d_norm[11] - d_norm[10])
            * (x - d_in[10]) / (d_in[11] - d_in[10]);
    }else if(x < d_in[12]){
        c = d_norm[11] + (d_norm[12] - d_norm[11])
            * (x - d_in[11]) / (d_in[12] - d_in[11]);
    }else if(x < d_in[13]){
        c = d_norm[12] + (d_norm[13] - d_norm[12])
            * (x - d_in[12]) / (d_in[13] - d_in[12]);
    }else if(x < d_in[14]){
        c = d_norm[13] + (d_norm[14] - d_norm[13])
            * (x - d_in[13]) / (d_in[14] - d_in[13]);
    }else if(x < d_in[15]){
        c = d_norm[14] + (d_norm[15] - d_norm[14])
            * (x - d_in[14]) / (d_in[15] - d_in[14]);
    }else{
        c = d_norm[num_tbl-1];
    };
    return c;
}
