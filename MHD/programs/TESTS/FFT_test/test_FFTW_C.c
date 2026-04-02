//
//  test_FFTW_C.c
//  Kemorin_MHD_Cocoa
//
//  Created by Hiroaki Matsui on 2014/03/23.
//
//

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "kemorin.h"

void init_fft_test_data_c(int nfld, int ngrid, double *x_rtp){
    int i, nd;
    double pi = 4.0*atan(1.0);
    int irnd_max = RAND_MAX();
    
    for(i=0; i<ngrid*nfld; i++){
    };
    
    if(nfld < 1) return;
    for(i=0; i<ngrid; i++){x[nfld*i  ] = 10.0;};
    
    if(nfld < 2) return;
    x[1] = -1.0;
    for(i=1; i<ngrid; i++){x[nfld*i+1] = (-1.0) * x[nfld*(i-1)+1];};
    
    if(nfld < 3) return;
    for(i=0; i<ngrid; i++){
        x[nfld*i+2] = 2.0 * sin(2.0*pi * ((double) (i-1)) / ((double) ngrid));
    };
    
    if(nfld < 4) return;
    for(i=0; i<ngrid; i++){
        x[nfld*i+3] = 3.0 * cos(2.0*2.0*pi * ((double) (i-1)) / ((double) ngrid));
    };
    
    if(nfld < 5) return;
    for(i=0; i<ngrid; i++){
        x[nfld*i+4] = -4.0 * sin(3.0*2.0*pi * ((double) (i-1)) / ((double) ngrid));
    };
    
    if(nfld < 6) return;
    for(i=0; i<ngrid; i++){
        x[nfld*i+5] = -5.0 * cos(4.0*2.0*pi * ((double) (i-1)) / ((double) ngrid));
    };
    
    if(nfld < 7) return;
    for(i=0; i<ngrid; i++){
        x[nfld*i+6] = 6.0 * sin(10.0*2.0*pi * ((double) (i-1)) / ((double) ngrid))
                     + 1.5 * cos( 8.0*2.0*pi * ((double) (i-1)) / ((double) ngrid));
    };
    
    if(nfld < 8) return;
    for(i=0; i<ngrid; i++){
        x[nfld*i+7] =  8.0 * sin(16.0*2.0*pi * ((double) (i-1)) / ((double) ngrid))
                     - 7.0 * cos( 5.0*2.0*pi * ((double) (i-1)) / ((double) ngrid))
                     - 3.0 * sin( 3.0*2.0*pi * ((double) (i-1)) / ((double) ngrid))
                     - 6.5 * cos( 2.0*2.0*pi * ((double) (i-1)) / ((double) ngrid))
                     + 3.0 * sin( 9.0*2.0*pi * ((double) (i-1)) / ((double) ngrid));
    };
    
    return;
}


int main(){
    double *x, *y, *z;
    double pi;
    int ngrid;
    int i, j, k;
    
    int nfld = 6;
	fftw_plan plan[nfld];
	int *iflag;
    
    
    pi = 4.0*atan(1.0);
    ngrid = 128;
	x = (double *)calloc(nfld*ngrid,sizeof(double));
	y = (double *)calloc(nfld*ngrid,sizeof(double));
	z = (double *)calloc(nfld*ngrid,sizeof(double));
    
    init_fft_test_data_c(nfld, ngrid, *x);
    
	for (j=0; j<nfld; j++) {
		plan[j] = fftw_plan_dft_r2c_1d(nfld, x, y, iflag);
    }
    
    for (i=0; i<ngrid*nfld; i++) z[i] = x[i];
    for (i=0; i<ngrid*nfld; i++) y[i] = x[i];
    
    
    for (j=0; j<nfld; j++) {
        printf("Solution for %d \n", j);
        for (i=0; i<ngrid; i++) {
            k = ((i+1)/2-1) * pow(-1,((i-1)%2));
            printf("%d, %d, %lf, %lf, %lf \n", 
                   i, k, x[nfld*i+j], y[nfld*i+j], z[nfld*i+j]);
        }
    }
    
    return 1;
}
