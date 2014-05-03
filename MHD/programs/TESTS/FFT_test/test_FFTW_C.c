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


int main(){
    double *x, *y, *z;
    double pi;
    int nfld;
    int ngrid;
    int i, j, k;
    
    fftw_plan plan[6];
    
    
    pi = 4.0*atan(1.0);
    nfld = 6;
    ngrid = 512;
	x = (double *)calloc(nfld*ngrid,sizeof(double));
	y = (double *)calloc(nfld*ngrid,sizeof(double));
	z = (double *)calloc(nfld*ngrid,sizeof(double));
    
    for (i=0; i<ngrid; i++) {
        x[nfld*i  ] = 10.0;
        x[nfld*i+1] =  2.0 * sin(2.0*pi * ((double) (i-1)) / ((double) ngrid));
        x[nfld*i+2] =  3.0 * cos(2.0*2.0*pi * ((double) (i-1)) / ((double) ngrid));
        x[nfld*i+3] = -4.0 * sin(3.0*2.0*pi * ((double) (i-1)) / ((double) ngrid));
        x[nfld*i+4] = -5.0 * cos(4.0*2.0*pi * ((double) (i-1)) / ((double) ngrid));
        x[nfld*i+5] =  6.0 * sin(10.0*2.0*pi * ((double) (i-1)) / ((double) ngrid))
                     + 1.5 * cos( 8.0*2.0*pi * ((double) (i-1)) / ((double) ngrid));
    }
    
    for (j=0; j<nfld; j++) {
        kemo_fftw_plan_dft_r2c_1d(plan[0], &nfld, x[], y[])
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