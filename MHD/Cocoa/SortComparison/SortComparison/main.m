//
//  main.m
//  SortComparison
//
//  Created by Hiroaki Matsui on 5/6/24.
//

#include <stdlib.h>

#import <Foundation/Foundation.h>
#import <Metal/Metal.h>
#import <Accelerate/Accelerate.h>

#include "quicksort_c.h"
#include "bitonic_sort_c.h"
#include "bitonic_sort_pthread.h"

#import "MetalSortComparison.h"


void init_Double_Array(long num, long nArray, double *dres, double *dorg, long *idx) {
    long i;
    for (i=0;i<num;i++) {
        dres[i] = 1.0 + (double) (rand() % num);
        dres[i] = 1.0 / dres[i];
        dorg[i] = dres[i];
        idx[i] = i;
    }
    double dmax = max_double_array(num, dres);
    for (i=num;i<nArray;i++) {
        dres[i] = dmax + 1.0;
        dorg[i] = dres[i];
        idx[i] = -1;
    }
}

void copy_Double_Array(long nArray, double *dres, double *dorg, long *idx){
    long i;
    for (i=0;i<nArray;i++) {
        dres[i] = dorg[i];
        idx[i] = i;
    }
}

void check_sorted_Double(long num, const double *ires){
  int pass = 1;
  long i;
    for (i=1;i<num;i++) {
    pass &= (ires[i-1] <= ires[i]);
  }
    
  printf(" TEST %s\n",(pass) ? "PASSed" : "FAILed");
}

void print_sorted_Double(long num, long nArray, const double *dorg,
                         const double *dres, const long *idx) {
    int i;
    if(num > 33) return;
  for (i = 0; i < num; i++) {
        printf("%d %lf: %d %lf \n", i, dorg[i], (int) idx[i], dres[i]);
  }
  printf("\n");
  for (i = num; i < nArray; i++) {
        printf("%d %lf: %d %lf \n", i, dorg[i], (int) idx[i], dres[i]);
  }
}


int main(int argc, const char * argv[]) {
    @autoreleasepool {
        int nthreads = 32;
        
        
        id<MTLDevice> device = MTLCreateSystemDefaultDevice();

        // Create the custom object used to encapsulate the Metal code.
        // Initializes objects to communicate with the GPU.
        MetalSortComparison* _bitonic = [[MetalSortComparison alloc] initWithDevice:device];

        struct sortdata *_d;
        if((_d = (struct sortdata *) malloc(sizeof(struct sortdata))) == NULL) {
            printf("malloc error for sortdata\n");
            exit(0);
        }
        [_bitonic prepareSort:_d];



        [_bitonic sendSortCommand:_d];
        NSLog(@"Sort finished");
 
        _d->narrayP2 =  1 << _d->nextP2;
        double *da = (double *) malloc(_d->narrayP2 * sizeof(double));
        double *dorg = (double *) malloc(_d->narrayP2 * sizeof(double));
        long *idx = (long *) malloc(_d->narrayP2 * sizeof(long));
        init_Double_Array(_d->sortLength, _d->narrayP2, da, dorg, idx);
        copy_Double_Array(_d->narrayP2, da, dorg, idx);
        vDSP_Length *kdx_tmp = (vDSP_Length *) calloc(_d->sortLength, sizeof(vDSP_Length));
        for(long i=0;i<_d->sortLength;i++){kdx_tmp[i] = i;};
        
        struct timeval startwtime;
        struct timeval endwtime;

        copy_Double_Array(_d->narrayP2, da, dorg, idx);
        gettimeofday( &startwtime, NULL );
        quicksort_double_c(da, idx, 0, (_d->sortLength-1));
        gettimeofday( &endwtime, NULL );
        double seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                    + endwtime.tv_sec - startwtime.tv_sec );
        printf("Quicksort ");
        check_sorted_Double(_d->sortLength, da);
        print_sorted_Double(_d->sortLength, _d->narrayP2, dorg, da, idx);
        printf("-------------------------------------\n");
        
        copy_Double_Array(_d->narrayP2, da, dorg, idx);
        gettimeofday( &startwtime, NULL );
        bitonicsort_rec_Double(_d->narrayP2, da, idx);
        gettimeofday( &endwtime, NULL );
        double seq_time2 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                    + endwtime.tv_sec - startwtime.tv_sec );
        printf("Bitonic serial   recursive ");
        check_sorted_Double(_d->sortLength, da);
        print_sorted_Double(_d->sortLength, _d->narrayP2, dorg, da, idx);
        printf("-------------------------------------\n");
        
        copy_Double_Array(_d->narrayP2, da, dorg, idx);
        gettimeofday( &startwtime, NULL );
        bitonicsort_Double_Pthread(nthreads, _d->narrayP2, da, idx);
        gettimeofday( &endwtime, NULL );
        double seq_time3 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                             + endwtime.tv_sec - startwtime.tv_sec );
        printf("Bitonic parallel recursive with %i threads ", nthreads);
        check_sorted_Double(_d->sortLength, da);
        print_sorted_Double(_d->sortLength, _d->narrayP2, dorg, da, idx);
        printf("-------------------------------------\n");
        
        
        copy_Double_Array(_d->narrayP2, da, dorg, idx);
        gettimeofday( &startwtime, NULL );
        BitonicSort_imp_Double(_d->narrayP2, da, idx);
        gettimeofday( &endwtime, NULL );
        double seq_time4 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                    + endwtime.tv_sec - startwtime.tv_sec );
        printf("Bitonic serial  imperative ");
        check_sorted_Double(_d->sortLength, da);
        print_sorted_Double(_d->sortLength, _d->narrayP2, dorg, da, idx);
        printf("-------------------------------------\n");


        gettimeofday( &startwtime, NULL );
        gettimeofday( &startwtime, NULL );
        vDSP_vsortiD(da, kdx_tmp, nil, _d->sortLength, 0);
//        vDSP_vsorti(da, kdx_tmp, nil, _d->sortLength, 0);
        gettimeofday( &endwtime, NULL );
        double seq_time6 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                             + endwtime.tv_sec - startwtime.tv_sec );
        for(long i=0;i<_d->sortLength;i++){
            idx[i] = kdx_tmp[i];
            da[i] = dorg[kdx_tmp[i]];
        };
        
        printf("                vDSP_vsortiD ");
        check_sorted_Double(_d->sortLength, da);
        print_sorted_Double(_d->sortLength, _d->narrayP2, dorg, da, idx);
        printf("-------------------------------------\n\n");
        printf("                 Quicksort wall clock time = %f\n", seq_time1);
        printf("Bitonic serial   recursive wall clock time = %f\n", seq_time2);
        printf("Bitonic parallel recursive with %i threads\n", nthreads);
        printf("             and quicksort wall clock time = %f\n", seq_time3);
        printf("Bitonic serial  imperative wall clock time = %f\n", seq_time4);
        printf("              vDSP_vsortiD wall clock time = %f\n", seq_time6);


    }
    return 0;
}
