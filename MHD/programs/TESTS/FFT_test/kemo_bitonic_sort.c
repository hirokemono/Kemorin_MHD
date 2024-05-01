/*
 bitonic.c 

 This file contains two different implementations of the bitonic sort
        recursive  version :  rec
        imperative version :  BitonicSort_imp_Int() 
 

 The bitonic sort is also known as Batcher Sort. 
 For a reference of the algorithm, see the article titled 
 Sorting networks and their applications by K. E. Batcher in 1968 


 The following codes take references to the codes avaiable at 

 http://www.cag.lcs.mit.edu/streamit/results/bitonic/code/c/bitonic.c

 http://www.tools-of-computing.com/tc/CS/Sorts/bitonic_sort.htm

 http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/bitonic/bitonicen.htm 
 */

/* 
------- ---------------------- 
   Nikos Pitsianis, Duke CS 
-----------------------------

    modified by Antotsiou Dafni and Sourgkounis Theodosis

*/


#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <math.h>

#include "quicksort_c.h"
#include "bitonic_sort_c.h"
#include "bitonic_sort_pthread.h"
#include "bitonic_sort_int_pthread.h"

#ifdef __APPLE__
  #include <Accelerate/Accelerate.h>
#else
  #include "bitonic_sort_omp.h"
#endif

void init_Int_Array(long num, long nArray, int *ires, int *iorg, long *idx);
void init_Long_Array(long num, long nArray, long *lres, long *lorg, long *idx);
void init_Double_Array(long num, long nArray, double *dres, double *dorg, long *idx);
void init_Float_Array(long num, long nArray, float *res, float *org, long *idx);

void copy_Int_Array(long nArray, int *ires, int *iorg, long *idx);
void copy_Long_Array(long nArray, long *lres, long *lorg, long *idx);
void copy_Double_Array(long nArray, double *dres, double *dorg, long *idx);
void copy_Float_Array(long nArray, float *res, float *org, long *idx);

void print_sorted_Int(long num, long nArray, const int *iorg, 
                      const int *ires, const long *idx);
void print_sorted_Double(long num, long nArray, const double *dorg, 
                         const double *dres, const long *idx);

void chect_sorted_Int(long num, const int *ires);
void chect_sorted_Double(long num, const double *ires);


/** compare for qsort **/
int desc( const void *a, const void *b ){
    int* arg1 = (int *)a;
    int* arg2 = (int *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 > *arg2 ){ return -1;};
    return 1;
}
int asc( const void *a, const void *b ){
    int* arg1 = (int *)a;
    int* arg2 = (int *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 < *arg2 ){ return -1;};
    return 1;
}

int desc_dbl( const void *a, const void *b ){
    double* arg1 = (double *)a;
    double* arg2 = (double *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 > *arg2 ){ return -1;};
    return 1;
}
int asc_dbl( const void *a, const void *b ){
    double* arg1 = (double *)a;
    double* arg2 = (double *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 < *arg2 ){ return -1;};
    return 1;
}

int desc_long( const void *a, const void *b ){
    long* arg1 = (long *)a;
    long* arg2 = (long *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 > *arg2 ){ return -1;};
    return 1;
}
int asc_long( const void *a, const void *b ){
    long* arg1 = (long *)a;
    long* arg2 = (long *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 < *arg2 ){ return -1;};
    return 1;
}

/** the main program **/ 
int main( int argc, char **argv ) {
    struct timeval startwtime, endwtime;
    double seq_time1, seq_time2, seq_time3;
    double seq_time4, seq_time5, seq_time6;
    
    if (argc != 3 || atoi( argv[ 2 ] ) > 256 ) {
        printf("Usage: %s n t\n  where n is problem size,", argv[ 0 ]);
        printf(" and t is the number of threads to use.\n");
        exit( 1 );
    }
    
    long Narray =  atol(argv[1]);
    int nthreads = atoi(argv[2]);
//    long Narray =  1 << atoi(argv[1]);
//    int nthreads = 1 << atoi(argv[2]);
    
    int nextP2 =  1 + (int) log2((double) (Narray-1));
    long narrayP2 =  1 << nextP2;
    /*
    int nextP2n = 1 + (int) log2((double) (Narray-2));
    int nextP2p = 1 + (int) log2((double) (Narray  ));
    long narrayP2n =  1 << nextP2n;
    long narrayP2p =  1 << nextP2p;
    printf("nextP2   %d %d %d \n", nextP2n,   nextP2,   nextP2p);
    printf("narrayP2 %d %d %d \n", narrayP2n, narrayP2, narrayP2p);
    */
    
    
    int    *ia = (int *)    malloc(narrayP2 * sizeof(int));
    long   *la = (long *)   malloc(narrayP2 * sizeof(long));
    double *da = (double *) malloc(narrayP2 * sizeof(double));
    float  *ra = (float *)  malloc(narrayP2 * sizeof(float));
    
    int *iorg =    (int *)    malloc(narrayP2 * sizeof(int));
    long *lorg =   (long *)   malloc(narrayP2 * sizeof(long));
    double *dorg = (double *) malloc(narrayP2 * sizeof(double));
    float *org =   (float *)  malloc(narrayP2 * sizeof(float));
    
    long *idx = (long *) malloc(narrayP2 * sizeof(long));
    
    init_Int_Array(Narray, narrayP2, ia, iorg, idx);
    init_Long_Array(Narray, narrayP2, la, lorg, idx);
    init_Double_Array(Narray, narrayP2, da, dorg, idx);
    init_Float_Array(Narray, narrayP2, ra, org, idx);
    
    
    copy_Int_Array(narrayP2, ia, iorg, idx);
    gettimeofday( &startwtime, NULL );
    quicksort_int_c(ia, idx, 0, (Narray-1));
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Quicksort ");
    chect_sorted_Int(Narray, ia);
    print_sorted_Int(Narray, narrayP2, iorg, ia, idx);
    printf("-------------------------------------\n");
    
    copy_Int_Array(narrayP2, ia, iorg, idx);
    gettimeofday( &startwtime, NULL );
    bitonicsort_rec_Int(narrayP2, ia, idx);
    gettimeofday( &endwtime, NULL );
    seq_time2 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic serial   recursive ");
    chect_sorted_Int(Narray, ia);
    print_sorted_Int(Narray, narrayP2, iorg, ia, idx);
    printf("-------------------------------------\n");
    
    copy_Int_Array(narrayP2, ia, iorg, idx);
    gettimeofday( &startwtime, NULL );
    bitonicsort_Int_Pthread(nthreads, narrayP2, ia, idx);
    gettimeofday( &endwtime, NULL );
    seq_time3 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic parallel recursive with %i threads ", nthreads);
    chect_sorted_Int(Narray, ia);
    print_sorted_Int(Narray, narrayP2, iorg, ia, idx);
    printf("-------------------------------------\n");
    
    
    copy_Int_Array(narrayP2, ia, iorg, idx);
    gettimeofday( &startwtime, NULL );
    BitonicSort_imp_Int(narrayP2, ia, idx);
    gettimeofday( &endwtime, NULL );
    seq_time4 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic serial  imperative ");
    chect_sorted_Int(Narray, ia);
    print_sorted_Int(Narray, narrayP2, iorg, ia, idx);
    printf("-------------------------------------\n");
    
#ifndef __APPLE__
    copy_Int_Array(narrayP2, ia, iorg, idx);
    gettimeofday( &startwtime, NULL );
    OMPimp_int_BitonicSort(nthreads, narrayP2, ia, idx);
    gettimeofday( &endwtime, NULL );
    seq_time5 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("OpenMP Bitonic parallel imperagive with %i threads ", nthreads);
    chect_sorted_Int(Narray, ia);
    print_sorted_Int(Narray, narrayP2, iorg, ia, idx);
    printf("-------------------------------------\n\n");
#endif
    
    printf("                 Quicksort wall clock time = %f\n", seq_time1);
    printf("Bitonic serial   recursive wall clock time = %f\n", seq_time2);
    printf("Bitonic parallel recursive with %i threads\n", nthreads);
    printf("             and quicksort wall clock time = %f\n", seq_time3);
    printf("Bitonic serial  imperative wall clock time = %f\n", seq_time4);
#ifndef __APPLE__
    printf("OpenMP Bitonic parallel imperagive \n");
    printf("           with %i threads wall clock time = %f\n", nthreads,  seq_time5);
#endif
    printf("-------------------------------------\n");
    
    int imax1, imax2, imax3;
    gettimeofday( &startwtime, NULL );
    imax1 = max_int_array(Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf("max_int_array          %d\n", imax1);
    
    gettimeofday( &startwtime, NULL );
    imax2 = max_Int_Array_pthreads(nthreads, Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time2 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf("max_Int_Array_pthreads %d\n", imax2);
    
#ifndef __APPLE__
    gettimeofday( &startwtime, NULL );
    imax3 = max_int_array_omp(nthreads, Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time3 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
#endif
    
    printf("max_int_array_omp      %d\n", imax3);
    printf("-------------------------------------\n");
    
    printf("           Serial maximum                 wall clock time = %f\n", seq_time1);
    printf("pthreads parallel maximum with %i threads wall clock time = %f\n",
           nthreads,  seq_time2);
#ifndef __APPLE__
    printf("OpenMP   parallel maximum with %i threads wall clock time = %f\n",
           nthreads,  seq_time3);
#endif
    printf("-------------------------------------\n");
    
    
    gettimeofday( &startwtime, NULL );
    flip_int_sign(Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("         Serial flip_int_sign                   wall clock time = %f\n", seq_time1 );
    
    gettimeofday( &startwtime, NULL );
    flip_sign_Int_pthreads(nthreads, Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("pthreads parallel flip_int_sign with %i threads wall clock time = %f\n",
           nthreads,  seq_time1 );
    
#ifndef __APPLE__
    gettimeofday( &startwtime, NULL );
    flip_int_sign_omp(nthreads, Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("OpenMP   parallel flip_int_sign with %i threads wall clock time = %f\n", 
            nthreads,  seq_time1);
#endif
    free(ia);
    
    
    
    copy_Double_Array(narrayP2, da, dorg, idx);
    gettimeofday( &startwtime, NULL );
    quicksort_double_c(da, idx, 0, (Narray-1));
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Quicksort ");
    chect_sorted_Double(Narray, da);
    print_sorted_Double(Narray, narrayP2, dorg, da, idx);
    printf("-------------------------------------\n");
    
    copy_Double_Array(narrayP2, da, dorg, idx);
    gettimeofday( &startwtime, NULL );
    bitonicsort_rec_Double(narrayP2, da, idx);
    gettimeofday( &endwtime, NULL );
    seq_time2 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic serial   recursive ");
    chect_sorted_Double(Narray, da);
    print_sorted_Double(Narray, narrayP2, dorg, da, idx);
    printf("-------------------------------------\n");
    
    copy_Double_Array(narrayP2, da, dorg, idx);
    gettimeofday( &startwtime, NULL );
    bitonicsort_Double_Pthread(nthreads, narrayP2, da, idx);
    gettimeofday( &endwtime, NULL );
    seq_time3 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic parallel recursive with %i threads ", nthreads);
    chect_sorted_Double(Narray, da);
    print_sorted_Double(Narray, narrayP2, dorg, da, idx);
    printf("-------------------------------------\n");
    
    
    copy_Double_Array(narrayP2, da, dorg, idx);
    gettimeofday( &startwtime, NULL );
    BitonicSort_imp_Double(narrayP2, da, idx);
    gettimeofday( &endwtime, NULL );
    seq_time4 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic serial  imperative ");
    chect_sorted_Double(Narray, da);
    print_sorted_Double(Narray, narrayP2, dorg, da, idx);
    printf("-------------------------------------\n");
    
#ifdef __APPLE__
    copy_Double_Array(narrayP2, da, dorg, idx);
    vDSP_Length *kdx_tmp = (vDSP_Length *) calloc(Narray, sizeof(vDSP_Length));
    for(long i=0;i<Narray;i++){kdx_tmp[i] = i;};
    
    gettimeofday( &startwtime, NULL );
    vDSP_vsortiD(da, kdx_tmp, nil, Narray, 0);
    gettimeofday( &endwtime, NULL );
    seq_time6 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    for(long i=0;i<Narray;i++){
        idx[i] = kdx_tmp[i];
        da[i] = dorg[kdx_tmp[i]];
    };
    
    printf("                vDSP_vsortiD ");
    chect_sorted_Double(Narray, da);
    print_sorted_Double(Narray, narrayP2, dorg, da, idx);
    printf("-------------------------------------\n\n");
#else
    copy_Double_Array(narrayP2, da, dorg, idx);
    gettimeofday( &startwtime, NULL );
    OMPimp_double_BitonicSort(nthreads, narrayP2, da, idx);
    gettimeofday( &endwtime, NULL );
    seq_time5 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("OpenMP Bitonic parallel imperagive with %i threads ", nthreads);
    chect_sorted_Double(Narray, da);
    print_sorted_Double(Narray, narrayP2, dorg, da, idx);
    printf("-------------------------------------\n\n");
#endif
    
    printf("                 Quicksort wall clock time = %f\n", seq_time1);
    printf("Bitonic serial   recursive wall clock time = %f\n", seq_time2);
    printf("Bitonic parallel recursive with %i threads\n", nthreads);
    printf("             and quicksort wall clock time = %f\n", seq_time3);
    printf("Bitonic serial  imperative wall clock time = %f\n", seq_time4);
#ifdef __APPLE__
    printf("              vDSP_vsortiD wall clock time = %f\n", seq_time6);
#else
    printf("OpenMP Bitonic parallel imperagive \n");
    printf("           with %i threads wall clock time = %f\n", nthreads,  seq_time5);
#endif
    printf("-------------------------------------\n");
    
    double dmax1, dmax2, dmax3;
    gettimeofday( &startwtime, NULL );
    dmax1 = max_double_array(Narray, da);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf("max_double_array          %lf\n", dmax1);
    
    gettimeofday( &startwtime, NULL );
    dmax2 = max_Double_Array_pthreads(nthreads, Narray, da);
    gettimeofday( &endwtime, NULL );
    seq_time2 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf("max_Double_Array_pthreads %lf\n", dmax2);
    
#ifndef __APPLE__
    gettimeofday( &startwtime, NULL );
    dmax3 = max_double_array_omp(1, Narray, da);
    gettimeofday( &endwtime, NULL );
    seq_time3 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf("max_double_array_omp      %lf\n", dmax3);
    
    printf("-------------------------------------\n");
#endif
    
    printf("           Serial maximum                 wall clock time = %f\n", seq_time1);
    printf("pthreads parallel maximum with %i threads wall clock time = %f\n",
           nthreads,  seq_time2);
#ifndef __APPLE__
    printf("OpenMP   parallel maximum with %i threads wall clock time = %f\n",
           nthreads,  seq_time3);
#endif
    printf("-------------------------------------\n");
    
    
    gettimeofday( &startwtime, NULL );
    flip_double_sign(Narray, da);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("         Serial flip_double_sign                   wall clock time = %f\n", seq_time1 );
    
    gettimeofday( &startwtime, NULL );
    flip_sign_Double_pthreads(nthreads, Narray, da);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("pthreads parallel flip_double_sign with %i threads wall clock time = %f\n",
           nthreads,  seq_time1 );
    
#ifndef __APPLE__
    gettimeofday( &startwtime, NULL );
    flip_double_sign_omp(nthreads, Narray, da);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("OpenMP   parallel flip_double_sign with %i threads wall clock time = %f\n", 
            nthreads,  seq_time1);
#endif
    
    free(da);
}

/** procedure chect_sorted_Int() : verify bitonicsort_rec_Int results **/
void chect_sorted_Int(long num, const int *ires){
  int pass = 1;
  long i;
    for (i=1;i<num;i++) {
    pass &= (ires[i-1] <= ires[i]);
  }
    
  printf(" TEST %s\n",(pass) ? "PASSed" : "FAILed");
}

void chect_sorted_Double(long num, const double *ires){
  int pass = 1;
  long i;
    for (i=1;i<num;i++) {
    pass &= (ires[i-1] <= ires[i]);
  }
    
  printf(" TEST %s\n",(pass) ? "PASSed" : "FAILed");
}


/** procedure init_Int_Array(long num, int *ires, int *iorg, long *idx) : initialize array "ires" with data **/
void init_Int_Array(long num, long nArray, int *ires, int *iorg, long *idx) {
    long i;
    for (i=0;i<num;i++) {
        ires[i] = rand(); // (N - i);
//        ires[i] = rand() % num; // (N - i);
        iorg[i] = ires[i];
        idx[i] = i;
    }
    int imax = max_int_array(num, ires);
    for (i=num;i<nArray;i++) {
        ires[i] = imax + 1;
        iorg[i] = ires[i];
        idx[i] = -1;
    }
}

void init_Long_Array(long num, long nArray, long *lres, long *lorg, long *idx) {
    long i;
    for (i=0;i<num;i++) {
        lres[i] = rand(); // (N - i);
//        lres[i] = rand() % num; // (N - i);
        lorg[i] = lres[i];
        idx[i] = i;
    }
    long lmax = max_long_array(num, lres);
    for (i=num;i<nArray;i++) {
        lres[i] = lmax + 1;
        lorg[i] = lres[i];
        idx[i] = -1;
    }
}

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

void init_Float_Array(long num, long nArray, float *res, float *org, long *idx) {
    long i;
    for (i=0;i<num;i++) {
        res[i] = rand(); // (N - i);
//        res[i] = rand() % num; // (N - i);
        org[i] = res[i];
        idx[i] = i;
    }
    float rmax = max_float_array(num, res);
    for (i=num;i<nArray;i++) {
        res[i] = rmax + 1;
        org[i] = res[i];
        idx[i] = -1;
    }
}


void copy_Int_Array(long nArray, int *ires, int *iorg, long *idx){
    long i;
    for (i=0;i<nArray;i++) {
        ires[i] = iorg[i];
        idx[i] = i;
    }
}

void copy_Long_Array(long nArray, long *lres, long *lorg, long *idx){
    long i;
    for (i=0;i<nArray;i++) {
        lres[i] = lorg[i];
        idx[i] = i;
    }
}

void copy_Double_Array(long nArray, double *dres, double *dorg, long *idx){
    long i;
    for (i=0;i<nArray;i++) {
        dres[i] = dorg[i];
        idx[i] = i;
    }
}

void copy_Float_Array(long nArray, float *res, float *org, long *idx){
    long i;
    for (i=0;i<nArray;i++) {
        res[i] = org[i];
        idx[i] = i;
    }
}

/** procedure  print_sorted_Int() : print array elements **/
void print_sorted_Int(long num, long nArray, const int *iorg, 
                      const int *ires, const long *idx) {
    int i;
    if(num > 33) return;
  for (i = 0; i < num; i++) {
        printf("%d %d: %d %d \n", i, iorg[i], (int) idx[i], ires[i]);
  }
  printf("\n");
  for (i = num; i < nArray; i++) {
        printf("%d %d: %d %d \n", i, iorg[i], (int) idx[i], ires[i]);
  }
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
