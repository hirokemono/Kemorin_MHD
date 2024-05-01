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
#include "bitonic_sort_omp.h"
#include "bitonic_sort_pthread.h"

void init(long num, long nArray, int *ires, int *org, long *idx);
void print(long num, long nArray, const int *org, 
           const int *ires, const long *idx);
void test(long num, const int *ires);


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
    double seq_time4, seq_time5;
    
    if (argc != 3 || atoi( argv[ 2 ] ) > 256 ) {
        printf("Usage: %s n t\n  where n is problem size,");
        printf(" and t is the number of threads to use.\n", argv[ 0 ] );
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
    
    int *ia;
    ia = (int *) malloc(narrayP2 * sizeof(int));
    int *org = (int *) malloc(narrayP2 * sizeof(int));
    long *idx = (long *) malloc(narrayP2 * sizeof(long));
    
    init(Narray, narrayP2, ia, org, idx);
    gettimeofday( &startwtime, NULL );
    quicksort_int_c(ia, idx, 0, (Narray-1));
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Quicksort ");
    test(Narray, ia);
    print(Narray, narrayP2, org, ia, idx);
    printf("-------------------------------------\n");
    
    init(Narray, narrayP2, ia, org, idx);
    gettimeofday( &startwtime, NULL );
    bitonicsort_rec_Int(narrayP2, ia, idx);
    gettimeofday( &endwtime, NULL );
    seq_time2 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic serial   recursive ");
    test(Narray, ia);
    print(Narray, narrayP2, org, ia, idx);
    printf("-------------------------------------\n");
    
    init(Narray, narrayP2, ia, org, idx);
    gettimeofday( &startwtime, NULL );
    bitonicsort_Int_Pthread(nthreads, narrayP2, ia, idx);
    gettimeofday( &endwtime, NULL );
    seq_time3 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic parallel recursive with %i threads ", nthreads);
    test(Narray, ia);
    print(Narray, narrayP2, org, ia, idx);
    printf("-------------------------------------\n");
    
    
    init(Narray, narrayP2, ia, org, idx);
    gettimeofday( &startwtime, NULL );
    BitonicSort_imp_Int(narrayP2, ia, idx);
    gettimeofday( &endwtime, NULL );
    seq_time4 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic serial  imperative ");
    test(Narray, ia);
    print(Narray, narrayP2, org, ia, idx);
    printf("-------------------------------------\n");
    
    init(Narray, narrayP2, ia, org, idx);
    gettimeofday( &startwtime, NULL );
    OMPimp_int_BitonicSort(nthreads, narrayP2, ia, idx);
    gettimeofday( &endwtime, NULL );
    seq_time5 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("OpenMP Bitonic parallel imperagive with %i threads ", nthreads);
    test(Narray, ia);
    print(Narray, narrayP2, org, ia, idx);
    printf("-------------------------------------\n\n");
    
    printf("                 Quicksort wall clock time = %f\n", seq_time1);
    printf("Bitonic serial   recursive wall clock time = %f\n", seq_time2);
    printf("Bitonic parallel recursive with %i threads\n", nthreads);
    printf("             and quicksort wall clock time = %f\n", seq_time3);
    printf("Bitonic serial  imperative wall clock time = %f\n", seq_time4);
    printf("OpenMP Bitonic parallel imperagive \n");
    printf("           with %i threads wall clock time = %f\n", nthreads,  seq_time5);
    printf("-------------------------------------\n");
    
    int imax1, imax2, imax3;
    gettimeofday( &startwtime, NULL );
    imax1 = max_int_array(Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    
    gettimeofday( &startwtime, NULL );
    imax2 = max_Int_Array_pthreads(nthreads, Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time2 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    
    gettimeofday( &startwtime, NULL );
    imax3 = max_int_array_omp(nthreads, Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time3 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    
    printf("max_int_array          %d\n", imax1);
    printf("max_Int_Array_pthreads %d\n", imax2);
    printf("max_int_array_omp      %d\n", imax3);
    printf("-------------------------------------\n");
    
    printf("           Serial maximum                 wall clock time = %f\n", seq_time1);
    printf("pthreads parallel maximum with %i threads wall clock time = %f\n",
           nthreads,  seq_time2);
    printf("OpenMP   parallel maximum with %i threads wall clock time = %f\n",
           nthreads,  seq_time3);
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
    
    gettimeofday( &startwtime, NULL );
    flip_int_sign_omp(nthreads, Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("OpenMP   parallel flip_int_sign with %i threads wall clock time = %f\n", 
            nthreads,  seq_time1);
    
    
    free(ia);
}

/** -------------- SUB-PROCEDURES  ----------------- **/ 




/** procedure test() : verify bitonicsort_rec_Int results **/
void test(long num, const int *ires){
  int pass = 1;
  long i;
    for (i=1;i<num;i++) {
    pass &= (ires[i-1] <= ires[i]);
  }
    
  printf(" TEST %s\n",(pass) ? "PASSed" : "FAILed");
}


/** procedure init(long num, int *ires, int *org, long *idx) : initialize array "ires" with data **/
void init(long num, long nArray, int *ires, int *org, long *idx) {
    long i;
    for (i=0;i<num;i++) {
        ires[i] = rand(); // (N - i);
//        ires[i] = rand() % num; // (N - i);
        org[i] = ires[i];
        idx[i] = i;
    }
    int imax = max_int_array(num, ires);
    for (i=num;i<nArray;i++) {
        ires[i] = imax + 1;
        org[i] = ires[i];
        idx[i] = -1;
    }
}

/** procedure  print() : print array elements **/
void print(long num, long nArray, const int *org, 
           const int *ires, const long *idx) {
    int i;
    if(num > 33) return;
  for (i = 0; i < num; i++) {
        printf("%d %d: %d %d \n", i, org[i], (int) idx[i], ires[i]);
  }
  printf("\n");
  for (i = num; i < nArray; i++) {
        printf("%d %d: %d %d \n", i, org[i], (int) idx[i], ires[i]);
  }
}
