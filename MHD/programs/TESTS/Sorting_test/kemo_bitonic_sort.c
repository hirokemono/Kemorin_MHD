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

#include "array_for_sorting_test.h"
#include "integer_sorting_tests.h"
#include "quicksort_c.h"
#include "bitonic_sort_c.h"
#include "bitonic_sort_pthread.h"
#include "bitonic_sort_int_pthread.h"
#include "bitonic_sort_float_pthread.h"

#ifdef __APPLE__
  #include <Accelerate/Accelerate.h>
#else
  #include "bitonic_sort_omp.h"
#endif





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
    
/*
    int    *ia = (int *)    malloc(narrayP2 * sizeof(int));
    long   *la = (long *)   malloc(narrayP2 * sizeof(long));
    double *da = (double *) malloc(narrayP2 * sizeof(double));
    float  *ra = (float *)  malloc(narrayP2 * sizeof(float));
    
    int *iorg =    (int *)    malloc(narrayP2 * sizeof(int));
    long *lorg =   (long *)   malloc(narrayP2 * sizeof(long));
    double *dorg = (double *) malloc(narrayP2 * sizeof(double));
    float *org =   (float *)  malloc(narrayP2 * sizeof(float));
    
    long *idx = (long *) malloc(narrayP2 * sizeof(long));
*/
    int    *ia;
    long   *la;
    double *da;
    float  *ra;
    
    int *iorg;
    long *lorg;
    double *dorg;
    float *org;
    
    long *idx;
    
    posix_memalign((void**)&idx, 4096, narrayP2*sizeof(long));

    posix_memalign((void**)&la, 4096, narrayP2*sizeof(long));
    posix_memalign((void**)&lorg, 4096, narrayP2*sizeof(long));
    init_Long_Array(Narray, narrayP2, lorg);

    free(lorg);
    free(la);
    
    posix_memalign((void**)&ia, 4096, narrayP2*sizeof(int));
    posix_memalign((void**)&iorg, 4096, narrayP2*sizeof(int));
    init_Int_Array(Narray, narrayP2, iorg);
    struct sort_int_array *_iSort = init_sort_int_array(nthreads, Narray);
    
    seq_time1 = max_int_array_test(_iSort);
    seq_time2 = max_int_array_pthread_test(_iSort);
#ifndef __APPLE__
    seq_time3 = max_int_array_omp_test(_iSort);
#endif
    
    printf("-------------------------------------\n");
    printf("           Serial maximum                 wall clock time = %f\n", seq_time1);
    printf("pthreads parallel maximum with %i threads wall clock time = %f\n",
           nthreads,  seq_time2);
#ifndef __APPLE__
    printf("OpenMP   parallel maximum with %i threads wall clock time = %f\n",
           nthreads,  seq_time3);
#endif
    printf("-------------------------------------\n");
    
    seq_time1 = flip_sign_int_test(_iSort);
    seq_time2 = flip_sign_int_pthread_test(_iSort);
    
    printf("         Serial flip_int_sign                   wall clock time = %f\n", seq_time1 );
    printf("pthreads parallel flip_int_sign with %i threads wall clock time = %f\n",
           nthreads,  seq_time2);
    
#ifndef __APPLE__
    seq_time3 = flip_sign_int_omp_test(_iSort);
    printf("OpenMP   parallel flip_int_sign with %i threads wall clock time = %f\n",
            nthreads,  seq_time1);
#endif
    
    printf("\n-------------------------------------\n");
    printf("--- Integer sorting ---\n");
    seq_time1 = quicksort_int_test(_iSort);
    seq_time2 = bitonicsort_rec_int_test(_iSort);
    seq_time4 = bitonicsort_imp_int_test(_iSort);
    seq_time3 = bitonicsort_pthread_int_test(_iSort);
    
#ifndef __APPLE__
    seq_time5 = bitonicsort_OMP_int_test(_iSort);
#endif
    
    printf("-------------------------------------\n\n");
    printf("--- Integer sorting wall clock times ---\n");
    printf("                 Quicksort wall clock time = %f\n", seq_time1);
    printf("Bitonic serial   recursive wall clock time = %f\n", seq_time2);
    printf("Bitonic serial  imperative wall clock time = %f\n", seq_time4);
    printf("Bitonic parallel recursive with %i threads\n", nthreads);
    printf("             and quicksort wall clock time = %f\n", seq_time3);
#ifndef __APPLE__
    printf("OpenMP Bitonic parallel imperagive \n");
    printf("           with %i threads wall clock time = %f\n", nthreads,  seq_time5);
#endif
    printf("-------------------------------------\n");
    
    free(iorg);
    free(ia);


    
    printf("\n-------------------------------------\n");
    printf("--- Single precision sorting ---\n");
    posix_memalign((void**)&ra, 4096, narrayP2*sizeof(float));
    posix_memalign((void**)&org, 4096, narrayP2*sizeof(float));
    init_Float_Array(Narray, narrayP2, org);
    
    copy_Float_Array(Narray, narrayP2, org, ra, idx);
    gettimeofday( &startwtime, NULL );
    quicksort_real_c(ra, idx, 0, (Narray-1));
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Quicksort ");
    check_sorted_Float(Narray, ra);
    print_sorted_Float(Narray, narrayP2, org, ra, idx);
    
    copy_Float_Array(Narray, narrayP2, org, ra, idx);
    gettimeofday( &startwtime, NULL );
    bitonicsort_rec_Float(narrayP2, ra, idx);
    gettimeofday( &endwtime, NULL );
    seq_time2 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic serial   recursive ");
    check_sorted_Float(Narray, ra);
    print_sorted_Float(Narray, narrayP2, org, ra, idx);
    
    copy_Float_Array(Narray, narrayP2, org, ra, idx);
    gettimeofday( &startwtime, NULL );
    bitonicsort_Float_Pthread(nthreads, narrayP2, ra, idx);
    gettimeofday( &endwtime, NULL );
    seq_time3 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic parallel recursive with %i threads ", nthreads);
    check_sorted_Float(Narray, ra);
    print_sorted_Float(Narray, narrayP2, org, ra, idx);
    
    
    copy_Float_Array(Narray, narrayP2, org, ra, idx);
    gettimeofday( &startwtime, NULL );
    BitonicSort_imp_Float(narrayP2, ra, idx);
    gettimeofday( &endwtime, NULL );
    seq_time4 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic serial  imperative ");
    check_sorted_Float(Narray, ra);
    print_sorted_Float(Narray, narrayP2, org, ra, idx);
    
#ifdef __APPLE__
    copy_Float_Array(Narray, narrayP2, org, ra, idx);
    gettimeofday( &startwtime, NULL );

    vDSP_Length *ldx_tmp = (vDSP_Length *) calloc(Narray, sizeof(vDSP_Length));
    for(long i=0;i<Narray;i++){ldx_tmp[i] = i;};
    
    vDSP_vsorti(ra, ldx_tmp, nil, Narray, 0);
    for(long i=0;i<Narray;i++){
        idx[i] = ldx_tmp[i];
        ra[i] = org[ldx_tmp[i]];
    };
    free(ldx_tmp);
    gettimeofday( &endwtime, NULL );
    seq_time6 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );

    printf("                vDSP_vsorti ");
    check_sorted_Float(Narray, ra);
    print_sorted_Float(Narray, narrayP2, org, ra, idx);
#else
    copy_Float_Array(Narray, narrayP2, org, ra, idx);
    gettimeofday( &startwtime, NULL );
    OMPimp_float_BitonicSort(nthreads, narrayP2, ra, idx);
    gettimeofday( &endwtime, NULL );
    seq_time5 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("OpenMP Bitonic parallel imperagive with %i threads ", nthreads);
    check_sorted_Float(Narray, ra);
    print_sorted_Float(Narray, narrayP2, org, ra, idx);
#endif
    
    free(ra);
    free(org);
    
    printf("-------------------------------------\n\n");
    printf("--- Single precision sorting wall clock times ---\n");
    printf("                 Quicksort wall clock time = %f\n", seq_time1);
    printf("Bitonic serial   recursive wall clock time = %f\n", seq_time2);
    printf("Bitonic parallel recursive with %i threads\n", nthreads);
    printf("             and quicksort wall clock time = %f\n", seq_time3);
    printf("Bitonic serial  imperative wall clock time = %f\n", seq_time4);
#ifdef __APPLE__
    printf("               vDSP_vsorti wall clock time = %f\n", seq_time6);
#else
    printf("OpenMP Bitonic parallel imperagive \n");
    printf("           with %i threads wall clock time = %f\n", nthreads,  seq_time5);
#endif
    printf("-------------------------------------\n");

    
    printf("\n-------------------------------------\n");
    posix_memalign((void**)&da, 4096, narrayP2*sizeof(double));
    posix_memalign((void**)&dorg, 4096, narrayP2*sizeof(double));
    init_Double_Array(Narray, narrayP2, dorg);

    double dmax1, dmax2, dmax3;
    gettimeofday( &startwtime, NULL );
    copy_Double_Array(Narray, narrayP2, dorg, da, idx);
    dmax1 = max_double_array(Narray, da);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf("max_double_array          %lf\n", dmax1);
    
    gettimeofday( &startwtime, NULL );
    copy_Double_Array(Narray, narrayP2, dorg, da, idx);
    dmax2 = max_Double_Array_pthreads(nthreads, Narray, da);
    gettimeofday( &endwtime, NULL );
    seq_time2 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf("max_Double_Array_pthreads %lf\n", dmax2);
    
#ifndef __APPLE__
    gettimeofday( &startwtime, NULL );
    copy_Double_Array(Narray, narrayP2, dorg, da, idx);
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
    copy_Double_Array(Narray, narrayP2, dorg, da, idx);
    flip_double_sign(Narray, da);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("         Serial flip_double_sign                   wall clock time = %f\n", seq_time1 );
    
    gettimeofday( &startwtime, NULL );
    copy_Double_Array(Narray, narrayP2, dorg, da, idx);
    flip_sign_Double_pthreads(nthreads, Narray, da);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("pthreads parallel flip_double_sign with %i threads wall clock time = %f\n",
           nthreads,  seq_time1 );
    
#ifndef __APPLE__
    gettimeofday( &startwtime, NULL );
    copy_Double_Array(Narray, narrayP2, dorg, da, idx);
    flip_double_sign_omp(nthreads, Narray, da);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("OpenMP   parallel flip_double_sign with %i threads wall clock time = %f\n",
            nthreads,  seq_time1);
#endif
    printf("\n--- Double precision sorting ---\n");

    copy_Double_Array(Narray, narrayP2, dorg, da, idx);
    gettimeofday( &startwtime, NULL );
    quicksort_double_c(da, idx, 0, (Narray-1));
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Quicksort ");
    check_sorted_Double(Narray, da);
    print_sorted_Double(Narray, narrayP2, dorg, da, idx);
    
    copy_Double_Array(Narray, narrayP2, dorg, da, idx);
    gettimeofday( &startwtime, NULL );
    bitonicsort_rec_Double(narrayP2, da, idx);
    gettimeofday( &endwtime, NULL );
    seq_time2 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic serial   recursive ");
    check_sorted_Double(Narray, da);
    print_sorted_Double(Narray, narrayP2, dorg, da, idx);
    
    copy_Double_Array(Narray, narrayP2, dorg, da, idx);
    gettimeofday( &startwtime, NULL );
    bitonicsort_Double_Pthread(nthreads, narrayP2, da, idx);
    gettimeofday( &endwtime, NULL );
    seq_time3 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic parallel recursive with %i threads ", nthreads);
    check_sorted_Double(Narray, da);
    print_sorted_Double(Narray, narrayP2, dorg, da, idx);
    
    
    copy_Double_Array(Narray, narrayP2, dorg, da, idx);
    gettimeofday( &startwtime, NULL );
    BitonicSort_imp_Double(narrayP2, da, idx);
    gettimeofday( &endwtime, NULL );
    seq_time4 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic serial  imperative ");
    check_sorted_Double(Narray, da);
    print_sorted_Double(Narray, narrayP2, dorg, da, idx);
    
#ifdef __APPLE__
    copy_Double_Array(Narray, narrayP2, dorg, da, idx);
    gettimeofday( &startwtime, NULL );
    vDSP_Length *kdx_tmp = (vDSP_Length *) calloc(Narray, sizeof(vDSP_Length));
    for(long i=0;i<Narray;i++){kdx_tmp[i] = i;};
    vDSP_vsortiD(da, kdx_tmp, nil, Narray, 0);
    for(long i=0;i<Narray;i++){
        idx[i] = kdx_tmp[i];
        da[i] = dorg[kdx_tmp[i]];
    };
    free(kdx_tmp);
    gettimeofday( &endwtime, NULL );
    seq_time6 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );

    printf("                vDSP_vsortiD ");
    check_sorted_Double(Narray, da);
    print_sorted_Double(Narray, narrayP2, dorg, da, idx);
#else
    copy_Double_Array(Narray, narrayP2, dorg, da, idx);
    gettimeofday( &startwtime, NULL );
    OMPimp_double_BitonicSort(nthreads, narrayP2, da, idx);
    gettimeofday( &endwtime, NULL );
    seq_time5 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("OpenMP Bitonic parallel imperagive with %i threads ", nthreads);
    check_sorted_Double(Narray, da);
    print_sorted_Double(Narray, narrayP2, dorg, da, idx);
#endif

    printf("-------------------------------------\n\n");
    printf("--- Double precision sorting wall clock times ---\n");
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
    
    free(dorg);
    free(da);
}
