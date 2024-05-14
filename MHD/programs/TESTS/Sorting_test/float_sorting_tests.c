/*
 *  float_sorting_tests.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 05/06/24.
 *
 */

#include "float_sorting_tests.h"


struct sort_float_array * init_sort_float_array(int nthreads, long Narray){
    struct sort_float_array *rSort = (struct sort_float_array *) malloc(sizeof(struct sort_float_array));
    if(rSort == NULL){
        printf("Allocation sort_float_array failed.\n");
        exit(1);
    }
    
    rSort->nthreads = nthreads;
    rSort->Narray =   Narray;
    rSort->nextP2 =    1 + (int) log2((double) (rSort->Narray-1));
    rSort->narrayP2 =  1 << rSort->nextP2;
    
    posix_memalign((void**)&rSort->org, 4096, rSort->narrayP2*sizeof(float));
    
    init_Float_Array(rSort->Narray, rSort->narrayP2, rSort->org);
    return rSort;
}

void alloc_sort_float_works(struct sort_float_array *rSort){
    posix_memalign((void**)&rSort->idx,  4096, rSort->narrayP2*sizeof(long));
    posix_memalign((void**)&rSort->ra,   4096, rSort->narrayP2*sizeof(float));
    copy_Float_Array(rSort->Narray, rSort->narrayP2, 
                     rSort->org, rSort->ra, rSort->idx);
    return;
}

void dealloc_sort_float_works(struct sort_float_array *rSort){
    free(rSort->ra);
    free(rSort->idx);
    return;
}
void dealloc_sort_float_array(struct sort_float_array *rSort){
    free(rSort->org);
    free(rSort);
    return;
}

double quicksort_float_test(struct sort_float_array *rSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_float_works(rSort);
    printf("Quicksort......");
    gettimeofday( &startwtime, NULL );
    quicksort_real_c(rSort->ra, rSort->idx, 0, (rSort->Narray-1));
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                          + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Float(rSort->Narray, rSort->ra);
    print_sorted_Float(rSort->Narray, rSort->narrayP2,
                     rSort->org, rSort->ra, rSort->idx);
    dealloc_sort_float_works(rSort);
    return seq_time1;
}

double bitonicsort_pthread_float_test(struct sort_float_array *rSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_float_works(rSort);
    printf("Bitonic parallel recursive with %i threads...",
           rSort->nthreads);
    gettimeofday( &startwtime, NULL );
    bitonicsort_Float_Pthread(rSort->nthreads, rSort->narrayP2,
                               rSort->ra, rSort->idx);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 
                         + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Float(rSort->Narray, rSort->ra);
    print_sorted_Float(rSort->Narray, rSort->narrayP2, rSort->org, rSort->ra, rSort->idx);
    dealloc_sort_float_works(rSort);
    return seq_time1;
}

double bitonicsort_rec_float_test(struct sort_float_array *rSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_float_works(rSort);
    printf("Bitonic serial   recursive......");
    gettimeofday( &startwtime, NULL );
    bitonicsort_rec_Float(rSort->narrayP2, rSort->ra, rSort->idx);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                          + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Float(rSort->Narray, rSort->ra);
    print_sorted_Float(rSort->Narray, rSort->narrayP2,
                     rSort->org, rSort->ra, rSort->idx);
    dealloc_sort_float_works(rSort);
    return seq_time1;
}

double bitonicsort_imp_float_test(struct sort_float_array *rSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_float_works(rSort);
    printf("Bitonic serial  imperative......");
    gettimeofday( &startwtime, NULL );
    BitonicSort_imp_Float(rSort->narrayP2, rSort->ra, rSort->idx);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                          + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Float(rSort->Narray, rSort->ra);
    print_sorted_Float(rSort->Narray, rSort->narrayP2,
                        rSort->org, rSort->ra, rSort->idx);
    dealloc_sort_float_works(rSort);
    return seq_time1;
}

double max_float_array_test(struct sort_float_array *rSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_float_works(rSort);
    gettimeofday( &startwtime, NULL );
    float rmax1 = max_float_array(rSort->Narray, rSort->ra);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    dealloc_sort_float_works(rSort);
    printf("max_float_array        %f\n", rmax1);
    return seq_time1;
}

double max_float_array_pthread_test(struct sort_float_array *rSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_float_works(rSort);
    gettimeofday( &startwtime, NULL );
    float rmax1 = max_Float_Array_pthreads(rSort->nthreads, rSort->Narray, rSort->ra);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    dealloc_sort_float_works(rSort);
    printf("max_Float_Array_pthreads %lf\n", rmax1);
    return seq_time1;
}

double flip_sign_float_test(struct sort_float_array *rSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_float_works(rSort);
    gettimeofday( &startwtime, NULL );
    flip_float_sign(rSort->Narray, rSort->ra);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    dealloc_sort_float_works(rSort);
    return seq_time1;
}

double flip_sign_float_pthread_test(struct sort_float_array *rSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_float_works(rSort);
    gettimeofday( &startwtime, NULL );
    flip_sign_Float_pthreads(rSort->nthreads, rSort->Narray, rSort->ra);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    dealloc_sort_float_works(rSort);
    return seq_time1;
}

#ifdef __APPLE__
double vDSP_vsorti_test(struct sort_float_array *rSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_float_works(rSort);
    printf("Sorting by vDSP_vsortiD...... ");
    gettimeofday( &startwtime, NULL );
    vDSP_Length *kdx_tmp = (vDSP_Length *) calloc(rSort->Narray, sizeof(vDSP_Length));
    for(long i=0;i<rSort->Narray;i++){kdx_tmp[i] = i;};
    vDSP_vsorti(rSort->ra, kdx_tmp, nil, rSort->Narray, 0);
    for(long i=0;i<rSort->Narray;i++){
        rSort->idx[i] = kdx_tmp[i];
        rSort->ra[i] = rSort->org[kdx_tmp[i]];
    };
    free(kdx_tmp);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                          + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Float(rSort->Narray, rSort->ra);
    print_sorted_Float(rSort->Narray, rSort->narrayP2,
                        rSort->org, rSort->ra, rSort->idx);
    dealloc_sort_float_works(rSort);
    return seq_time1;
}
#else
double bitonicsort_OMP_float_test(struct sort_float_array *rSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_float_works(rSort);
    printf("OpenMP Bitonic parallel imperagive with %i threads...",
           rSort->nthreads);
    gettimeofday( &startwtime, NULL );
    OMPimp_float_BitonicSort(rSort->nthreads, rSort->narrayP2,
                             rSort->ra, rSort->idx);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                          + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Float(rSort->Narray, rSort->ra);
    print_sorted_Float(rSort->Narray, rSort->narrayP2,
                        rSort->org, rSort->ra, rSort->idx);
    dealloc_sort_float_works(rSort);
    return seq_time1;
}

double max_float_array_omp_test(struct sort_float_array *rSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_float_works(rSort);
    gettimeofday( &startwtime, NULL );
    float rmax1 = max_double_array_omp(rSort->nthreads, rSort->Narray, rSort->ra);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    dealloc_sort_float_works(rSort);
    printf("max_float_array_omp      %f\n", rmax1);
    return seq_time1;
}

double flip_sign_float_omp_test(struct sort_float_array *rSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_float_works(rSort);
    gettimeofday( &startwtime, NULL );
    flip_double_sign_omp(rSort->nthreads, rSort->Narray, rSort->ra);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    dealloc_sort_float_works(rSort);
    return seq_time1;
}
#endif

