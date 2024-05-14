/*
 *  integer_sorting_tests.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 05/06/24.
 *
 */

#include "integer_sorting_tests.h"

struct sort_int_array * init_sort_int_array(int nthreads, long Narray){
    struct sort_int_array *_iSort = (struct sort_int_array *) malloc(sizeof(struct sort_int_array));
    if(_iSort == NULL){
        printf("Allocation sort_int_array failed.\n");
        exit(1);
    }
    
    _iSort->nthreads = nthreads;
    _iSort->Narray =   Narray;
    _iSort->nextP2 =    1 + (int) log2((double) (_iSort->Narray-1));
    _iSort->narrayP2 =  1 << _iSort->nextP2;
    
    posix_memalign((void**)&_iSort->iorg, 4096, _iSort->narrayP2*sizeof(int));
    
    init_Int_Array(_iSort->Narray, _iSort->narrayP2, _iSort->iorg);
    return _iSort;
}

void alloc_sort_int_works(struct sort_int_array *_iSort){
    posix_memalign((void**)&_iSort->idx,  4096, _iSort->narrayP2*sizeof(long));
    posix_memalign((void**)&_iSort->ia,   4096, _iSort->narrayP2*sizeof(int));
    copy_Int_Array(_iSort->Narray, _iSort->narrayP2, 
                   _iSort->iorg, _iSort->ia, _iSort->idx);
    return;
}

void dealloc_sort_int_works(struct sort_int_array *_iSort){
    free(_iSort->ia);
    free(_iSort->idx);
    return;
}
void dealloc_sort_int_array(struct sort_int_array *_iSort){
    free(_iSort->iorg);
    free(_iSort);
    return;
}

double quicksort_int_test(struct sort_int_array *_iSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_int_works(_iSort);
    printf("Quicksort......");
    gettimeofday( &startwtime, NULL );
    quicksort_int_c(_iSort->ia, _iSort->idx, 0, (_iSort->Narray-1));
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                          + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Int(_iSort->Narray, _iSort->ia);
    print_sorted_Int(_iSort->Narray, _iSort->narrayP2,
                     _iSort->iorg, _iSort->ia, _iSort->idx);
    dealloc_sort_int_works(_iSort);
    return seq_time1;
}

double bitonicsort_pthread_int_test(struct sort_int_array *_iSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_int_works(_iSort);
    printf("Bitonic parallel recursive with %i threads...",
           _iSort->nthreads);
    gettimeofday( &startwtime, NULL );
    bitonicsort_Int_Pthread(_iSort->nthreads, _iSort->narrayP2,
                            _iSort->ia, _iSort->idx);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 
                         + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Int(_iSort->Narray, _iSort->ia);
    print_sorted_Int(_iSort->Narray, _iSort->narrayP2, _iSort->iorg, _iSort->ia, _iSort->idx);
    dealloc_sort_int_works(_iSort);
    return seq_time1;
}

double bitonicsort_rec_int_test(struct sort_int_array *_iSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_int_works(_iSort);
    printf("Bitonic serial   recursive ......");
    gettimeofday( &startwtime, NULL );
    bitonicsort_rec_Int(_iSort->narrayP2, _iSort->ia, _iSort->idx);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                          + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Int(_iSort->Narray, _iSort->ia);
    print_sorted_Int(_iSort->Narray, _iSort->narrayP2,
                     _iSort->iorg, _iSort->ia, _iSort->idx);
    dealloc_sort_int_works(_iSort);
    return seq_time1;
}

double bitonicsort_imp_int_test(struct sort_int_array *_iSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_int_works(_iSort);
    printf("Bitonic serial  imperative ......");
    gettimeofday( &startwtime, NULL );
    BitonicSort_imp_Int(_iSort->narrayP2, _iSort->ia, _iSort->idx);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                          + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Int(_iSort->Narray, _iSort->ia);
    print_sorted_Int(_iSort->Narray, _iSort->narrayP2,
                     _iSort->iorg, _iSort->ia, _iSort->idx);
    dealloc_sort_int_works(_iSort);
    return seq_time1;
}

double max_int_array_test(struct sort_int_array *_iSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_int_works(_iSort);
    gettimeofday( &startwtime, NULL );
    int imax1 = max_int_array(_iSort->Narray, _iSort->ia);
    gettimeofday( &endwtime, NULL );
    dealloc_sort_int_works(_iSort);
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf("max_int_array          %d\n", imax1);
    return seq_time1;
}

double max_int_array_pthread_test(struct sort_int_array *_iSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_int_works(_iSort);
    gettimeofday( &startwtime, NULL );
    int imax1 = max_Int_Array_pthreads(_iSort->nthreads, _iSort->Narray, _iSort->ia);
    gettimeofday( &endwtime, NULL );
    dealloc_sort_int_works(_iSort);
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf("max_Int_Array_pthreads  %d\n", imax1);
    return seq_time1;
}

double flip_sign_int_test(struct sort_int_array *_iSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_int_works(_iSort);
    gettimeofday( &startwtime, NULL );
    flip_int_sign(_iSort->Narray, _iSort->ia);
    gettimeofday( &endwtime, NULL );
    dealloc_sort_int_works(_iSort);
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    return seq_time1;
}

double flip_sign_int_pthread_test(struct sort_int_array *_iSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_int_works(_iSort);
    gettimeofday( &startwtime, NULL );
    flip_sign_Int_pthreads(_iSort->nthreads, _iSort->Narray, _iSort->ia);
    gettimeofday( &endwtime, NULL );
    dealloc_sort_int_works(_iSort);
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    return seq_time1;
}

#ifndef __APPLE__
double bitonicsort_OMP_int_test(struct sort_int_array *_iSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_int_works(_iSort);
    printf("OpenMP Bitonic parallel imperagive with %i threads...",
           _iSort->nthreads);
    gettimeofday( &startwtime, NULL );
    OMPimp_int_BitonicSort(_iSort->nthreads, _iSort->narrayP2,
                           _iSort->ia, _iSort->idx);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                          + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Int(_iSort->Narray, _iSort->ia);
    print_sorted_Int(_iSort->Narray, _iSort->narrayP2,
                     _iSort->iorg, _iSort->ia, _iSort->idx);
    dealloc_sort_int_works(_iSort);
    return seq_time1;
}

double max_int_array_omp_test(struct sort_int_array *_iSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_int_works(_iSort);
    gettimeofday( &startwtime, NULL );
    int imax1 = max_int_array_omp(_iSort->nthreads, _iSort->Narray, _iSort->ia);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf("max_int_array_omp      %d\n", imax1);
    dealloc_sort_int_works(_iSort);
    return seq_time1;
}

double flip_sign_int_omp_test(struct sort_int_array *_iSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_int_works(_iSort);
    gettimeofday( &startwtime, NULL );
    flip_int_sign_omp(_iSort->nthreads, _iSort->Narray, _iSort->ia);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    dealloc_sort_int_works(_iSort);
    return seq_time1;
}
#endif

