/*
 *  double_sorting_tests.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 05/06/24.
 *
 */

#include "double_sorting_tests.h"

struct sort_double_array * init_sort_double_array(int nthreads, long Narray){
    struct sort_double_array *dSort = (struct sort_double_array *) malloc(sizeof(struct sort_double_array));
    if(dSort == NULL){
        printf("Allocation sort_double_array failed.\n");
        exit(1);
    }
    
    dSort->nthreads = nthreads;
    dSort->Narray =   Narray;
    dSort->nextP2 =    1 + (int) log2((double) (dSort->Narray-1));
    dSort->narrayP2 =  1 << dSort->nextP2;
    
    posix_memalign((void**)&dSort->dorg, 4096, dSort->narrayP2*sizeof(double));
    
    init_Double_Array(dSort->Narray, dSort->narrayP2, dSort->dorg);
    return dSort;
}

void alloc_sort_double_works(struct sort_double_array *dSort){
    posix_memalign((void**)&dSort->idx,  4096, dSort->narrayP2*sizeof(long));
    posix_memalign((void**)&dSort->da,   4096, dSort->narrayP2*sizeof(double));
    copy_Double_Array(dSort->Narray, dSort->narrayP2, 
                      dSort->dorg, dSort->da, dSort->idx);
    return;
}

void dealloc_sort_double_works(struct sort_double_array *dSort){
    free(dSort->da);
    free(dSort->idx);
    return;
}
void dealloc_sort_double_array(struct sort_double_array *dSort){
    free(dSort->dorg);
    free(dSort);
    return;
}

double quicksort_double_test(struct sort_double_array *dSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_double_works(dSort);
    printf("Quicksort......");
    gettimeofday( &startwtime, NULL );
    quicksort_double_c(dSort->da, dSort->idx, 0, (dSort->Narray-1));
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                          + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Double(dSort->Narray, dSort->da);
    print_sorted_Double(dSort->Narray, dSort->narrayP2,
                     dSort->dorg, dSort->da, dSort->idx);
    dealloc_sort_double_works(dSort);
    return seq_time1;
}

double bitonicsort_pthread_double_test(struct sort_double_array *dSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_double_works(dSort);
    printf("Bitonic parallel recursive with %i threads...",
           dSort->nthreads);
    gettimeofday( &startwtime, NULL );
    bitonicsort_Double_Pthread(dSort->nthreads, dSort->narrayP2,
                               dSort->da, dSort->idx);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 
                         + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Double(dSort->Narray, dSort->da);
    print_sorted_Double(dSort->Narray, dSort->narrayP2, dSort->dorg, dSort->da, dSort->idx);
    dealloc_sort_double_works(dSort);
    return seq_time1;
}

double bitonicsort_rec_double_test(struct sort_double_array *dSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_double_works(dSort);
    printf("Bitonic serial   recursive......");
    gettimeofday( &startwtime, NULL );
    bitonicsort_rec_Double(dSort->narrayP2, dSort->da, dSort->idx);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                          + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Double(dSort->Narray, dSort->da);
    print_sorted_Double(dSort->Narray, dSort->narrayP2,
                     dSort->dorg, dSort->da, dSort->idx);
    dealloc_sort_double_works(dSort);
    return seq_time1;
}

double bitonicsort_imp_double_test(struct sort_double_array *dSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_double_works(dSort);
    printf("Bitonic serial  imperative......");
    gettimeofday( &startwtime, NULL );
    BitonicSort_imp_Double(dSort->narrayP2, dSort->da, dSort->idx);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                          + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Double(dSort->Narray, dSort->da);
    print_sorted_Double(dSort->Narray, dSort->narrayP2,
                        dSort->dorg, dSort->da, dSort->idx);
    dealloc_sort_double_works(dSort);
    return seq_time1;
}

double max_double_array_test(struct sort_double_array *dSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_double_works(dSort);
    gettimeofday( &startwtime, NULL );
    double dmax1 = max_double_array(dSort->Narray, dSort->da);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    dealloc_sort_double_works(dSort);
    printf("max_double_array        %lf\n", dmax1);
    return seq_time1;
}

double max_double_array_pthread_test(struct sort_double_array *dSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_double_works(dSort);
    gettimeofday( &startwtime, NULL );
    double dmax1 = max_Double_Array_pthreads(dSort->nthreads, dSort->Narray, dSort->da);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    dealloc_sort_double_works(dSort);
    printf("max_Double_Array_pthreads %lf\n", dmax1);
    return seq_time1;
}

double flip_sign_double_test(struct sort_double_array *dSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_double_works(dSort);
    gettimeofday( &startwtime, NULL );
    flip_double_sign(dSort->Narray, dSort->da);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    dealloc_sort_double_works(dSort);
    return seq_time1;
}

double flip_sign_double_pthread_test(struct sort_double_array *dSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_double_works(dSort);
    gettimeofday( &startwtime, NULL );
    flip_sign_Double_pthreads(dSort->nthreads, dSort->Narray, dSort->da);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    dealloc_sort_double_works(dSort);
    return seq_time1;
}

#ifdef __vDSP__
double vDSP_vsortiD_test(struct sort_double_array *dSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_double_works(dSort);
    vDSP_Length *kdx_tmp = (vDSP_Length *) calloc(dSort->Narray, sizeof(vDSP_Length));
    for(long i=0;i<dSort->Narray;i++){kdx_tmp[i] = i;};
    printf("Sort by vDSP_vsortiD......");
    
    gettimeofday( &startwtime, NULL );
    vDSP_vsortiD(dSort->da, kdx_tmp, nil, dSort->Narray, 0);
    for(long i=0;i<dSort->Narray;i++){
        dSort->idx[i] = kdx_tmp[i];
        dSort->da[i] = dSort->dorg[kdx_tmp[i]];
    };
    gettimeofday( &endwtime, NULL );
    free(kdx_tmp);
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                          + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Double(dSort->Narray, dSort->da);
    print_sorted_Double(dSort->Narray, dSort->narrayP2,
                        dSort->dorg, dSort->da, dSort->idx);
    dealloc_sort_double_works(dSort);
    return seq_time1;
}
#else
double bitonicsort_OMP_double_test(struct sort_double_array *dSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_double_works(dSort);
    printf("OpenMP Bitonic parallel imperagive with %i threads ...",
           dSort->nthreads);
    gettimeofday( &startwtime, NULL );
    OMPimp_double_BitonicSort(dSort->nthreads, dSort->narrayP2,
                              dSort->da, dSort->idx);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                          + endwtime.tv_sec - startwtime.tv_sec );
    check_sorted_Double(dSort->Narray, dSort->da);
    print_sorted_Double(dSort->Narray, dSort->narrayP2,
                        dSort->dorg, dSort->da, dSort->idx);
    dealloc_sort_double_works(dSort);
    return seq_time1;
}

double max_double_array_omp_test(struct sort_double_array *dSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_double_works(dSort);
    gettimeofday( &startwtime, NULL );
    double rmax1 = max_double_array_omp(dSort->nthreads, dSort->Narray, dSort->da);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    dealloc_sort_double_works(dSort);
    printf("max_double_array_omp      %lf\n", rmax1);
    return seq_time1;
}

double flip_sign_double_omp_test(struct sort_double_array *dSort){
    double seq_time1;
    struct timeval startwtime, endwtime;
    alloc_sort_double_works(dSort);
    gettimeofday( &startwtime, NULL );
    flip_double_sign_omp(dSort->nthreads, dSort->Narray, dSort->da);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    dealloc_sort_double_works(dSort);
    return seq_time1;
}
#endif
