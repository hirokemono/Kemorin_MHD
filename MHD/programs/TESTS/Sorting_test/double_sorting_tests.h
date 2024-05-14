/*
 *  double_sorting_tests.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 05/06/24.
 *
 */

#ifndef DOUBLE_SORTING_TESTS_
#define DOUBLE_SORTING_TESTS_

#include <stdlib.h>
#include <sys/time.h>
#include "array_for_sorting_test.h"

#include "quicksort_c.h"
#include "bitonic_sort_c.h"
#include "bitonic_sort_pthread.h"

#ifdef __APPLE__
  #include <Accelerate/Accelerate.h>
#else
  #include "bitonic_sort_omp.h"
#endif

struct sort_double_array{
    long Narray;
    int nthreads;
    int nextP2;
    long narrayP2;

    long *idx;
    
    double *dorg;
    double *da;
};

/* prototypes */

struct sort_double_array * init_sort_double_array(int nthreads, long Narray);
void alloc_sort_double_works(struct sort_double_array *dSort);
void dealloc_sort_double_works(struct sort_double_array *dSort);
void dealloc_sort_double_array(struct sort_double_array *dSort);

double quicksort_double_test(struct sort_double_array *dSort);
double bitonicsort_pthread_double_test(struct sort_double_array *dSort);
double bitonicsort_rec_double_test(struct sort_double_array *dSort);
double bitonicsort_imp_double_test(struct sort_double_array *dSort);

double max_double_array_test(struct sort_double_array *dSort);
double max_double_array_pthread_test(struct sort_double_array *dSort);

double flip_sign_double_test(struct sort_double_array *dSort);
double flip_sign_double_pthread_test(struct sort_double_array *dSort);

#ifdef __APPLE__
    double vDSP_vsortiD_test(struct sort_double_array *dSort);
#else
    double bitonicsort_OMP_double_test(struct sort_double_array *dSort);
    double max_double_array_omp_test(struct sort_double_array *dSort);
    double flip_sign_double_omp_test(struct sort_double_array *dSort);
#endif

#endif /* DOUBLE_SORTING_TESTS_ */
