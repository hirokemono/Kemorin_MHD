/*
 *  float_sorting_tests.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 05/06/24.
 *
 */

#ifndef FLOAT_SORTING_TESTS_
#define FLOAT_SORTING_TESTS_

#include <stdlib.h>
#include <sys/time.h>
#include "array_for_sorting_test.h"

#include "quicksort_c.h"
#include "bitonic_sort_c.h"
#include "bitonic_sort_float_pthread.h"

#ifdef __vDSP__
  #include <Accelerate/Accelerate.h>
#else
  #include "bitonic_sort_omp.h"
#endif

struct sort_float_array{
    long Narray;
    int nthreads;
    int nextP2;
    long narrayP2;

    long *idx;
    
    float *org;
    float *ra;
};


/* prototypes */

struct sort_float_array * init_sort_float_array(int nthreads, long Narray);
void alloc_sort_float_works(struct sort_float_array *rSort);

void dealloc_sort_float_works(struct sort_float_array *rSort);
void dealloc_sort_float_array(struct sort_float_array *rSort);

double quicksort_float_test(struct sort_float_array *rSort);
double bitonicsort_pthread_float_test(struct sort_float_array *rSort);
double bitonicsort_rec_float_test(struct sort_float_array *rSort);
double bitonicsort_imp_float_test(struct sort_float_array *rSort);

double max_float_array_test(struct sort_float_array *rSort);
double max_float_array_pthread_test(struct sort_float_array *rSort);

double flip_sign_float_test(struct sort_float_array *rSort);
double flip_sign_float_pthread_test(struct sort_float_array *rSort);

#ifdef __vDSP__
    double vDSP_vsorti_test(struct sort_float_array *rSort);
#else
    double bitonicsort_OMP_float_test(struct sort_float_array *rSort);
    double max_float_array_omp_test(struct sort_float_array *rSort);
    double flip_sign_float_omp_test(struct sort_float_array *rSort);
#endif


#endif /* FLOAT_SORTING_TESTS_ */
