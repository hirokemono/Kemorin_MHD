/*
 *  integer_sorting_tests.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 05/06/24.
 *
 */

#ifndef INTEGER_SORTING_TESTS_
#define INTEGER_SORTING_TESTS_

#include <stdlib.h>
#include <sys/time.h>
#include "array_for_sorting_test.h"

#include "quicksort_c.h"
#include "bitonic_sort_c.h"
#include "bitonic_sort_int_pthread.h"

struct sort_int_array{
    long Narray;
    int nthreads;
    int nextP2;
    long narrayP2;

    long *idx;
    
    int *iorg;
    int *ia;
};

/* prototypes */

struct sort_int_array * init_sort_int_array(int nthreads, long Narray);
void alloc_sort_int_works(struct sort_int_array *_iSort);
void dealloc_sort_int_works(struct sort_int_array *_iSort);
void dealloc_sort_int_array(struct sort_int_array *_iSort);

double quicksort_int_test(struct sort_int_array *_iSort);
double bitonicsort_pthread_int_test(struct sort_int_array *_iSort);
double bitonicsort_rec_int_test(struct sort_int_array *_iSort);
double bitonicsort_imp_int_test(struct sort_int_array *_iSort);

double max_int_array_test(struct sort_int_array *_iSort);
double max_int_array_pthread_test(struct sort_int_array *_iSort);

double flip_sign_int_test(struct sort_int_array *_iSort);
double flip_sign_int_pthread_test(struct sort_int_array *_iSort);

#ifndef __vDSP__
double bitonicsort_OMP_int_test(struct sort_int_array *_iSort);
    double max_int_array_omp_test(struct sort_int_array *_iSort);
    double flip_sign_int_omp_test(struct sort_int_array *_iSort);
#endif

#endif /* INTEGER_SORTING_TESTS_ */