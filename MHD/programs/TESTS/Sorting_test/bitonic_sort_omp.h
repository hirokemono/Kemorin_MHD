/*
 bitonic_sort_omp.h

 This file contains two different implementations of the bitonic sort
        imperative version :  OMPimp_int_BitonicSort(int nthreads, long num, int *ires, long *idx);
        imperative version :  OMPimp_long_BitonicSort(int nthreads, long num, long *lres, long *idx);

        imperative version :  OMPimp_double_BitonicSort(int nthreads, long num, double *res, long *idx);
 

 The bitonic sort is also known as Batcher Sort. 
 For a reference of the algorithm, see the article titled 
 Sorting networks and their applications by K. E. Batcher in 1968 


 The following codes take references to the codes avaiable at 

 http://www.cag.lcs.mit.edu/streamit/results/bitonic/code/c/bitonic.c

 http://www.tools-of-computing.com/tc/CS/Sorts/bitonic_sort.htm

 http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/bitonic/bitonicen.htm 
 */

#ifndef BITONIC_SORT_OMP_
#define BITONIC_SORT_OMP_

#include <math.h>
#include <omp.h>

#include "quicksort_c.h"

void OMPimp_int_BitonicSort(int nthreads, long num, int *ires, long *idx);
void flip_int_sign_omp(int nthreads, long num, int *ires);
int max_int_array_omp(int nthreads, long num, const int *ires);

void OMPimp_long_BitonicSort(int nthreads, long num, long *lres, long *idx);
void flip_long_sign_omp(int nthreads, long num, long *lres);
long max_long_array_omp(int nthreads, long num, const long *lres);

void OMPimp_double_BitonicSort(int nthreads, long num, double *res, long *idx);
void flip_double_sign_omp(int nthreads, long num, double *res);
double max_double_array_omp(int nthreads, long num, const double *res);

void OMPimp_float_BitonicSort(int nthreads, long num, float *res, long *idx);
void flip_float_sign_omp(int nthreads, long num, float *res);
float max_float_array_omp(int nthreads, long num, const float *res);

#endif  //  BITONIC_SORT_OMP_
