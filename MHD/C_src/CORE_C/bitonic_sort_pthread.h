/*
 bitonic_sort_pthread.h

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

#ifndef BITONIC_SORT_PTHREAD_
#define BITONIC_SORT_PTHREAD_

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "quicksort_c.h"
#include "bitonic_sort_c.h"

/* prototypes */

double max_Double_Array_pthreads(int nthreads, long num, double *dres);
void flip_sign_Double_pthreads(int nthreads, long num, double *dres);
void bitonicsort_Double_Pthread(int nthreads, long num, double *dres, long *idx);

#endif // BITONIC_SORT_PTHREAD_
