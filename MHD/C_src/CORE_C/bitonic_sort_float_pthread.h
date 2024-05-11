/*
 bitonic_sort_float_pthread.h

 This file contains the bitonic sort using pthread
         bitonicsort_Float_Pthread(int nthreads, long num, float *res, long *idx);

 The bitonic sort is also known as Batcher Sort. 
 For a reference of the algorithm, see the article titled 
 Sorting networks and their applications by K. E. Batcher in 1968 


 The following codes take references to the codes avaiable at 

 http://www.cag.lcs.mit.edu/streamit/results/bitonic/code/c/bitonic.c

 http://www.tools-of-computing.com/tc/CS/Sorts/bitonic_sort.htm

 http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/bitonic/bitonicen.htm 
 */

#ifndef BITONIC_SORT_FLOAT_PTHREAD_
#define BITONIC_SORT_FLOAT_PTHREAD_

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "quicksort_c.h"
#include "bitonic_sort_c.h"

/* prototypes */

float  max_Float_Array_pthreads(int nthreads, long num, float *res);
void flip_sign_Float_pthreads(int nthreads, long num, float *res);
void bitonicsort_Float_Pthread(int nthreads, long num, float *res, long *idx);

#endif // BITONIC_SORT_FLOAT_PTHREAD_
