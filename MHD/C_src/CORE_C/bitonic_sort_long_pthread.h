/*
 bitonic_sort_long_pthread.h

 This file contains the bitonic sort using pthread
         bitonicsort_Long_Pthread(int nthreads, long num, long *lres, long *idx);

 The bitonic sort is also known as Batcher Sort. 
 For a reference of the algorithm, see the article titled 
 Sorting networks and their applications by K. E. Batcher in 1968 


 The following codes take references to the codes avaiable at 

 http://www.cag.lcs.mit.edu/streamit/results/bitonic/code/c/bitonic.c

 http://www.tools-of-computing.com/tc/CS/Sorts/bitonic_sort.htm

 http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/bitonic/bitonicen.htm 
 */

#ifndef BITONIC_SORT_LONG_PTHREAD_
#define BITONIC_SORT_LONG_PTHREAD_

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "quicksort_c.h"
#include "bitonic_sort_c.h"

/* prototypes */

long  max_Long_Array_pthreads(int nthreads, long num, long *lres);
void flip_sign_Long_pthreads(int nthreads, long num, long *lres);
void bitonicsort_Long_Pthread(int nthreads, long num, long *lres, long *idx);

#endif // BITONIC_SORT_LONG_PTHREAD_
