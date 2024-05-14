/*
 bitonic_sort_c.h

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

#ifndef BITONIC_SORT_C_
#define BITONIC_SORT_C_

#include <math.h>

#include "quicksort_c.h"

#define ASCENDING  1
#define DESCENDING 0

void bitonic_Int_Merge(long lo, long hi, int iflag_ascend,
                       int *ires, long *idx);
void bitonic_Long_Merge(long lo, long hi, int iflag_ascend,
                        long *lres, long *idx);
void bitonic_Double_Merge(long lo, long hi, int iflag_ascend,
                          double *dres, long *idx);
void bitonic_Float_Merge(long lo, long hi, int iflag_ascend,
                         float *res, long *idx);

void rec_Int_BitonicSort(long lo, long hi, int iflag_ascend,
                         int *ires, long *idx);
void rec_Long_BitonicSort(long lo, long hi, int iflag_ascend,
                          long *lres, long *idx);
void rec_Double_BitonicSort(long lo, long hi, int iflag_ascend,
                            double *dres, long *idx);
void rec_Float_BitonicSort(long lo, long hi, int iflag_ascend,
                           float *res, long *idx);


void bitonicsort_rec_Int(long num, int *ires, long *idx);
void bitonicsort_rec_Long(long num, long *lres, long *idx);
void bitonicsort_rec_Double(long num, double *dres, long *idx);
void bitonicsort_rec_Float(long num, float *res, long *idx);

void BitonicSort_imp_Int(long num, int *ires, long *idx);
void BitonicSort_imp_Long(long num, long *lres, long *idx);
void BitonicSort_imp_Double(long num, double *dres, long *idx);
void BitonicSort_imp_Float(long num, float *res, long *idx);

#endif // BITONIC_SORT_C_
