/*
 bitonic_sort_c.c

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

#include "bitonic_sort_c.h"

/** Procedure bitonicIntMerge() 
   It recursively sorts a bitonic sequence in ascending order, 
   if iflag_ascend = ASCENDING, and in descending order otherwise. 
   The sequence to be sorted starts at index position lo,
   the parameter cbt is the number of elements to be sorted. 
 **/
void bitonic_Int_Merge(long lo, long hi, int iflag_ascend,
                       int *ires, long *idx){
    long i;
    if (hi>1) {
        long k = hi/2;
        for (i=lo; i<lo+k; i++){
            if (iflag_ascend == (ires[i]>ires[i+k])){
                exchange(&ires[i], &ires[i+k]);
                exchange_long(&idx[i], &idx[i+k]);
            };
        };
        
        bitonic_Int_Merge(lo,   k, iflag_ascend, ires, idx);
        bitonic_Int_Merge(lo+k, k, iflag_ascend, ires, idx);
    }
}

void bitonic_Long_Merge(long lo, long hi, int iflag_ascend,
                       long *lres, long *idx){
    long i;
    if (hi>1) {
        long k = hi/2;
        for (i=lo; i<lo+k; i++){
            if (iflag_ascend == (lres[i]>lres[i+k])){
                exchange_long(&lres[i], &lres[i+k]);
                exchange_long(&idx[i], &idx[i+k]);
            };
        };
        
        bitonic_Long_Merge(lo,   k, iflag_ascend, lres, idx);
        bitonic_Long_Merge(lo+k, k, iflag_ascend, lres, idx);
    }
}

void bitonic_Double_Merge(long lo, long hi, int iflag_ascend,
                          double *dres, long *idx){
    long i;
    if (hi>1) {
        long k = hi/2;
        for (i=lo; i<lo+k; i++){
            if (iflag_ascend == (dres[i]>dres[i+k])){
                exchange_double(&dres[i], &dres[i+k]);
                exchange_long(&idx[i], &idx[i+k]);
            };
        };
        
        bitonic_Double_Merge(lo,   k, iflag_ascend, dres, idx);
        bitonic_Double_Merge(lo+k, k, iflag_ascend, dres, idx);
    }
}

void bitonic_Float_Merge(long lo, long hi, int iflag_ascend,
                         float *res, long *idx){
    long i;
    if (hi>1) {
        long k = hi/2;
        for (i=lo; i<lo+k; i++){
            if (iflag_ascend == (res[i]>res[i+k])){
                exchange_float(&res[i], &res[i+k]);
                exchange_long(&idx[i], &idx[i+k]);
            };
        };
        
        bitonic_Float_Merge(lo,   k, iflag_ascend, res, idx);
        bitonic_Float_Merge(lo+k, k, iflag_ascend, res, idx);
    }
}


/** function rec_int_BitonicSort() 
    first produces a bitonic sequence by recursively sorting 
    its two halves in opposite sorting orders, and then
    calls bitonic_Int_Merge to make them in the same order 
 **/
void rec_Int_BitonicSort(long lo, long hi, int iflag_ascend,
                         int *ires, long *idx) {
  if (hi>1) {
    long k = hi/2;
    rec_Int_BitonicSort(lo, k,   ASCENDING,  ires, idx);
    rec_Int_BitonicSort(lo+k, k, DESCENDING, ires, idx);
    bitonic_Int_Merge(lo, hi, iflag_ascend, ires, idx);
  }
}

void rec_Long_BitonicSort(long lo, long hi, int iflag_ascend,
                          long *lres, long *idx) {
  if (hi>1) {
    long k = hi/2;
    rec_Long_BitonicSort(lo, k,   ASCENDING,  lres, idx);
    rec_Long_BitonicSort(lo+k, k, DESCENDING, lres, idx);
    bitonic_Long_Merge(lo, hi, iflag_ascend, lres, idx);
  }
}

void rec_Double_BitonicSort(long lo, long hi, int iflag_ascend,
                            double *dres, long *idx) {
  if (hi>1) {
    long k = hi/2;
    rec_Double_BitonicSort(lo, k, ASCENDING,    dres, idx);
    rec_Double_BitonicSort(lo+k, k, DESCENDING, dres, idx);
    bitonic_Double_Merge(lo, hi, iflag_ascend, dres, idx);
  }
}

void rec_Float_BitonicSort(long lo, long hi, int iflag_ascend,
                           float *res, long *idx) {
  if (hi>1) {
    long k = hi/2;
    rec_Float_BitonicSort(lo, k,   ASCENDING, res, idx);
    rec_Float_BitonicSort(lo+k, k, DESCENDING, res, idx);
    bitonic_Float_Merge(lo, hi, iflag_ascend, res, idx);
  }
}


/** function bitonicsort_rec_Int() 
   Caller of rec_Int_BitonicSort for sorting the entire array of length N 
   in ASCENDING order
 **/
void bitonicsort_rec_Int(long num, int *ires, long *idx) {
  rec_Int_BitonicSort(0, num, ASCENDING, ires, idx);
}

void bitonicsort_rec_Long(long num, long *lres, long *idx) {
  rec_Long_BitonicSort(0, num, ASCENDING, lres, idx);
}

void bitonicsort_rec_Double(long num, double *dres, long *idx) {
  rec_Double_BitonicSort(0, num, ASCENDING, dres, idx);
}

void bitonicsort_rec_Float(long num, float *res, long *idx) {
  rec_Float_BitonicSort(0, num, ASCENDING, res, idx);
}


/*
  imperative version of bitonic sort
*/
void BitonicSort_imp_Int(long num, int *ires, long *idx){
    long i, j, k, ij;
    
    for (k=2; k<=num; k=2*k) {
        for (j=k>>1; j>0; j=j>>1) {
            for (i=0; i<num; i++) {
                ij=i^j;
                if ((ij)>i) {
                    if ((i&k)==0 && ires[i] > ires[ij]){
                        exchange(&ires[i], &ires[ij]);
                        exchange_long(&idx[i], &idx[ij]);
                    };
                    if ((i&k)!=0 && ires[i] < ires[ij]){
                        exchange(&ires[i], &ires[ij]);
                        exchange_long(&idx[i], &idx[ij]);
                    };
                }
            }
        }
    }
    return;
}

void BitonicSort_imp_Long(long num, long *lres, long *idx){
    long i, j, k, ij;
    
    for (k=2; k<=num; k=2*k) {
        for (j=k>>1; j>0; j=j>>1) {
            for (i=0; i<num; i++) {
                ij=i^j;
                if ((ij)>i) {
                    if ((i&k)==0 && lres[i] > lres[ij]){
                        exchange_long(&lres[i], &lres[ij]);
                        exchange_long(&idx[i], &idx[ij]);
                    };
                    if ((i&k)!=0 && lres[i] < lres[ij]){
                        exchange_long(&lres[i], &lres[ij]);
                        exchange_long(&idx[i], &idx[ij]);
                    };
                }
            }
        }
    }
    return;
}

void BitonicSort_imp_Double(long num, double *dres, long *idx){
    long i, j, k, ij;
    
    for (k=2; k<=num; k=2*k) {
        for (j=k>>1; j>0; j=j>>1) {
            for (i=0; i<num; i++) {
                ij=i^j;
                if ((ij)>i) {
                    if ((i&k)==0 && dres[i] > dres[ij]){
                        exchange_double(&dres[i], &dres[ij]);
                        exchange_long(&idx[i], &idx[ij]);
                    };
                    if ((i&k)!=0 && dres[i] < dres[ij]){
                        exchange_double(&dres[i], &dres[ij]);
                        exchange_long(&idx[i], &idx[ij]);
                    };
                }
            }
        }
    }
    return;
}

void BitonicSort_imp_Float(long num, float *res, long *idx){
    long i, j, k, ij;
    
    for (k=2; k<=num; k=2*k) {
        for (j=k>>1; j>0; j=j>>1) {
            for (i=0; i<num; i++) {
                ij=i^j;
                if ((ij)>i) {
                    if ((i&k)==0 && res[i] > res[ij]){
                        exchange_float(&res[i], &res[ij]);
                        exchange_long(&idx[i], &idx[ij]);
                    };
                    if ((i&k)!=0 && res[i] < res[ij]){
                        exchange_float(&res[i], &res[ij]);
                        exchange_long(&idx[i], &idx[ij]);
                    };
                }
            }
        }
    }
    return;
}
