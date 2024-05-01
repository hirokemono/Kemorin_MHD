/*
 bitonic_sort_omp.c 

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

/* 
------- ---------------------- 
   Nikos Pitsianis, Duke CS 
-----------------------------

    modified by Antotsiou Dafni and Sourgkounis Theodosis

*/

#include "bitonic_sort_omp.h"

void OMPimp_int_BitonicSort(int nthreads, long num, int *ires, long *idx){
    long i, j, ij;
    long k=0;
    
    omp_set_num_threads(nthreads);//"num" is the number of threads - arg[2]; 
#pragma omp parallel private(k,j)
    for(k=2;k<=num;k *= 2) {
        for (j=k>>1;j>0;j=j>>1) {
#pragma omp for private(i,ij)
            for(i=0;i<num;i++) {
                ij=i^j;
                if ((ij)>i) {
                    if ((i&k)==0 && ires[i] > ires[ij]){
                        exchange(&ires[i],&ires[ij]);
                        exchange_long(&idx[i],&idx[ij]);
                    }
                    if ((i&k)!=0 && ires[i] < ires[ij]){
                        exchange(&ires[i],&ires[ij]);
                        exchange_long(&idx[i],&idx[ij]);
                    }
                }
            }
        }
    }
} 

void OMPimp_long_BitonicSort(int nthreads, long num, long *lres, long *idx){
    long i, j, ij;
    long k=0;
    
    omp_set_num_threads(nthreads);//"num" is the number of threads - arg[2]; 
#pragma omp parallel private(k,j)
    for(k=2;k<=num;k *= 2) {
        for (j=k>>1;j>0;j=j>>1) {
#pragma omp for private(i,ij)
            for(i=0;i<num;i++) {
                ij=i^j;
                if ((ij)>i) {
                    if ((i&k)==0 && lres[i] > lres[ij]){
                        exchange_long(&lres[i],&lres[ij]);
                        exchange_long(&idx[i],&idx[ij]);
                    }
                    if ((i&k)!=0 && lres[i] < lres[ij]){
                        exchange_long(&lres[i],&lres[ij]);
                        exchange_long(&idx[i],&idx[ij]);
                    }
                }
            }
        }
    }
} 

void OMPimp_double_BitonicSort(int nthreads, long num, double *res, long *idx){
    long i, j, ij;
    long k=0;
    
    omp_set_num_threads(nthreads);//"num" is the number of threads - arg[2]; 
#pragma omp parallel private(k,j)
    for(k=2;k<=num;k *= 2) {
        for (j=k>>1;j>0;j=j>>1) {
#pragma omp for private(i,ij)
            for(i=0;i<num;i++) {
                ij=i^j;
                if ((ij)>i) {
                    if ((i&k)==0 && res[i] > res[ij]){
                        exchange_double(&res[i],&res[ij]);
                        exchange_long(&idx[i],&idx[ij]);
                    }
                    if ((i&k)!=0 && res[i] < res[ij]){
                        exchange_double(&res[i],&res[ij]);
                        exchange_long(&idx[i],&idx[ij]);
                    }
                }
            }
        }
    }
} 

void OMPimp_float_BitonicSort(int nthreads, long num, float *res, long *idx){
    long i, j, ij;
    long k=0;
    
    omp_set_num_threads(nthreads);//"num" is the number of threads - arg[2]; 
#pragma omp parallel private(k,j)
    for(k=2;k<=num;k *= 2) {
        for (j=k>>1;j>0;j=j>>1) {
#pragma omp for private(i,ij)
            for(i=0;i<num;i++) {
                ij=i^j;
                if ((ij)>i) {
                    if ((i&k)==0 && res[i] > res[ij]){
                        exchange_float(&res[i],&res[ij]);
                        exchange_long(&idx[i],&idx[ij]);
                    }
                    if ((i&k)!=0 && res[i] < res[ij]){
                        exchange_float(&res[i],&res[ij]);
                        exchange_long(&idx[i],&idx[ij]);
                    }
                }
            }
        }
    }
} 


int max_int_array_omp(int nthreads, long num, const int *ires){
    omp_set_num_threads(nthreads);
    long i;
    int  imax;
    imax = ires[0];
#pragma omp parallel for private(i) reduction(max:imax)
    for (i=0;i<num; i++) {
        if(ires[i] > imax){imax = ires[i];};
    };
  return imax;
}

long max_long_array_omp(int nthreads, long num, const long *lres){
    omp_set_num_threads(nthreads);
    long i, lmax;
    lmax = lres[0];
#pragma omp parallel for private(i) reduction(max:lmax)
    for (i=0;i<num; i++) {
        if(lres[i] > lmax){lmax = lres[i];};
    };
  return lmax;
}

double max_double_array_omp(int nthreads, long num, const double *res){
    omp_set_num_threads(nthreads);
    long i;
    double dmax;
    dmax = res[0];
#pragma omp parallel for private(i) reduction(max:dmax)
    for (i=0;i<num; i++) {
        if(res[i] > dmax){dmax = res[i];};
    };
  return dmax;
}

float max_float_array_omp(int nthreads, long num, const float *res){
    omp_set_num_threads(nthreads);
    long i;
    float rmax;
    rmax = res[0];
#pragma omp parallel for private(i) reduction(max:rmax)
    for (i=0;i<num; i++) {
        if(res[i] > rmax){rmax = res[i];};
    };
  return rmax;
}


void flip_int_sign_omp(int nthreads, long num, int *ires){
    omp_set_num_threads(nthreads);
    long i;
#pragma omp parallel for private(i)
    for (i=0;i<num; i++) {ires[i] = -ires[i];};
    return;
}

void flip_long_sign_omp(int nthreads, long num, long *lres){
    omp_set_num_threads(nthreads);
    long i;
#pragma omp parallel for private(i)
    for (i=0;i<num; i++) {lres[i] = -lres[i];};
    return;
}

void flip_double_sign_omp(int nthreads, long num, double *res){
    omp_set_num_threads(nthreads);
    long i;
#pragma omp parallel for private(i)
    for (i=0;i<num; i++) {res[i] = -res[i];};
    return;
}

void flip_float_sign_omp(int nthreads, long num, float *res){
    omp_set_num_threads(nthreads);
    long i;
#pragma omp parallel for private(i)
    for (i=0;i<num; i++) {res[i] = -res[i];};
    return;
}
