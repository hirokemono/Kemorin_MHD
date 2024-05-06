/*
 *  array_for_sorting_test.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 05/06/24.
 *
 */

#include "array_for_sorting_test.h"

/** compare for qsort **/
int desc( const void *a, const void *b ){
    int* arg1 = (int *)a;
    int* arg2 = (int *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 > *arg2 ){ return -1;};
    return 1;
}
int asc( const void *a, const void *b ){
    int* arg1 = (int *)a;
    int* arg2 = (int *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 < *arg2 ){ return -1;};
    return 1;
}

int desc_dbl( const void *a, const void *b ){
    double* arg1 = (double *)a;
    double* arg2 = (double *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 > *arg2 ){ return -1;};
    return 1;
}
int asc_dbl( const void *a, const void *b ){
    double* arg1 = (double *)a;
    double* arg2 = (double *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 < *arg2 ){ return -1;};
    return 1;
}

int desc_long( const void *a, const void *b ){
    long* arg1 = (long *)a;
    long* arg2 = (long *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 > *arg2 ){ return -1;};
    return 1;
}
int asc_long( const void *a, const void *b ){
    long* arg1 = (long *)a;
    long* arg2 = (long *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 < *arg2 ){ return -1;};
    return 1;
}

/** procedure to set randum values to sort **/
void init_Int_Array(long num, long nArray, int *iorg) {
    long i;
    for (i=0;i<num;i++) {
        iorg[i] = rand(); // (N - i);
//        iorg[i] = rand() % num; // (N - i);
    }
    int imax = max_int_array(num, iorg);
    for (i=num;i<nArray;i++) {iorg[i] = imax + 1;}
}

void init_Long_Array(long num, long nArray, long *lorg) {
    long i;
    for (i=0;i<num;i++) {
        lorg[i] = rand(); // (N - i);
//        lorg[i] = rand() % num; // (N - i);
    }
    long lmax = max_long_array(num, lorg);
    for (i=num;i<nArray;i++) {lorg[i] = lmax + 1;}
}

void init_Double_Array(long num, long nArray, double *dorg) {
    long i;
    for (i=0;i<num;i++) {
        dorg[i] = 1.0 + (double) (rand() % num);
        dorg[i] = 1.0 / dorg[i];
    }
    double dmax = max_double_array(num, dorg);
    for (i=num;i<nArray;i++) {dorg[i] = dmax + 1.0;}
}

void init_Float_Array(long num, long nArray, float *org) {
    long i;
    for (i=0;i<num;i++) {
        org[i] = 1.0 + (double) (rand() % num);
        org[i] = 1.0 / org[i];
    }
    float rmax = max_float_array(num, org);
    for (i=num;i<nArray;i++) {org[i] = rmax + 1;}
}


/** procedure to copy randum values to sort **/
void copy_Int_Array(const long num, const long nArray, 
                    const int *iorg, int *ires, long *idx){
    long i;
    for (i=0;i<nArray;i++) {ires[i] = iorg[i];}
    for (i=0;i<num;i++) {idx[i] = i;}
    for (i=num;i<nArray;i++) {idx[i] = -1;}
}

void copy_Long_Array(const long num, const long nArray,
                     const long *lorg, long *lres, long *idx){
    long i;
    for (i=0;i<nArray;i++) {lres[i] = lorg[i];}
    for (i=0;i<num;i++) {idx[i] = i;}
    for (i=num;i<nArray;i++) {idx[i] = -1;}
}

void copy_Double_Array(const long num, long const nArray, 
                       const double *dorg, double *dres, long *idx){
    long i;
    for (i=0;i<nArray;i++) {dres[i] = dorg[i];}
    for (i=0;i<num;i++) {idx[i] = i;}
    for (i=num;i<nArray;i++) {idx[i] = -1;}
}

void copy_Float_Array(const long num, const long nArray, 
                      const float *org, float *res, long *idx){
    long i;
    for (i=0;i<nArray;i++) {res[i] = org[i];}
    for (i=0;i<num;i++) {idx[i] = i;}
    for (i=num;i<nArray;i++) {idx[i] = -1;}
}

/** procedure to verify sort results **/
void check_sorted_Int(long num, const int *ires){
  int pass = 1;
  long i;
    for (i=1;i<num;i++) {
    pass &= (ires[i-1] <= ires[i]);
  }
    
  printf(" TEST %s\n",(pass) ? "PASSed" : "FAILed");
}

void check_sorted_Double(long num, const double *dres){
  int pass = 1;
  long i;
    for (i=1;i<num;i++) {
    pass &= (dres[i-1] <= dres[i]);
  }
    
  printf(" TEST %s\n",(pass) ? "PASSed" : "FAILed");
}

void check_sorted_Float(long num, const float *res){
  int pass = 1;
  long i;
    for (i=1;i<num;i++) {
    pass &= (res[i-1] <= res[i]);
  }
    
  printf(" TEST %s\n",(pass) ? "PASSed" : "FAILed");
}

/** Output sorted lists for short array **/
void print_sorted_Int(long num, long nArray, const int *iorg,
                      const int *ires, const long *idx) {
    int i;
    if(num > 33) return;
    printf("Length of sorting and array: %ld %ld \n", num, nArray);
    printf("Index original_value: original_index, sorted_value\n");
    for (i = 0; i < num; i++) {
        printf("%d %d: %d %d \n", i, iorg[i], (int) idx[i], ires[i]);
    }
    printf("\n");
    for (i = num; i < nArray; i++) {
        printf("%d %d: %d %d \n", i, iorg[i], (int) idx[i], ires[i]);
    }
}

void print_sorted_Long(long num, long nArray, const long *iorg,
                       const long *ires, const long *idx) {
    int i;
    if(num > 33) return;
    printf("Length of sorting and array: %ld %ld \n", num, nArray);
    printf("Index original_value: original_index, sorted_value\n");
    for (i = 0; i < num; i++) {
        printf("%d %ld: %ld %ld \n", i, iorg[i], idx[i], ires[i]);
    }
    printf("\n");
    for (i = num; i < nArray; i++) {
        printf("%d %ld: %ld %ld \n", i, iorg[i],  idx[i], ires[i]);
    }
}

void print_sorted_Double(long num, long nArray, const double *dorg, 
                         const double *dres, const long *idx) {
    int i;
    if(num > 33) return;
    printf("Length of sorting and array: %ld %ld \n", num, nArray);
    printf("Index original_value: original_index, sorted_value\n");
    for (i = 0; i < num; i++) {
        printf("%d %lf: %d %lf \n", i, dorg[i], (int) idx[i], dres[i]);
    }
    printf("\n");
    for (i = num; i < nArray; i++) {
        printf("%d %lf: %d %lf \n", i, dorg[i], (int) idx[i], dres[i]);
    }
}

void print_sorted_Float(long num, long nArray, const float *org,
                        const float *res, const long *idx){
    int i;
    if(num > 33) return;
    printf("Length of sorting and array: %ld %ld \n", num, nArray);
    printf("Index original_value: original_index, sorted_value\n");
    for (i = 0; i < num; i++) {
        printf("%d %f: %d %f \n", i, org[i], (int) idx[i], res[i]);
    }
    printf("\n");
    for (i = num; i < nArray; i++) {
        printf("%d %f: %d %f \n", i, org[i], (int) idx[i], res[i]);
    }
};
