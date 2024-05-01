
/*   quicksort_c.c */

#include <stdio.h>
#include "quicksort_c.h"

/*
#define maxsize 600
int A[maxsize];
int list[maxsize];
*/

void exchange(int *i, int *j){
    int t;
    t = *i;
    *i = *j;
    *j = t;
    return;
}

void exchange_long(long *i, long *j){
    long t;
    t = *i;
    *i = *j;
    *j = t;
    return;
}

void exchange_float(float *x, float *y){
    float t;
    t = *x;
    *x = *y;
    *y = t;
    return;
}

void exchange_double(double *x, double *y){
    double t;
    t = *x;
    *x = *y;
    *y = t;
    return;
}


void quicksort_int_c(int *ivec, long *list, long lo, long hi){
    int pivot;
    long i, j;
    
    if(lo == hi) return;
    i=lo;
    j=hi;
    pivot= ivec[(lo+hi)/2];
    /* Split the array into two parts */
    do {
        while (ivec[i] < pivot) i++;
        while (ivec[j] > pivot) j--;
        if (i<=j) {
            exchange(&ivec[i], &ivec[j]);
            exchange_long(&list[i], &list[j]);
            i++;
            j--;
        }
    } while (i<=j);
    
    if (lo < j) quicksort_int_c(ivec, list, lo, j);
    if (i < hi) quicksort_int_c(ivec, list, i, hi);
    return;
};

void quicksort_int8_c(long *ivec, long *list, long lo, long hi) {
    long i, j, pivot;
	
	
	if(lo == hi) return; 
	i=lo; 
	j=hi;
	pivot= ivec[(lo+hi)/2]; 
	
	/* Split the array into two parts */
	do {    
		while (ivec[i] < pivot) i++; 
		while (ivec[j] > pivot) j--;
		if (i<=j) {
            exchange_long(&ivec[i], &ivec[j]);
            exchange_long(&list[i], &list[j]);
			i++;
			j--;
		}
	} while (i<=j);
	
	if (lo < j) quicksort_int8_c(ivec, list, lo, j);
	if (i < hi) quicksort_int8_c(ivec, list, i, hi);
	return;
};

void quicksort_double_c(double *dvec, long *list, long lo, long hi){
    long i, j;
	double pivot;
	
	if(lo >= hi) return; 
	i=lo; 
	j=hi;
	pivot= dvec[(lo+hi)/2]; 
	
	/* Split the array into two parts */
	do {    
		while (dvec[i] < pivot) i++; 
		while (dvec[j] > pivot) j--;
		if (i<=j) {
            exchange_double(&dvec[i], &dvec[j]);
            exchange_long(&list[i], &list[j]);
			i++;
			j--;
		}
	} while (i<=j);
	
	if (lo < j) quicksort_double_c(dvec, list, lo, j);
	if (i < hi) quicksort_double_c(dvec, list, i, hi);
	return;
}

void quicksort_real_c(float *rvec, long *list, long lo, long hi){
    long i, j;
	float pivot;
	
	if(lo == hi) return; 
	i=lo; 
	j=hi;
	pivot= rvec[(lo+hi)/2]; 
	
	/* Split the array into two parts */
	do {    
		while (rvec[i] < pivot) i++; 
		while (rvec[j] > pivot) j--;
        if (i<=j) {
            exchange_float(&rvec[i], &rvec[j]);
            exchange_long(&list[i], &list[j]);
			i++;
			j--;
		}
	} while (i<=j);
	
	if (lo < j) quicksort_real_c(rvec, list, lo, j);
	if (i < hi) quicksort_real_c(rvec, list, i, hi);
	return;
}



int max_int_array(long num, const int *ires){
    long i;
    int  imax;
    imax = ires[0];
    for (i=1;i<num; i++) {
        if(ires[i] > imax){imax = ires[i];};
    };
  return imax;
}

long max_long_array(long num, const long *lres){
    long i, lmax;
    lmax = lres[0];
    for (i=1;i<num; i++) {
        if(lres[i] > lmax){lmax = lres[i];};
    };
  return lmax;
}

double max_double_array(long num, const double *res){
    long i;
    double dmax;
    dmax = res[0];
    for (i=1;i<num; i++) {
        if(res[i] > dmax){dmax = res[i];};
    };
  return dmax;
}

float max_float_array(long num, const float *res){
    long i;
    float rmax;
    rmax = res[0];
    for (i=1;i<num; i++) {
        if(res[i] > rmax){rmax = res[i];};
    };
  return rmax;
}


void flip_int_sign(long num, int *ires){
    long i;
    for (i=0;i<num; i++) {ires[i] = -ires[i];};
    return;
}

void flip_long_sign(long num, long *lres){
    long i;
    for (i=0;i<num; i++) {lres[i] = -lres[i];};
    return;
}

void flip_double_sign(long num, double *res){
    long i;
    for (i=0;i<num; i++) {res[i] = -res[i];};
    return;
}

void flip_float_sign(long num, float *res){
    long i;
    for (i=0;i<num; i++) {res[i] = -res[i];};
    return;
}
