
/* quicksort_c.h */

#ifndef QUICKSORT_C_
#define QUICKSORT_C_

/* prototypes */

void exchange(int *i, int *j);
void exchange_long(long *i, long *j);
void exchange_float(float *x, float *y);
void exchange_double(double *x, double *y);

void quicksort_int_c(int *ivec, long *list, long lo, long hi);
void quicksort_int8_c(long *ivec, long *list, long lo, long hi);
void quicksort_double_c(double *dvec, long *list, long lo, long hi);
void quicksort_real_c(float *rvec, long *list, long lo, long hi);

int max_int_array(long num, const int *ires);
long max_long_array(long num, const long *lres);
double max_double_array(long num, const double *res);
float max_float_array(long num, const float *res);

void flip_int_sign(long num, int *ires);
void flip_long_sign(long num, long *lres);
void flip_double_sign(long num, double *res);
void flip_float_sign(long num, float *res);
#endif


