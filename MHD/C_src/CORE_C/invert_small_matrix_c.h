
/* invert_small_matrix_c.h */

#include <math.h>
#include <stdio.h>

double cal_det_22_matrix_c(const double *a);
double cal_det_33_matrix_c(const double *a);
double cal_det_44_matrix_c(const double *a);
double cal_det_nn_matrix_c(int nsize, const double *a);

int cal_inverse_22_matrix_c(const double *a, double *a_inv);
int cal_inverse_33_matrix_c(const double *a, double *a_inv);
int cal_inverse_44_matrix_c(const double *a, double *a_inv);
int cal_inverse_nn_matrix_c(int nsize, const double *a, double *a_inv);
