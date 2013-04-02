
/* jacobian_trilinear_c.h */

#ifndef JACOBIAN_TRILINEAR_C_
#define JACOBIAN_TRILINEAR_C_

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* prototypes */

void find_inverse_33matrix(double coff_matrix[3][3], double inv_matrix[3][3]);
int jacobi_trilinear(double detj[2][2][2], double pnq[2][2][8], double pne[2][2][8], 
	   double pnt[2][2][8], double pnx[2][2][2][8], double pny[2][2][2][8],
	   double pnz[2][2][2][8], 
	   double x0, double x1, double x2, double x3, double x4, double x5, 
	   double x6, double x7,
	   double y0, double y1, double y2, double y3, double y4, double y5, 
	   double y6, double y7, 
	   double z0, double z1, double z2, double z3, double z4, double z5, 
	   double z6, double z7);

#endif
