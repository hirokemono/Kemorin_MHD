
/* jacobian_trilinear_c.c */

#include "jacobian_trilinear_c.h"

void find_inverse_33matrix(double coff_matrix[3][3], double inv_matrix[3][3])
{
	int i, j;
	double a[3][3], norm_a, aa[3][3], a_norm;

    for(i=0;i<3;i++)
		for(j=0;j<3;j++)
			a[i][j]=coff_matrix[i][j];
	norm_a=a[0][0]*a[1][1]*a[2][2]+a[0][1]*a[1][2]*a[2][0]+a[0][2]*a[1][0]*a[2][1]-a[0][2]*a[1][1]*a[2][0]
		-a[0][1]*a[1][0]*a[2][2]-a[0][0]*a[1][2]*a[2][1];
	if(fabs(norm_a)<1.0E-7) {
		fprintf(stderr, "There is something wrong with transform matrix, invers =0\n");
		exit(0);
	} else {
		a_norm = 1.0 / norm_a;
	}
	aa[0][0]=  a[1][1]*a[2][2]-a[1][2]*a[2][1];
	aa[0][1]=-(a[0][1]*a[2][2]-a[2][1]*a[0][2]);
	aa[0][2]=  a[0][1]*a[1][2]-a[1][1]*a[0][2];
	aa[1][0]=-(a[1][0]*a[2][2]-a[2][0]*a[1][2]);
	aa[1][1]=  a[0][0]*a[2][2]-a[2][0]*a[0][2];
	aa[1][2]=-(a[0][0]*a[1][2]-a[1][0]*a[0][2]);
	aa[2][0]=  a[1][0]*a[2][1]-a[2][0]*a[1][1];
	aa[2][1]=-(a[0][0]*a[2][1]-a[2][0]*a[0][1]);
	aa[2][2]=  a[0][0]*a[1][1]-a[1][0]*a[0][1];
	for(i=0;i<3;i++)
		for(j=0;j<3;j++) 
			inv_matrix[i][j]=aa[i][j]*a_norm;
	return;
}


/* jacobi */
int jacobi_trilinear(double detj[2][2][2], double pnq[2][2][8], double pne[2][2][8], 
	   double pnt[2][2][8], double pnx[2][2][2][8], double pny[2][2][2][8],
	   double pnz[2][2][2][8], 
	   double x0, double x1, double x2, double x3, double x4, double x5, 
	   double x6, double x7,
	   double y0, double y1, double y2, double y3, double y4, double y5, 
	   double y6, double y7, 
	   double z0, double z1, double z2, double z3, double z4, double z5, 
	   double z6, double z7)
{

  /* 
     calculates JACOBIAN & INVERSE JACOBIAN
     dNi/dx, dNi/dy & dNi/dz        
     */

  /***
    implicit REAL*8 (A-H,O-Z)
    dimension DETJ[2,2,2]
    dimension pnq[2,2,8],   pne[2,2,8],   pnt[2,2,8]
    dimension PNX[2,2,2,8], pny[2,2,2,8], pnz[2,2,2,8]
    ***/

  int		ip, jp, kp;
  double	dxdq, dydq, dzdq, dxde, dyde, dzde, dxdt, dydt, dzdt;
  double	coef;
  double	a11, a12, a13, a21, a22, a23, a31, a32, a33;

  for (kp = 0; kp < 2; kp++) {
    for (jp = 0; jp < 2; jp++) {
      for (ip = 0; ip < 2; ip++) {
        pnx[ip][jp][kp][0] = 0.0;
        pnx[ip][jp][kp][1] = 0.0;
        pnx[ip][jp][kp][2] = 0.0;
        pnx[ip][jp][kp][3] = 0.0;
        pnx[ip][jp][kp][4] = 0.0;
        pnx[ip][jp][kp][5] = 0.0;
        pnx[ip][jp][kp][6] = 0.0;
        pnx[ip][jp][kp][7] = 0.0;

        pny[ip][jp][kp][0] = 0.0;
        pny[ip][jp][kp][1] = 0.0;
        pny[ip][jp][kp][2] = 0.0;
        pny[ip][jp][kp][3] = 0.0;
        pny[ip][jp][kp][4] = 0.0;
        pny[ip][jp][kp][5] = 0.0;
        pny[ip][jp][kp][6] = 0.0;
        pny[ip][jp][kp][7] = 0.0;

        pnz[ip][jp][kp][0] = 0.0;
        pnz[ip][jp][kp][1] = 0.0;
        pnz[ip][jp][kp][2] = 0.0;
        pnz[ip][jp][kp][3] = 0.0;
        pnz[ip][jp][kp][4] = 0.0;
        pnz[ip][jp][kp][5] = 0.0;
        pnz[ip][jp][kp][6] = 0.0;
        pnz[ip][jp][kp][7] = 0.0;

	/* DETERMINANT of the JACOBIAN */
        dxdq = + pnq[jp][kp][0] * x0 + pnq[jp][kp][1] * x1
	  + pnq[jp][kp][2] * x2 + pnq[jp][kp][3] * x3
	  + pnq[jp][kp][4] * x4 + pnq[jp][kp][5] * x5
	  + pnq[jp][kp][6] * x6 + pnq[jp][kp][7] * x7;
        dydq = + pnq[jp][kp][0] * y0 + pnq[jp][kp][1] * y1
	  + pnq[jp][kp][2] * y2 + pnq[jp][kp][3] * y3
	  + pnq[jp][kp][4] * y4 + pnq[jp][kp][5] * y5
	  + pnq[jp][kp][6] * y6 + pnq[jp][kp][7] * y7;
        dzdq = + pnq[jp][kp][0] * z0 + pnq[jp][kp][1] * z1
	  + pnq[jp][kp][2] * z2 + pnq[jp][kp][3] * z3
	  + pnq[jp][kp][4] * z4 + pnq[jp][kp][5] * z5
	  + pnq[jp][kp][6] * z6 + pnq[jp][kp][7] * z7;
	dxde = + pne[ip][kp][0] * x0 + pne[ip][kp][1] * x1
	 + pne[ip][kp][2] * x2 + pne[ip][kp][3] * x3
	 + pne[ip][kp][4] * x4 + pne[ip][kp][5] * x5
	 + pne[ip][kp][6] * x6 + pne[ip][kp][7] * x7;
        dyde = + pne[ip][kp][0] * y0 + pne[ip][kp][1] * y1            
	  + pne[ip][kp][2] * y2 + pne[ip][kp][3] * y3           
	  + pne[ip][kp][4] * y4 + pne[ip][kp][5] * y5           
	  + pne[ip][kp][6] * y6 + pne[ip][kp][7] * y7;

        dzde = + pne[ip][kp][0] * z0 + pne[ip][kp][1] * z1           
	  + pne[ip][kp][2] * z2 + pne[ip][kp][3] * z3           
	  + pne[ip][kp][4] * z4 + pne[ip][kp][5] * z5           
	  + pne[ip][kp][6] * z6 + pne[ip][kp][7] * z7;
	dxdt = + pnt[ip][jp][0] * x0 + pnt[ip][jp][1] * x1             
	  + pnt[ip][jp][2] * x2 + pnt[ip][jp][3] * x3             
	  + pnt[ip][jp][4] * x4 + pnt[ip][jp][5] * x5             
	  + pnt[ip][jp][6] * x6 + pnt[ip][jp][7] * x7;
        dydt = + pnt[ip][jp][0] * y0 + pnt[ip][jp][1] * y1
	  + pnt[ip][jp][2] * y2 + pnt[ip][jp][3] * y3
	  + pnt[ip][jp][4] * y4 + pnt[ip][jp][5] * y5
	  + pnt[ip][jp][6] * y6 + pnt[ip][jp][7] * y7;
        dzdt = + pnt[ip][jp][0] * z0 + pnt[ip][jp][1] * z1
	  + pnt[ip][jp][2] * z2 + pnt[ip][jp][3] * z3
	  + pnt[ip][jp][4] * z4 + pnt[ip][jp][5] * z5
	  + pnt[ip][jp][6] * z6 + pnt[ip][jp][7] * z7;

        detj[ip][jp][kp]= dxdq * (dyde * dzdt - dzde * dydt) 
	  + dydq * (dzde * dxdt - dxde * dzdt) 
	  + dzdq * (dxde * dydt - dyde * dxdt);

	/* INVERSE JACOBIAN */
        coef = 1.0 / detj[ip][jp][kp];
        a11 = coef * (dyde * dzdt - dzde * dydt);
        a12 = coef * (dzdq * dydt - dydq * dzdt);
        a13 = coef * (dydq * dzde - dzdq * dyde);

        a21 = coef * (dzde * dxdt - dxde * dzdt);
        a22 = coef * (dxdq * dzdt - dzdq * dxdt);
        a23 = coef * (dzdq * dxde - dxdq * dzde);

        a31 = coef * (dxde * dydt - dyde * dxdt);
        a32 = coef * (dydq * dxdt - dxdq * dydt);
        a33 = coef * (dxdq * dyde - dydq * dxde);

        detj[ip][jp][kp] = fabs(detj[ip][jp][kp]);

	/* set the dNi/dx, dNi/dy & dNi/dz components */
        pnx[ip][jp][kp][0] = a11 * pnq[jp][kp][0] + a12 * pne[ip][kp][0] 
	  + a13 * pnt[ip][jp][0];
        pnx[ip][jp][kp][1] = a11 * pnq[jp][kp][1] + a12 * pne[ip][kp][1]
	  + a13 * pnt[ip][jp][1];
        pnx[ip][jp][kp][2] = a11 * pnq[jp][kp][2] + a12 * pne[ip][kp][2] 
	  + a13 * pnt[ip][jp][2];
        pnx[ip][jp][kp][3] = a11 * pnq[jp][kp][3] + a12 * pne[ip][kp][3]
	  + a13 * pnt[ip][jp][3];
        pnx[ip][jp][kp][4] = a11 * pnq[jp][kp][4] + a12 * pne[ip][kp][4]
	  + a13 * pnt[ip][jp][4];
        pnx[ip][jp][kp][5] = a11 * pnq[jp][kp][5] + a12 * pne[ip][kp][5]
	  + a13 * pnt[ip][jp][5];
        pnx[ip][jp][kp][6] = a11 * pnq[jp][kp][6] + a12 * pne[ip][kp][6]
	  + a13 * pnt[ip][jp][6];
        pnx[ip][jp][kp][7] = a11 * pnq[jp][kp][7] + a12 * pne[ip][kp][7]
	  + a13 * pnt[ip][jp][7];

        pny[ip][jp][kp][0] = a21 * pnq[jp][kp][0] + a22 * pne[ip][kp][0] 
	  + a23 * pnt[ip][jp][0];
        pny[ip][jp][kp][1] = a21 * pnq[jp][kp][1] + a22 * pne[ip][kp][1] 
	  + a23 * pnt[ip][jp][1];
        pny[ip][jp][kp][2] = a21 * pnq[jp][kp][2] + a22 * pne[ip][kp][2] 
	  + a23 * pnt[ip][jp][2];
        pny[ip][jp][kp][3] = a21 * pnq[jp][kp][3] + a22 * pne[ip][kp][3] 
	  + a23 * pnt[ip][jp][3];
        pny[ip][jp][kp][4] = a21 * pnq[jp][kp][4] + a22 * pne[ip][kp][4] 
	  + a23 * pnt[ip][jp][4];
        pny[ip][jp][kp][5] = a21 * pnq[jp][kp][5] + a22 * pne[ip][kp][5]
	  + a23 * pnt[ip][jp][5];
        pny[ip][jp][kp][6] = a21 * pnq[jp][kp][6] + a22 * pne[ip][kp][6]
	  + a23 * pnt[ip][jp][6];
        pny[ip][jp][kp][7] = a21 * pnq[jp][kp][7] + a22 * pne[ip][kp][7]
	  + a23 * pnt[ip][jp][7];

        pnz[ip][jp][kp][0] = a31 * pnq[jp][kp][0] + a32 * pne[ip][kp][0]
	  + a33 * pnt[ip][jp][0];
        pnz[ip][jp][kp][1] = a31 * pnq[jp][kp][1] + a32 * pne[ip][kp][1] 
	  + a33 * pnt[ip][jp][1];
        pnz[ip][jp][kp][2] = a31 * pnq[jp][kp][2] + a32 * pne[ip][kp][2]
	  + a33 * pnt[ip][jp][2];
        pnz[ip][jp][kp][3] = a31 * pnq[jp][kp][3] + a32 * pne[ip][kp][3]
	  + a33 * pnt[ip][jp][3];
        pnz[ip][jp][kp][4] = a31 * pnq[jp][kp][4] + a32 * pne[ip][kp][4]
	  + a33 * pnt[ip][jp][4];
        pnz[ip][jp][kp][5] = a31 * pnq[jp][kp][5] + a32 * pne[ip][kp][5] 
	  + a33 * pnt[ip][jp][5];
        pnz[ip][jp][kp][6] = a31 * pnq[jp][kp][6] + a32 * pne[ip][kp][6]
	  + a33 * pnt[ip][jp][6];
        pnz[ip][jp][kp][7] = a31 * pnq[jp][kp][7] + a32 * pne[ip][kp][7]
	  + a33 * pnt[ip][jp][7];
      }
    }
  }

  return 1;

}
