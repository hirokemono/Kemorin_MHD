
/* transfer_matvec.c */

#include "transfer_matvec.h"


void transform_frame_xyz(int num, double *xx_org, double coef_matrix[16], double *xx_new)
{
	int i;
	double xx, yy, zz;

	for(i=0;i<num;i++) {
		xx = xx_org[i*3  ];
		yy = xx_org[i*3+1];
		zz = xx_org[i*3+2];
		xx_new[i*3  ] = xx*coef_matrix[0] + yy*coef_matrix[4] + zz*coef_matrix[ 8] + coef_matrix[12];
		xx_new[i*3+1] = xx*coef_matrix[1] + yy*coef_matrix[5] + zz*coef_matrix[ 9] + coef_matrix[13];
		xx_new[i*3+2] = xx*coef_matrix[2] + yy*coef_matrix[6] + zz*coef_matrix[10] + coef_matrix[14];
	}
	return;
}

void transform_frame_xyzw(int num, double *xx_org, double coef_matrix[16], double *xx_new)
{
	int i;
	double xx, yy, zz, ww;

	for(i=0;i<num;i++) {
		xx = xx_org[i*4  ];
		yy = xx_org[i*4+1];
		zz = xx_org[i*4+2];
		ww = xx_org[i*4+3];
		xx_new[i*4  ] = xx*coef_matrix[0] + yy*coef_matrix[4] + zz*coef_matrix[ 8] + coef_matrix[12];
		xx_new[i*4+1] = xx*coef_matrix[1] + yy*coef_matrix[5] + zz*coef_matrix[ 9] + coef_matrix[13];
		xx_new[i*4+2] = xx*coef_matrix[2] + yy*coef_matrix[6] + zz*coef_matrix[10] + coef_matrix[14];
		xx_new[i*4+3] = xx*coef_matrix[3] + yy*coef_matrix[7] + zz*coef_matrix[11] + coef_matrix[15];
	}
	return;
}
