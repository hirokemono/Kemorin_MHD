
/* transfer_matvec.c */

#include "transfer_matvec.h"


void transform_frame_xyz(int num, double *xx_org, double coff_matrix[4][4], double *xx_new)
{
	int i;
	double xx, yy, zz;

	for(i=0;i<num;i++) {
		xx = xx_org[i*3  ];
		yy = xx_org[i*3+1];
		zz = xx_org[i*3+2];
		xx_new[i*3  ] = xx*coff_matrix[0][0] + yy*coff_matrix[1][0]+zz*coff_matrix[2][0] + coff_matrix[3][0];
		xx_new[i*3+1] = xx*coff_matrix[0][1] + yy*coff_matrix[1][1]+zz*coff_matrix[2][1] + coff_matrix[3][1];
		xx_new[i*3+2] = xx*coff_matrix[0][2] + yy*coff_matrix[1][2]+zz*coff_matrix[2][2] + coff_matrix[3][2];
	}
	return;
}

void transform_frame_xyzw(int num, double *xx_org, double coff_matrix[4][4], double *xx_new)
{
	int i;
	double xx, yy, zz, ww;

	for(i=0;i<num;i++) {
		xx = xx_org[i*4  ];
		yy = xx_org[i*4+1];
		zz = xx_org[i*4+2];
		ww = xx_org[i*4+3];
		xx_new[i*4  ] = xx*coff_matrix[0][0] + yy*coff_matrix[1][0]+zz*coff_matrix[2][0] + coff_matrix[3][0];
		xx_new[i*4+1] = xx*coff_matrix[0][1] + yy*coff_matrix[1][1]+zz*coff_matrix[2][1] + coff_matrix[3][1];
		xx_new[i*4+2] = xx*coff_matrix[0][2] + yy*coff_matrix[1][2]+zz*coff_matrix[2][2] + coff_matrix[3][2];
		xx_new[i*4+2] = xx*coff_matrix[0][3] + yy*coff_matrix[1][3]+zz*coff_matrix[2][3] + coff_matrix[3][3];
	}
	return;
}
