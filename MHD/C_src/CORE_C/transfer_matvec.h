
/* transfer_matvec.h */

#ifndef DEFINE_PARAMETERS_
#define DEFINE_PARAMETERS_

/* prototypes */

void transform_frame_xyz(int num, double *xx_org, double coff_matrix[4][4], double *xx_new);
void transform_frame_xyzw(int num, double *xx_org, double coff_matrix[4][4], double *xx_new);

#endif
