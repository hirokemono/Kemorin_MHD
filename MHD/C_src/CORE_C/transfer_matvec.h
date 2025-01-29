
/* transfer_matvec.h */

#ifndef DEFINE_PARAMETERS_
#define DEFINE_PARAMETERS_

/* prototypes */

void transform_frame_xyz(int num, double *xx_org, double coef_matrix[16], double *xx_new);
void transform_frame_xyzw(int num, double *xx_org, double coef_matrix[16], double *xx_new);

#endif
