
/* m_gl_transfer_matrix.c */

#include "m_gl_transfer_matrix.h"

#define DTOR            0.01745329251994

static double object_size = 7.0e0;


void identity_glmat_c(double mat[16]){
	mat[ 0] = ONE;
	mat[ 4] = ZERO;
	mat[ 8] = ZERO;
	mat[12] = ZERO;
	
	mat[ 1] = ZERO;
	mat[ 5] = ONE;
	mat[ 9] = ZERO;
	mat[13] = ZERO;
	
	mat[ 2] = ZERO;
	mat[ 6] = ZERO;
	mat[10] = ONE;
	mat[14] = ZERO;
	
	mat[ 3] = ZERO;
	mat[ 7] = ZERO;
	mat[11] = ZERO;
	mat[15] = ONE;
	return;
};

void translate_glmat_c(double trans_x, double trans_y, double trans_z, 
			double mat[16]){
	mat[ 0] = ONE;
	mat[ 4] = ZERO;
	mat[ 8] = ZERO;
	mat[12] = trans_x;
	
	mat[ 1] = ZERO;
	mat[ 5] = ONE;
	mat[ 9] = ZERO;
	mat[13] = trans_y;
	
	mat[ 2] = ZERO;
	mat[ 6] = ZERO;
	mat[10] = ONE;
	mat[14] = trans_z;
	
	mat[ 3] = ZERO;
	mat[ 7] = ZERO;
	mat[11] = ZERO;
	mat[15] = ONE;
	return;
};

void scale_glmat_c(double scale_x, double scale_y, double scale_z,
			double mat[16]){
	mat[ 0] = scale_x;
	mat[ 4] = ZERO;
	mat[ 8] = ZERO;
	mat[12] = ZERO;
	
	mat[ 1] = ZERO;
	mat[ 5] = scale_y;
	mat[ 9] = ZERO;
	mat[13] = ZERO;
	
	mat[ 2] = ZERO;
	mat[ 6] = ZERO;
	mat[10] = scale_z;
	mat[14] = ZERO;
	
	mat[ 3] = ZERO;
	mat[ 7] = ZERO;
	mat[11] = ZERO;
	mat[15] = ONE;
	return;
};

void rotate_glmat_c(double angle_deg, double axis_x, double axis_y, double axis_z, 
			double mat[16]){
	double pi = FOUR * atan(ONE);
	double axs1_x, axs1_y, axs1_z, saxis;
	double angle = angle_deg * pi / 180.0;
	double c_agl = cos(angle);
	double s_agl = sin(angle);
	
	saxis = sqrt(axis_x*axis_x + axis_y*axis_y + axis_z*axis_z);
	if(saxis == 0.0){
		axs1_x= 0.0;
		axs1_y= 0.0;
		axs1_z= 1.0;
	} else {
		axs1_x= axis_x / saxis;
		axs1_y= axis_y / saxis;
		axs1_z= axis_z / saxis;
	};
	
	mat[ 0] = axs1_x*axs1_x * (ONE - c_agl) +          c_agl;
	mat[ 4] = axs1_x*axs1_y * (ONE - c_agl) - axs1_z * s_agl;
	mat[ 8] = axs1_x*axs1_z * (ONE - c_agl) + axs1_y * s_agl;
	mat[12] = ZERO;
	
	mat[ 1] = axs1_y*axs1_x * (ONE - c_agl) + axs1_z * s_agl;
	mat[ 5] = axs1_y*axs1_y * (ONE - c_agl) +           c_agl;
	mat[ 9] = axs1_y*axs1_z * (ONE - c_agl) - axs1_x * s_agl;
	mat[13] = ZERO;
	
	mat[ 2] = axs1_z*axs1_x * (ONE - c_agl) - axs1_y * s_agl;
	mat[ 6] = axs1_z*axs1_y * (ONE - c_agl) + axs1_x * s_agl;
	mat[10] = axs1_z*axs1_z * (ONE - c_agl) +          c_agl;
	mat[14] = ZERO;
	
	mat[ 3] = ZERO;
	mat[ 7] = ZERO;
	mat[11] = ZERO;
	mat[15] = ONE;
	return;
};

void frustsum_glmat_c(double left, double right, double bottom, double top,
			double near, double far, double mat[16]){
	
	mat[ 0] = TWO*near / (right-left);
	mat[ 4] = ZERO;
	mat[ 8] = (right+left) / (right-left);
	mat[12] = ZERO;
	
	mat[ 1] = ZERO;
	mat[ 5] = TWO*near / (top-bottom);
	mat[ 9] = (top+bottom) / (top-bottom);
	mat[13] = ZERO;
	
	mat[ 2] = ZERO;
	mat[ 6] = ZERO;
	mat[10] =-(far+near) / (far-near);
	mat[14] =-TWO*far*near / (far-near);
	
	mat[ 3] = ZERO;
	mat[ 7] = ZERO;
	mat[11] = -ONE;
	mat[15] = ZERO;
	
	return;
};

void orthogonal_glmat_c(double left, double right, double bottom, double top,
			double near, double far, double mat[16]){
	
	mat[ 0] = TWO / (right-left);
	mat[ 4] = ZERO;
	mat[ 8] = ZERO;
	mat[12] = -(right+left) / (right-left);
	
	mat[ 1] = ZERO;
	mat[ 5] = TWO / (top-bottom);
	mat[ 9] = ZERO;
	mat[13] = -(top+bottom) / (top-bottom);
	
	mat[ 2] = ZERO;
	mat[ 6] = ZERO;
	mat[10] =-TWO/ (far-near);
	mat[14] =-(far+near) / (far-near);
	
	mat[ 3] = ZERO;
	mat[ 7] = ZERO;
	mat[11] = ZERO;
	mat[15] = ONE;
	
	return;
};

void copy_glmat_c(double a[16], double b[16]){
	int i;
	for(i=0;i<16;i++){b[i] = a[i];};
	return;
};

void cal_glmat44_prod_c(double a[16], double b[16], double c[16]){
	c[ 0] = a[0]*b[0] + a[4]*b[1] + a[ 8]*b[2] + a[12]*b[3];
	c[ 1] = a[1]*b[0] + a[5]*b[1] + a[ 9]*b[2] + a[13]*b[3];
	c[ 2] = a[2]*b[0] + a[6]*b[1] + a[10]*b[2] + a[14]*b[3];
	c[ 3] = a[3]*b[0] + a[7]*b[1] + a[11]*b[2] + a[15]*b[3];
	
	c[ 4] = a[0]*b[4] + a[4]*b[5] + a[ 8]*b[6] + a[12]*b[7];
	c[ 5] = a[1]*b[4] + a[5]*b[5] + a[ 9]*b[6] + a[13]*b[7];
	c[ 6] = a[2]*b[4] + a[6]*b[5] + a[10]*b[6] + a[14]*b[7];
	c[ 7] = a[3]*b[4] + a[7]*b[5] + a[11]*b[6] + a[15]*b[7];
	
	c[ 8] = a[0]*b[8] + a[4]*b[9] + a[ 8]*b[10] + a[12]*b[11];
	c[ 9] = a[1]*b[8] + a[5]*b[9] + a[ 9]*b[10] + a[13]*b[11];
	c[10] = a[2]*b[8] + a[6]*b[9] + a[10]*b[10] + a[14]*b[11];
	c[11] = a[3]*b[8] + a[7]*b[9] + a[11]*b[10] + a[15]*b[11];
	
	c[12] = a[0]*b[12] + a[4]*b[13] + a[ 8]*b[14] + a[12]*b[15];
	c[13] = a[1]*b[12] + a[5]*b[13] + a[ 9]*b[14] + a[13]*b[15];
	c[14] = a[2]*b[12] + a[6]*b[13] + a[10]*b[14] + a[14]*b[15];
	c[15] = a[3]*b[12] + a[7]*b[13] + a[11]*b[14] + a[15]*b[15];
	return;
};

void cal_glmat33_prod_c(double a[16], double b[16], double c[16]){
	c[ 0] = a[0]*b[0] + a[4]*b[1] + a[ 8]*b[2];
	c[ 1] = a[1]*b[0] + a[5]*b[1] + a[ 9]*b[2];
	c[ 2] = a[2]*b[0] + a[6]*b[1] + a[10]*b[2];
	c[ 3] = a[3]*b[0] + a[7]*b[1] + a[11]*b[2];
	
	c[ 4] = a[0]*b[4] + a[4]*b[5] + a[ 8]*b[6];
	c[ 5] = a[1]*b[4] + a[5]*b[5] + a[ 9]*b[6];
	c[ 6] = a[2]*b[4] + a[6]*b[5] + a[10]*b[6];
	c[ 7] = a[3]*b[4] + a[7]*b[5] + a[11]*b[6];
	
	c[ 8] = a[0]*b[8] + a[4]*b[9] + a[ 8]*b[10];
	c[ 9] = a[1]*b[8] + a[5]*b[9] + a[ 9]*b[10];
	c[10] = a[2]*b[8] + a[6]*b[9] + a[10]*b[10];
	c[11] = a[3]*b[8] + a[7]*b[9] + a[11]*b[10];
	
	c[12] = b[12];
	c[13] = b[13];
	c[14] = b[14];
	c[15] = b[15];
	return;
};


static void perspectiveGL(double fovY, double aspect, double zNear, double zFar,
			double projection[16]){
    double pi = 3.1415926535897932384626433832795;
    double fW, fH;
    
    /* fH = tan( (fovY / 2) / 180 * pi ) * zNear; */
    fH = tan( fovY / 360 * pi ) * zNear;
    fW = fH * aspect;
    
	frustsum_glmat_c(-fW, fW, -fH, fH, zNear, zFar, projection);
}

void orthogonalGL(double left, double right, double bottom, double top,
			double near, double far){
	double orthogonal[16];
	orthogonal_glmat_c(left, right, bottom, top, near, far, orthogonal);
	return;
};

static void Kemo_Translate_view_c(double shift_x, double shift_y, double shift_z, 
			double model[16]){
	double tmpmat[16];
	double trans[16];
	
	copy_glmat_c(model, tmpmat);
   	translate_glmat_c(shift_x, shift_y, shift_z, trans);
	cal_glmat44_prod_c(trans, tmpmat, model);
	return;
}

static void Kemo_Scale_view_c(double scale_x, double scale_y, double scale_z, 
			double model[16]){
	double tmpmat[16];
	double trans[16];
	
	copy_glmat_c(model, tmpmat);
   	scale_glmat_c(scale_x, scale_y, scale_z, trans);
	cal_glmat33_prod_c(trans, tmpmat, model);
	return;
}

static void Kemo_Rotate_view_c(double rotate[4], double model[16]){
	double tmpmat[16];
	double trans[16];
	
	copy_glmat_c(model, tmpmat);
   	rotate_glmat_c(rotate[0], rotate[1], rotate[2], rotate[3], trans);
	cal_glmat33_prod_c(trans, tmpmat, model);
	return;
}

static void update_projection(double x_lookfrom[2], int nx_frame, int ny_frame,
							  double aperture, double aspect, double near, double far,
							  double projection[16]){
	double wd2;
	double left, right;
	
	near = x_lookfrom[2] - object_size * HALF;
	if (near < 1.0e-6) near = 1.0e-6;
	
	far = x_lookfrom[2] + object_size * HALF;
	if (far < ONE) far = ONE;
	
	aspect = ((double) nx_frame) / ((double) ny_frame);
	wd2 =  near * tan(aperture*DTOR*HALF);
	
	left  = - aspect * wd2;
	right =   aspect * wd2;
	
	frustsum_glmat_c(left, right, (-wd2), wd2, near, far, projection);
	return;
}

static void update_projection_left(double x_lookfrom[2], int nx_frame, int ny_frame,
								   double aperture, double aspect, double near, double far,
								   double focalLength, double eyeSep, double projection[16]){
	double wd2, ndfl;
	double left, right;
	
	near = x_lookfrom[2] - object_size * HALF;
	if (near < 1.0e-6) near = 1.0e-6;
	
	far = x_lookfrom[2] + object_size * HALF;
	if (far < ONE) far = ONE;
	
	aspect = ((double) nx_frame) / ((double) ny_frame);
	wd2 =  near * tan(aperture*DTOR*HALF);
	ndfl = near / focalLength;
	
	left  = - aspect * wd2 + 0.5 * eyeSep * ndfl;
	right =   aspect * wd2 + 0.5 * eyeSep * ndfl;
	
	frustsum_glmat_c(left, right, (-wd2), wd2, near, far, projection);
	return;
}

static void update_projection_right(double x_lookfrom[2], int nx_frame, int ny_frame,
									double aperture, double aspect, double near, double far,
									double focalLength, double eyeSep, double projection[16]){
	double wd2, ndfl;
	double left, right;
	
	near = x_lookfrom[2] - object_size * HALF;
	if (near < 1.0e-6) near = 1.0e-6;
	
	far = x_lookfrom[2] + object_size * HALF;
	if (far < ONE) far = ONE;
	
	aspect = ((double) nx_frame) / ((double) ny_frame);
	wd2 =  near * tan(aperture*DTOR*HALF);
	ndfl = near / focalLength;
	
	left  = - aspect * wd2 - 0.5 * eyeSep * ndfl;
	right =   aspect * wd2 - 0.5 * eyeSep * ndfl;
	
	identity_glmat_c(projection);
	frustsum_glmat_c(left, right, (-wd2), wd2, near, far, projection);
	return;
}


void set_view_by_identity(void){
	double modelview[16];
	
	identity_glmat_c(modelview);
	return;
};


void modify_view_by_struct(struct view_element *view){
	identity_glmat_c(view->mat_object_2_eye);
	Kemo_Translate_view_c(view->shift[0], view->shift[1], view->shift[2],
				view->mat_object_2_eye);
	Kemo_Rotate_view_c(view->rotation, view->mat_object_2_eye);
	Kemo_Scale_view_c(view->iso_scale, view->iso_scale, view->iso_scale,
				view->mat_object_2_eye);
	Kemo_Translate_view_c(-view->x_lookat[0], -view->x_lookat[1], -view->x_lookat[2],
				view->mat_object_2_eye);
	return;
};
void modify_left_view_by_struct(struct view_element *view){
	identity_glmat_c(view->mat_object_2_eye);
	Kemo_Translate_view_c(view->shift[0], view->shift[1], view->shift[2],
				view->mat_object_2_eye);
	Kemo_Translate_view_c(view->eye_separation/3.0, ZERO, ZERO, view->mat_object_2_eye);
	Kemo_Rotate_view_c(view->rotation, view->mat_object_2_eye);
	Kemo_Scale_view_c(view->iso_scale, view->iso_scale, view->iso_scale,
				view->mat_object_2_eye);
	Kemo_Translate_view_c(-view->x_lookat[0], -view->x_lookat[1], -view->x_lookat[2],
				view->mat_object_2_eye);
	return;
};
void modify_right_view_by_struct(struct view_element *view){
	
	identity_glmat_c(view->mat_object_2_eye);
	Kemo_Translate_view_c(view->shift[0], view->shift[1], view->shift[2], 
				view->mat_object_2_eye);
	Kemo_Translate_view_c(-view->eye_separation/3.0, ZERO, ZERO,
				view->mat_object_2_eye);
	Kemo_Rotate_view_c(view->rotation, view->mat_object_2_eye);
	Kemo_Scale_view_c(view->iso_scale, view->iso_scale, view->iso_scale,
				view->mat_object_2_eye);
	Kemo_Translate_view_c(-view->x_lookat[0], -view->x_lookat[1], -view->x_lookat[2],
				view->mat_object_2_eye);
	return;
};


void rotate_view_by_struct(struct view_element *view){
	identity_glmat_c(view->mat_object_2_eye);
	Kemo_Translate_view_c(view->shift[0], view->shift[1], view->shift[2],
				view->mat_object_2_eye);
	Kemo_Rotate_view_c(view->rotate_animation, view->mat_object_2_eye);
	Kemo_Rotate_view_c(view->rotation, view->mat_object_2_eye);
	Kemo_Scale_view_c(view->iso_scale, view->iso_scale, view->iso_scale,
				view->mat_object_2_eye);
	Kemo_Translate_view_c(-view->x_lookat[0], -view->x_lookat[1], -view->x_lookat[2],
				view->mat_object_2_eye);
	/*
	printf("          /%f %f %f %f\\ \n", view->mat_object_2_eye[0], view->mat_object_2_eye[4], view->mat_object_2_eye[ 8], view->mat_object_2_eye[12]);
	printf(" matrix = |%f %f %f %f| \n", view->mat_object_2_eye[1], view->mat_object_2_eye[5], view->mat_object_2_eye[ 9], view->mat_object_2_eye[13]);
	printf("          |%f %f %f %f| \n", view->mat_object_2_eye[2], view->mat_object_2_eye[6], view->mat_object_2_eye[10], view->mat_object_2_eye[14]);
	printf("          \\%f %f %f %f/ \n", view->mat_object_2_eye[3], view->mat_object_2_eye[7], view->mat_object_2_eye[11], view->mat_object_2_eye[15]);
	*/
	return;
};

void rotate_left_view_by_struct(struct view_element *view){
	identity_glmat_c(view->mat_object_2_eye);
	Kemo_Translate_view_c(view->shift[0], view->shift[1], view->shift[2], 
				view->mat_object_2_eye);
	Kemo_Translate_view_c(view->eye_separation/3.0, ZERO, ZERO, 
				view->mat_object_2_eye);
	Kemo_Rotate_view_c(view->rotate_animation, view->mat_object_2_eye);
	Kemo_Rotate_view_c(view->rotation, view->mat_object_2_eye);
	Kemo_Scale_view_c(view->iso_scale, view->iso_scale, view->iso_scale, 
				view->mat_object_2_eye);
	Kemo_Translate_view_c(-view->x_lookat[0], -view->x_lookat[1], -view->x_lookat[2],
				view->mat_object_2_eye);
	return;
};

void rotate_right_view_by_struct(struct view_element *view){
	identity_glmat_c(view->mat_object_2_eye);
	Kemo_Translate_view_c(view->shift[0], view->shift[1], view->shift[2], 
				view->mat_object_2_eye);
	Kemo_Translate_view_c(-view->eye_separation/3.0, ZERO, ZERO, 
				view->mat_object_2_eye);
	Kemo_Rotate_view_c(view->rotate_animation, view->mat_object_2_eye);
	Kemo_Rotate_view_c(view->rotation, view->mat_object_2_eye);
	Kemo_Scale_view_c(view->iso_scale, view->iso_scale, view->iso_scale, 
				view->mat_object_2_eye);
	Kemo_Translate_view_c(-view->x_lookat[0], -view->x_lookat[1], -view->x_lookat[2],
				view->mat_object_2_eye);
	return;
};

void set_view_for_message(struct view_element *view){
	double modelview[16];
	double scale[3];
	
	scale[0] =  ((double) (1+view->iflag_retina)) / ((double) view->nx_frame);
	scale[1] = - ((double) (1+view->iflag_retina)) / ((double) view->ny_frame);
	scale[2] =  1.0;
	
	identity_glmat_c(modelview);
	Kemo_Scale_view_c(scale[0], scale[1], scale[2], modelview);
	return;
};


void set_projection_by_identity(void){
	double projection[16];
	
	identity_glmat_c(projection);
	return;
};

void init_projection_struct(struct view_element *view){
	perspectiveGL(view->aperture, view->aspect, view->near, view->far,
				  view->mat_eye_2_clip);
	return;
};

void update_projection_struct(struct view_element *view){
	update_projection(view->x_lookfrom, view->nx_frame, view->ny_frame,
                      view->aperture, view->aspect, view->near, view->far, 
					  view->mat_eye_2_clip);
	return;
};
void update_left_projection_struct(struct view_element *view){
	update_projection_left(view->x_lookfrom, view->nx_frame, view->ny_frame,
						   view->aperture, view->aspect, view->near, view->far,
						   view->focal_length, view->eye_separation, 
						   view->mat_eye_2_clip);
	return;
};
void update_right_projection_struct(struct view_element *view){
	update_projection_right(view->x_lookfrom, view->nx_frame, view->ny_frame,
							view->aperture, view->aspect, view->near, view->far,
							view->focal_length, view->eye_separation, 
							view->mat_eye_2_clip);
	return;
};



void copy_lookatpoint_struct(struct view_element *origin, struct view_element *dest)
{
	int i;
	for (i=0; i<3; i++) dest->x_lookat[i] = origin->x_lookat[i];
	return;
}

void copy_viewmatrix_struct(struct view_element *origin, struct view_element *dest)
{
	int i;
	for (i=0; i<3; i++) {
		dest->shift[i] =        origin->shift[i];
		dest->rotation[i] =     origin->rotation[i];
	}
	dest->rotation[3] = origin->rotation[3];
    dest->iso_scale = origin->iso_scale;
	return;
}



void set_position_in_model(struct view_element *view, int nnod,
                           double **xx, double **end_eye){
	int i;
	
	/* transfer matrix for object */
	for (i=0;i<nnod;i++){
        end_eye[i][0] =  (double) view->mat_object_2_eye[   0]*xx[i][0]
        + (double) view->mat_object_2_eye[ 4+0]*xx[i][1]
        + (double) view->mat_object_2_eye[ 8+0]*xx[i][2]
        + (double) view->mat_object_2_eye[12+0] * 1.0;
        end_eye[i][1] =  (double) view->mat_object_2_eye[   1]*xx[i][0]
        + (double) view->mat_object_2_eye[ 4+1]*xx[i][1]
        + (double) view->mat_object_2_eye[ 8+1]*xx[i][2]
        + (double) view->mat_object_2_eye[12+1] * 1.0;
        end_eye[i][2] =  (double) view->mat_object_2_eye[   2]*xx[i][0]
        + (double) view->mat_object_2_eye[ 4+2]*xx[i][1]
        + (double) view->mat_object_2_eye[ 8+2]*xx[i][2]
        + (double) view->mat_object_2_eye[12+2] * 1.0;
        end_eye[i][3] =  (double) view->mat_object_2_eye[   3]*xx[i][0]
        + (double) view->mat_object_2_eye[ 4+3]*xx[i][1]
        + (double) view->mat_object_2_eye[ 8+3]*xx[i][2]
        + (double) view->mat_object_2_eye[12+3] * 1.0;
	};
	
	return;
}


void set_distance_in_model(struct view_element *view, int nnod,
                           double **xx, double *z_eye){
	int i;
	
	/* transfer matrix for object*/
	for (i=0;i<nnod;i++){
		z_eye[i]= (double) view->mat_object_2_eye[   2]*xx[i][0]
        + (double) view->mat_object_2_eye[ 4+2]*xx[i][1]
        + (double) view->mat_object_2_eye[ 8+2]*xx[i][2]
        + (double) view->mat_object_2_eye[12+2] * 1.0;
	}
	return;
}

void set_3d_position_to_window(int point_screen[2], double xx[3], 
                               int nx_win, int ny_win, struct view_element *view){
	int i;
	double end_eye[4];
	double end_clip[4];
	double end_device[4];
	
	/* transfer matrix for object
     glGetFloatv(GL_MODELVIEW_MATRIX, view->mat_object_2_eye);
     glGetFloatv(GL_PROJECTION_MATRIX, view->mat_eye_2_clip); */
	for (i=0;i<4;i++){
		end_eye[i] =  view->mat_object_2_eye[   i]*((float) xx[0])
        + view->mat_object_2_eye[ 4+i]*((float) xx[1])
        + view->mat_object_2_eye[ 8+i]*((float) xx[2])
        + view->mat_object_2_eye[12+i]* 1.0;
	};
	for (i=0;i<4;i++){
		end_clip[i] =  view->mat_eye_2_clip[   i]*end_eye[0]
        + view->mat_eye_2_clip[ 4+i]*end_eye[1]
        + view->mat_eye_2_clip[ 8+i]*end_eye[2]
        + view->mat_eye_2_clip[ 12+i]*end_eye[3];
	};
	for (i=0;i<4;i++){
		end_device[i] =  end_clip[i] / end_clip[3];
	};
	point_screen[0] = (int) ( (end_device[0] + 1.0) * ((double) nx_win / 2.0) );
	/*point_screen[1] = (int) ( (end_device[1] + ((double) nx_win) / ((double) ny_win) )
                             * ((double) ny_win / 2.0) ) ; */
    point_screen[1] = (int) ( (end_device[1] + 1.0) * ((double) ny_win / 2.0) );
	
	return;
}

void init_kemoview_perspective(struct view_element *view){
	view->aperture =   INITIAL_APATURE;
	
	view->near =       INITIAL_NEAR;
	view->far =        INITIAL_FAR;
	
	view->aspect = ((double) view->nx_frame) / ((double) view->ny_frame);
	
	view->focal_length =  INITIAL_FOCAL;
	view->eye_separation = INITIAL_EYE_SEP;
	view->eye_to_focal =  view->eye_separation / view->focal_length;
	return;
}

void set_gl_windowsize(struct view_element *view, int npixel_x, int npixel_y,
                       int nwindow_x, int nwindow_y)
{
	view->nx_frame = npixel_x;
	view->ny_frame = npixel_y;
    view->nx_window = nwindow_x;
    view->ny_window = nwindow_y;

    if(view->nx_frame > view->nx_window){
        view->iflag_retina = 1;
    } else {
        view->iflag_retina = 0;
    }
	return;
}

int send_gl_windowsize_x(struct view_element *view){return (int) view->nx_frame;};
int send_gl_windowsize_y(struct view_element *view){return (int) view->ny_frame;};
void update_projection_by_windowsize(struct view_element *view, int npixel_x, int npixel_y,
                                     int nwindow_x, int nwindow_y)
{
    set_gl_windowsize(view, npixel_x, npixel_y, nwindow_x, nwindow_y);
	update_projection_struct(view);
    return;
}

void set_gl_retinamode(struct view_element *view, int i_retina)
{
    view->iflag_retina = i_retina;
    return;
}
int send_gl_retinamode(struct view_element *view)
{
    return view->iflag_retina;
}

void set_gl_rotation_parameter(struct view_element *view, int i, double rot_vect)
{
	view->rotation[i] = rot_vect;
	return;
}

void set_gl_dragging_rotation(struct view_element *view, double rot_vect[4])
{
	int i;
	for (i=0;i<4;i++) {view->rotate_dragging[i] = rot_vect[i];};
	return;
}

void set_gl_animation_rotation(struct view_element *view, double rot_vect[4])
{
	int i;
	for (i=0;i<4;i++) {view->rotate_animation[i] = rot_vect[i];};
	return;
}
void set_gl_animation_rot_axis(struct view_element *view, int iaxis){
	view->rotate_animation[1] = ZERO;
	view->rotate_animation[2] = ZERO;
	view->rotate_animation[3] = ZERO;
	view->rotate_animation[iaxis] = ONE;
}
void set_gl_animation_rot_angle(struct view_element *view, int int_degree){
	view->rotate_animation[0] = (double) int_degree;
}

void set_gl_shift_vector(struct view_element *view, int i, double position)
{
	view->shift[i] =       position;
	view->x_lookfrom[i] = -position;
	return;
}

void set_gl_scalar_scale_factor(struct view_element *view, double scale_s)
{
	view->iso_scale = scale_s;
	return;
};

void set_gl_projection_aperture(struct view_element *view, double aperture_s)
{
	view->aperture = aperture_s;
	return;
};

void set_gl_stereo_parameter(struct view_element *view, double focus, double eye_sep)
{
	view->focal_length =   focus;
	view->eye_separation = eye_sep;
	view->eye_to_focal =  view->eye_separation / view->focal_length;
	return;
};


double send_gl_rotation_parameter(struct view_element *view, int i){return (double) view->rotation[i];}

void send_gl_dragging_rotation(struct view_element *view, double rot_vect[4])
{
	int i;
	for (i=0;i<4;i++) {rot_vect[i] = view->rotate_dragging[i];};
	return;
}

void send_gl_animation_rotation(struct view_element *view, double rot_vect[4])
{
	int i;
	for (i=0;i<4;i++) {rot_vect[i] = view->rotate_animation[i];};
	return;
}

double send_gl_shift_vector(struct view_element *view, int i){return (double) view->shift[i];};

double send_gl_lookat_vector(struct view_element *view, int i){return (double) view->x_lookat[i];};

double send_scalar_scale_factor(struct view_element *view){return (double) view->iso_scale;};

double send_gl_projection_aperture(struct view_element *view){return (double) view->aperture;};
double send_gl_projection_far(struct view_element *view){return (double) view->near;};
double send_gl_projection_near(struct view_element *view){return (double) view->far;};
double send_gl_projection_aspect(struct view_element *view){return (double) view->aspect;};

double send_gl_stereo_focus(struct view_element *view){
	return (double) view->focal_length;
};
double send_gl_stereo_eyeseparation(struct view_element *view){
	return (double) view->eye_separation;
};


/* called with the start position and the window origin + size */
void gl_startTrackball(double x, double y, struct view_element *view){
	startTrackball_c(x, y, ZERO, ZERO, view->nx_frame, -view->ny_frame);
	return;
};
/* calculated rotation based on current mouse position */
void gl_rollToTrackball(double x, double y, struct view_element *view){
	rollToTrackball_c (x, y, view->rotate_dragging);
	return;
};
/* add a GL rotation (dA) to an existing GL rotation (A) */
void gl_drag_addToRotationTrackball(struct view_element *view){
	if (view->rotate_dragging[0] != ZERO)
    addToRotationTrackball_c (view->rotate_dragging, view->rotation);
	view->rotate_dragging [0] = ZERO;
	view->rotate_dragging [1] = ZERO;
	view->rotate_dragging [2] = ZERO;
	view->rotate_dragging [3] = ZERO;
	return;
};




void gl_mousedolly_struct(struct view_element *view, double start[2], 
                          double x_dolly, double y_dolly){
	double dolly = (start[1] - y_dolly) * view->shift[2] / 300.0f;
	view->shift[2] -= dolly;
    /* do not let z = 0.0 */
	if (view->shift[2] == 0.0) view->shift[2] = -0.0001;
	view->x_lookfrom[2] = -view->shift[2];
	start[0] = (int) x_dolly;
	start[1] = (int) y_dolly;
	return;
}

void gl_mousepan_struct(struct view_element *view, double start[2], 
                        double x_pan, double y_pan){
	double panX = (start[0] - x_pan) / (1200.0f / view->shift[2]);
	double panY = (start[1] - y_pan) / (1200.0f / view->shift[2]);
	view->shift[0] += panX;
	view->shift[1] -= panY;
	view->x_lookfrom[0] = -view->shift[0];
	view->x_lookfrom[1] = -view->shift[1];
	start[0] = x_pan;
	start[1] = y_pan;
}


void gl_zooming_struct(struct view_element *view, double wheelDelta){
	double deltaAperture = wheelDelta * -view->aperture / 200.0f;
	view->aperture += deltaAperture;
	/* do not let aperture <= 0.1 and  aperture >= 180*/
	if (view->aperture < 0.1)   view->aperture = 0.1;
	if (view->aperture > 179.9) view->aperture = 179.9;
	
//	update_projection_struct(view);
}

void reset_to_init_angle(struct view_element *view){
	view->x_lookfrom[0] = ZERO;
	view->x_lookfrom[1] = ZERO;
	view->x_lookfrom[2] = TEN;
	
	view->shift[0] = -view->x_lookfrom[0];
	view->shift[1] = -view->x_lookfrom[1];
	view->shift[2] = -view->x_lookfrom[2];
	
	view->rotation[0] =   110.0;
	view->rotation[1] =  -0.32;
	view->rotation[2] =  -0.41;
	view->rotation[3] =  -0.854;
	
	view->rotate_dragging[0] =   ZERO;
	view->rotate_dragging[1] =   ZERO;
	view->rotate_dragging[2] =   ZERO;
	view->rotate_dragging[3] =   ONE;
	
	view->rotate_animation[0] =   ZERO;
	view->rotate_animation[1] =   ZERO;
	view->rotate_animation[2] =   ZERO;
	view->rotate_animation[3] =   ONE;
	
	view->aperture =   TEN;
	return;
}

void reset_all_view_parameter(struct view_element *view){
    int i;
    
	reset_to_init_angle(view);
	
    view->iso_scale = ONE;
	for (i=0;i<3;i++){
        view->x_lookat[i] = ZERO;
    };
	return;
}

void view_for_xy_plane(struct view_element *view){
	
	view->rotation[0]= ZERO;
	view->rotation[1]= ZERO;
	view->rotation[2]= ZERO;
	view->rotation[3]= ZERO;
	
	return;
};

void view_for_yz_plane(struct view_element *view){
	
	view->rotation[0]= THREE * FOURTY;
	view->rotation[1]= -ONE;
	view->rotation[2]= -ONE;
	view->rotation[3]= -ONE;
	
	return;
};

void view_for_xz_plane(struct view_element *view){
	
	view->rotation[0]= OPP_DEG + PERP_DEG;
	view->rotation[1]=  ONE;
	view->rotation[2]=  ZERO;
	view->rotation[3]=  ZERO;
	
	return;
};


void set_3d_position_to_window_d(int point_screen[2], double xx[3], 
                                 int nx_win, int ny_win, struct view_element *view){
	int i;
	double end_eye[4];
	double end_clip[4];
	double end_device[4];
	
	/* transfer matrix for object
	 glGetFloatv(GL_MODELVIEW_MATRIX, view->mat_object_2_eye);
	 glGetFloatv(GL_PROJECTION_MATRIX, view->mat_eye_2_clip); */
	for (i=0;i<4;i++){
		end_eye[i] =  view->mat_object_2_eye[   i]*xx[0]
		+ view->mat_object_2_eye[ 4+i]*xx[1]
		+ view->mat_object_2_eye[ 8+i]*xx[2]
		+ view->mat_object_2_eye[12+i]* 1.0;
	};
	for (i=0;i<4;i++){
		end_clip[i] =  view->mat_eye_2_clip[   i]*end_eye[0]
		+ view->mat_eye_2_clip[ 4+i]*end_eye[1]
		+ view->mat_eye_2_clip[ 8+i]*end_eye[2]
		+ view->mat_eye_2_clip[ 12+i]*end_eye[3];
	};
	for (i=0;i<4;i++){
		end_device[i] =  end_clip[i] / end_clip[3];
	};
	point_screen[0] = (int) ( (end_device[0] + 1.0) * ((double) nx_win / 2.0) );
	point_screen[1] = (int) ( (end_device[1] + ((double) nx_win) / ((double) ny_win) )
							 * ((double) ny_win / 2.0) ) ;
	
	return;
}
