
/* m_gl_transfer_matrix.c */

#include "m_gl_transfer_matrix.h"

#define DTOR            0.01745329251994

static GLdouble object_size = 7.0e0;

static void init_center_model(GLdouble shift[3]){
	glMatrixMode(GL_MODELVIEW);
	glPopMatrix();
	glPushMatrix();
	glLoadIdentity();

	/* Shift object againt to the eye point */
	/*  gluLookAt(-shift[0], -shift[1], -shift[2], -shift[0], -shift[1], 0, 0., 1., 0.); */

	glTranslated(shift[0], shift[1], shift[2]);

	return;
}
static void init_left_eye_model(GLdouble shift[3], GLdouble eyeSep){
	glMatrixMode(GL_MODELVIEW);
	glPopMatrix();
	glPushMatrix();
	glLoadIdentity();

	/* Shift object againt to the eye point */
	/*  gluLookAt(-shift[0]-eyeSep/3.0, -shift[1], -shift[2], 
				  -shift[0]-eyeSep/3.0, -shift[1],         0., 0., 1., 0.); */
	
	glTranslated(shift[0]+eyeSep/3.0, shift[1], shift[2]);
	return;
}
static void init_right_eye_model(GLdouble shift[3], GLdouble eyeSep){
	glMatrixMode(GL_MODELVIEW);
	glPopMatrix();
	glPushMatrix();
	glLoadIdentity();

	/* Shift object againt to the eye point */
	/*  gluLookAt(-shift[0]+eyeSep/3.0, -shift[1], -shift[2], 
				  -shift[0]+eyeSep/3.0, -shift[1],         0., 0., 1., 0.); */
	
	glTranslated(shift[0]-eyeSep/3.0, shift[1], shift[2]);
	return;
}

static void modify_view_kemo(GLdouble x_lookat[3], GLdouble shift[3], GLdouble scale_factor[3],
							 GLdouble rotation[4]){

	/* Rotate object around the lookat point*/
	glRotated(rotation[0], rotation[1], rotation[2], rotation[3]);
	/* Change size of object */
	glScaled(scale_factor[0], scale_factor[1], scale_factor[2]);
	/* Move lookat point to the center of coordinate */
	glTranslated(-x_lookat[0], -x_lookat[1], -x_lookat[2]);
	
	return;
};

static void rotate_view_kemo(GLdouble x_lookat[3], GLdouble shift[3], GLdouble scale_factor[3],
							 GLdouble rotation[4], GLdouble rotate_animation[4]){
	glRotated(rotation[0], rotation[1], rotation[2], rotation[3]);
	glRotated(rotate_animation[0], rotate_animation[1], rotate_animation[2], rotate_animation[3]);
	glScaled(scale_factor[0], scale_factor[1], scale_factor[2]);
	glTranslated(-x_lookat[0], -x_lookat[1], -x_lookat[2]);
	return;
};


static void update_projection(GLdouble x_lookfrom[2], GLint nx_window, GLint ny_window,
							  GLdouble aperture, GLdouble aspect, GLdouble near, GLdouble far){
	GLdouble wd2;
	GLdouble left, right;
	/* set projection */
	glMatrixMode (GL_PROJECTION);
	glLoadIdentity ();
	
	near = x_lookfrom[2] - object_size * HALF;
	if (near < 1.0e-6) near = 1.0e-6;
	
	far = x_lookfrom[2] + object_size * HALF;
	if (far < ONE) far = ONE;
	
	aspect = ((GLdouble) nx_window) / ((GLdouble) ny_window);
	wd2 =  near * tan(aperture*DTOR*HALF);
	
	left  = - aspect * wd2;
	right =   aspect * wd2;
	glFrustum (left, right, (-wd2), wd2, near, far);

/*	gluPerspective(aperture, aspect, near, far);*/
	return;
}

static void update_projection_left(GLdouble x_lookfrom[2], GLint nx_window, GLint ny_window,
								   GLdouble aperture, GLdouble aspect, GLdouble near, GLdouble far,
								   GLdouble focalLength, GLdouble eyeSep){
	GLdouble wd2, ndfl;
	GLdouble left, right;
	/* set projection */
	glMatrixMode (GL_PROJECTION);
	glLoadIdentity ();
	
	near = x_lookfrom[2] - object_size * HALF;
	if (near < 1.0e-6) near = 1.0e-6;
	
	far = x_lookfrom[2] + object_size * HALF;
	if (far < ONE) far = ONE;
	
	aspect = ((GLdouble) nx_window) / ((GLdouble) ny_window);
	wd2 =  near * tan(aperture*DTOR*HALF);
	ndfl = near / focalLength;
	
	left  = - aspect * wd2 + 0.5 * eyeSep * ndfl;
	right =   aspect * wd2 + 0.5 * eyeSep * ndfl;
	glFrustum (left, right, (-wd2), wd2, near, far);
	return;
}

static void update_projection_right(GLdouble x_lookfrom[2], GLint nx_window, GLint ny_window,
									GLdouble aperture, GLdouble aspect, GLdouble near, GLdouble far,
									GLdouble focalLength, GLdouble eyeSep){
	GLdouble wd2, ndfl;
	GLdouble left, right;
	/* set projection */
	glMatrixMode (GL_PROJECTION);
	glLoadIdentity ();
	
	near = x_lookfrom[2] - object_size * HALF;
	if (near < 1.0e-6) near = 1.0e-6;
	
	far = x_lookfrom[2] + object_size * HALF;
	if (far < ONE) far = ONE;
	
	aspect = ((GLdouble) nx_window) / ((GLdouble) ny_window);
	wd2 =  near * tan(aperture*DTOR*HALF);
	ndfl = near / focalLength;
	
	left  = - aspect * wd2 - 0.5 * eyeSep * ndfl;
	right =   aspect * wd2 - 0.5 * eyeSep * ndfl;
	glFrustum (left, right, (-wd2), wd2, near, far);
	return;
}




void modify_view_by_struct(struct view_element *view){
	init_center_model(view->shift);
	modify_view_kemo(view->x_lookat, view->shift, view->scale_factor,
			view->rotation);
	glGetDoublev(GL_MODELVIEW_MATRIX, view->mat_object_2_eye);
	return;
};
void modify_left_view_by_struct(struct view_element *view){
	init_left_eye_model(view->shift, view->eye_separation);
	modify_view_kemo(view->x_lookat, view->shift, view->scale_factor,
					 view->rotation);
	glGetDoublev(GL_MODELVIEW_MATRIX, view->mat_object_2_eye);
	return;
};
void modify_right_view_by_struct(struct view_element *view){
	init_right_eye_model(view->shift, view->eye_separation);
	modify_view_kemo(view->x_lookat, view->shift, view->scale_factor,
					 view->rotation);
	glGetDoublev(GL_MODELVIEW_MATRIX, view->mat_object_2_eye);
	return;
};


void rotate_view_by_struct(struct view_element *view){
	init_center_model(view->shift);
	rotate_view_kemo(view->x_lookat, view->shift, view->scale_factor,
					 view->rotation, view->rotate_animation);
	
	glGetDoublev(GL_MODELVIEW_MATRIX, view->mat_object_2_eye);
	return;
};
void rotate_left_view_by_struct(struct view_element *view){
	init_left_eye_model(view->shift, view->eye_separation);
	rotate_view_kemo(view->x_lookat, view->shift, view->scale_factor,
					 view->rotation, view->rotate_animation);
	
	glGetDoublev(GL_MODELVIEW_MATRIX, view->mat_object_2_eye);
	return;
};
void rotate_right_view_by_struct(struct view_element *view){
	init_right_eye_model(view->shift, view->eye_separation);
	rotate_view_kemo(view->x_lookat, view->shift, view->scale_factor,
			view->rotation, view->rotate_animation);

	glGetDoublev(GL_MODELVIEW_MATRIX, view->mat_object_2_eye);
	return;
};


void update_projection_struct(struct view_element *view){
	update_projection(view->x_lookfrom, view->nx_window, view->ny_window,
			view->aperture, view->aspect, view->near, view->far);
	return;
}
void update_left_projection_struct(struct view_element *view){
	update_projection_left(view->x_lookfrom, view->nx_window, view->ny_window,
						   view->aperture, view->aspect, view->near, view->far,
						   view->focal_length, view->eye_separation);
	return;
}
void update_right_projection_struct(struct view_element *view){
	update_projection_right(view->x_lookfrom, view->nx_window, view->ny_window,
							view->aperture, view->aspect, view->near, view->far,
							view->focal_length, view->eye_separation);
	return;
}



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
		dest->scale_factor[i] = origin->scale_factor[i];
		dest->rotation[i] =     origin->rotation[i];
	}
	dest->rotation[3] = origin->rotation[3];
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

void set_3d_position_to_window(int point_screen[2], GLfloat xx[3], 
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
	point_screen[1] = (int) ( (end_device[1] + ((double) nx_win) / ((double) ny_win) )
			* ((double) ny_win / 2.0) ) ;
	
	return;
}

void init_kemoview_perspective(struct view_element *view){
	view->aperture =   INITIAL_APATURE;
	
	view->near =       INITIAL_NEAR;
	view->far =        INITIAL_FAR;
	
	view->aspect = ((GLdouble) view->nx_window) / ((GLdouble) view->ny_window);
	
	view->focal_length =  INITIAL_FOCAL;
	view->eye_separation = INITIAL_EYE_SEP;
	view->eye_to_focal =  view->eye_separation / view->focal_length;
	return;
}

void set_gl_windowsize(struct view_element *view, GLint npixel_x, GLint npixel_y)
{
	view->nx_window = npixel_x;
	view->ny_window = npixel_y;
	return;
}
void send_gl_windowsize(struct view_element *view, GLint *npixel_x, GLint *npixel_y)
{
	*npixel_x = view->nx_window;
	*npixel_y = view->ny_window;
	return;
}
void update_projection_by_windowsize(struct view_element *view,
                                     GLint npixel_x, GLint npixel_y)
{
	glViewport(IZERO, IZERO, npixel_x, npixel_y);
    set_gl_windowsize(view, npixel_x, npixel_y);
	update_projection_struct(view);
    return;
}

void set_gl_retinamode(struct view_element *view, int i_retina)
{
    view->iflag_retina = i_retina;
    return;
}


void set_gl_rotation_parameter(struct view_element *view, GLdouble rot_vect[4])
{
	int i;
	for (i=0;i<4;i++) {view->rotation[i] = rot_vect[i];};
	return;
}

void set_gl_dragging_rotation(struct view_element *view, GLdouble rot_vect[4])
{
	int i;
	for (i=0;i<4;i++) {view->rotate_dragging[i] = rot_vect[i];};
	return;
}

void set_gl_animation_rotation(struct view_element *view, GLdouble rot_vect[4])
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
	view->rotate_animation[0] = (GLdouble) int_degree;
}

void set_gl_shift_vector(struct view_element *view, GLdouble position[3])
{
	int i;
	for (i=0;i<3;i++) {view->shift[i] =      position[i];};
	for (i=0;i<3;i++) {view->x_lookfrom[i] =-position[i];};
	return;
}

void set_gl_scalar_scale_factor(struct view_element *view, GLdouble scale_s)
{
	view->scale_factor[0] = scale_s;
	view->scale_factor[1] = scale_s;
	view->scale_factor[2] = scale_s;
	return;
};

void set_gl_projection_aperture(struct view_element *view, GLdouble aperture_s)
{
	view->aperture = aperture_s;
	return;
};

void set_gl_stereo_parameter(struct view_element *view, GLdouble focus, GLdouble eye_sep)
{
	view->focal_length =   focus;
	view->eye_separation = eye_sep;
	view->eye_to_focal =  view->eye_separation / view->focal_length;
	return;
};


void send_gl_rotation_parameter(struct view_element *view, GLdouble rot_vect[4])
{
	int i;
	for (i=0;i<4;i++) {rot_vect[i] = view->rotation[i];};
	return;
}

void send_gl_dragging_rotation(struct view_element *view, GLdouble rot_vect[4])
{
	int i;
	for (i=0;i<4;i++) {rot_vect[i] = view->rotate_dragging[i];};
	return;
}

void send_gl_animation_rotation(struct view_element *view, GLdouble rot_vect[4])
{
	int i;
	for (i=0;i<4;i++) {rot_vect[i] = view->rotate_animation[i];};
	return;
}

void send_gl_shift_vector(struct view_element *view, GLdouble position[3])
{
	int i;
	for (i=0;i<3;i++) {position[i] = view->shift[i];};
	return;
}

void send_gl_lookat_vector(struct view_element *view, GLdouble position[3])
{
	int i;
	for (i=0;i<3;i++) {position[i] = view->x_lookat[i];};
	return;
}

void send_scalar_scale_factor(struct view_element *view, GLdouble scale[1])
{
	scale[0] = (view->scale_factor[0] + view->scale_factor[1]
			+ view->scale_factor[2]) / 3.0;
	return;
};

void send_gl_projection_aperture(struct view_element *view, GLdouble *aperture_s)
{
	*aperture_s = view->aperture;
	return;
};
void send_gl_projection_parameters(struct view_element *view, GLdouble *aperture_s,
								   GLdouble *near_s, GLdouble *far_s, GLdouble *aspect_s)
{
	*aperture_s = view->aperture;
	*near_s =     view->near;
	*far_s =      view->far;
	*aspect_s =   view->aspect;
	return;
};

GLdouble send_gl_stereo_parameters(struct view_element *view){
	return view->focal_length;
};
GLdouble send_gl_stereo_eyeseparation(struct view_element *view){
	return view->eye_separation;
};


/* called with the start position and the window origin + size */
void gl_startTrackball(GLdouble x, GLdouble y, struct view_element *view){
	startTrackball_c(x, y, ZERO, ZERO, view->nx_window, -view->ny_window);
	return;
};
/* calculated rotation based on current mouse position */
void gl_rollToTrackball(GLdouble x, GLdouble y, struct view_element *view){
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




void gl_mousedolly_struct(struct view_element *view, GLdouble start[2], 
			GLdouble x_dolly, GLdouble y_dolly){
	GLdouble dolly = (start[1] - y_dolly) * view->shift[2] / 300.0f;
	view->shift[2] -= dolly;
	 /* do not let z = 0.0 */
	if (view->shift[2] == 0.0) view->shift[2] = -0.0001;
	view->x_lookfrom[2] = -view->shift[2];
	start[0] = (int) x_dolly;
	start[1] = (int) y_dolly;
	return;
}

void gl_mousepan_struct(struct view_element *view, GLdouble start[2], 
			GLdouble x_pan, GLdouble y_pan){
	GLdouble panX = (start[0] - x_pan) / (1200.0f / view->shift[2]);
	GLdouble panY = (start[1] - y_pan) / (1200.0f / view->shift[2]);
	view->shift[0] += panX;
	view->shift[1] -= panY;
	view->x_lookfrom[0] = -view->shift[0];
	view->x_lookfrom[1] = -view->shift[1];
	start[0] = x_pan;
	start[1] = y_pan;
}


void gl_zooming_struct(struct view_element *view, GLdouble wheelDelta){
	GLdouble deltaAperture = wheelDelta * -view->aperture / 200.0f;
	view->aperture += deltaAperture;
	/* do not let aperture <= 0.1 and  aperture >= 180*/
	if (view->aperture < 0.1)   view->aperture = 0.1;
	if (view->aperture > 179.9) view->aperture = 179.9;
	
	update_projection_struct(view);
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
	reset_to_init_angle(view);
	
	view->scale_factor[0] = ONE;
	view->scale_factor[1] = ONE;
	view->scale_factor[2] = ONE;
	
	view->x_lookat[0] = ZERO;
	view->x_lookat[1] = ZERO;
	view->x_lookat[2] = ZERO;
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


void set_3d_position_to_window_d(int point_screen[2], GLdouble xx[3], 
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
