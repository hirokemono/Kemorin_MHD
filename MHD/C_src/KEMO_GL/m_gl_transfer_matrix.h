
/* m_gl_transfer_matrix.h */

#ifndef M_GL_TRANSFER_MATRIX_
#define M_GL_TRANSFER_MATRIX_

#include<stdio.h>
#include<math.h>

#include "kemoviewer_param_c.h"
#include "trackball.h"

#define INITIAL_APATURE    10
#define INITIAL_ASPECT      1
#define INITIAL_NEAR        0.02
#define INITIAL_FAR      1000

#define INITIAL_FOCAL      20
#define INITIAL_EYE_SEP  0.1

#define NSIZE_GL_BUFFER  32768

struct buffer_for_gl{
	GLfloat xyz[4*NSIZE_GL_BUFFER][3];
	GLfloat xy[4*NSIZE_GL_BUFFER][2];
	GLfloat norm[4*NSIZE_GL_BUFFER][3];
	GLfloat rgba[4*NSIZE_GL_BUFFER][4];
};

struct view_element{
	GLint gl_drawID;
	
    int   iflag_write_ps;
    int   iflag_retina;
	GLint nx_window, ny_window;
	
	GLdouble x_lookfrom[3];
	GLdouble x_lookat[3];
	GLdouble shift[3];
	GLdouble rotation[4];
	GLdouble r_max;
	GLdouble iso_scale;
	GLdouble max_point[3];
	GLdouble min_point[3];
	
	GLdouble rotate_dragging[4];
	GLdouble rotate_animation[4];
	
	GLdouble mat_object_2_eye[16];
	GLdouble mat_eye_2_clip[16];
	
	GLdouble aperture; /*camera aperture*/
	GLdouble aspect; /*camera aspect ratio*/
	GLdouble near; /*camera near distance*/
	GLdouble far; /*camera far   distance*/
    
	GLdouble rotpoint[3];   /*rotation point*/
    
	GLdouble eye_separation; /*eye separation for streo view*/
	GLdouble focal_length;   /*focal length for streo view*/
	GLdouble eye_to_focal;   /*ratio of eye sparation to focal length*/
    
	/* spin for Animation */
	GLdouble rRot_animate[3];
	GLdouble rVel_animate[3];
	GLdouble rAccel_animate[3];
    
    /* Shader object */
    GLuint PhongGl2Program;
    GLuint GouraudGl2Program;


	int iflag_core_profile;
	int iflag_shading_profile;
	GLuint index_modelViewMatrix;
	GLuint index_ProjectionMatrix;
};

/* prototypes */

void identity_glmat_c(GLdouble mat[16]);

void perspectiveGL(GLdouble fovY, GLdouble aspect, GLdouble zNear, GLdouble zFar);
void orthogonalGL(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top,
			GLdouble near, GLdouble far);

void set_view_by_identity();
void modify_view_by_struct(struct view_element *view);
void modify_left_view_by_struct(struct view_element *view);
void modify_right_view_by_struct(struct view_element *view);

void rotate_view_by_struct(struct view_element *view);
void rotate_left_view_by_struct(struct view_element *view);
void rotate_right_view_by_struct(struct view_element *view);

void update_projection_struct(struct view_element *view);
void update_left_projection_struct(struct view_element *view);
void update_right_projection_struct(struct view_element *view);

void copy_lookatpoint_struct(struct view_element *origin, struct view_element *dest);
void copy_viewmatrix_struct(struct view_element *origin, struct view_element *dest);



void set_position_in_model(struct view_element *view, int nnod,
                           double **xx, double **end_eye);
void set_distance_in_model(struct view_element *view, int nnod,
                           double **xx, double *z_eye);
void set_3d_position_to_window(int point_screen[2], GLfloat xx[3], 
                               int nx_win, int ny_win, struct view_element *view);

void init_kemoview_perspective(struct view_element *view);

void set_gl_windowsize(struct view_element *view, GLint npixel_x, GLint npixel_y);
void send_gl_windowsize(struct view_element *view, GLint *npixel_x, GLint *npixel_y);
void update_projection_by_windowsize(struct view_element *view,
                                     GLint npixel_x, GLint npixel_y);

void set_gl_retinamode(struct view_element *view, int i_retina);


void set_gl_rotation_parameter(struct view_element *view, GLdouble rot_vect[4]);
void set_gl_dragging_rotation(struct view_element *view, GLdouble rot_vect[4]);
void set_gl_animation_rotation(struct view_element *view, GLdouble rot_vect[4]);
void set_gl_animation_rot_axis(struct view_element *view, int iaxis);
void set_gl_animation_rot_angle(struct view_element *view, int int_degree);
void set_gl_shift_vector(struct view_element *view, GLdouble position[3]);
void set_gl_scalar_scale_factor(struct view_element *view, GLdouble scale_s);
void set_gl_projection_aperture(struct view_element *view, GLdouble aperture_s);
void set_gl_stereo_parameter(struct view_element *view, GLdouble focus, GLdouble eye_sep);

void send_gl_rotation_parameter(struct view_element *view, GLdouble rot_vect[4]);
void send_gl_dragging_rotation(struct view_element *view, GLdouble rot_vect[4]);
void send_gl_animation_rotation(struct view_element *view, GLdouble rot_vect[4]);
void send_gl_shift_vector(struct view_element *view, GLdouble position[3]);
void send_gl_lookat_vector(struct view_element *view, GLdouble position[3]);
GLdouble send_scalar_scale_factor(struct view_element *view);
GLdouble send_gl_projection_aperture(struct view_element *view);
void send_gl_projection_parameters(struct view_element *view, GLdouble *aperture_s,
								   GLdouble *near_s, GLdouble *far_s, GLdouble *aspect_s);

GLdouble send_gl_stereo_parameters(struct view_element *view);
GLdouble send_gl_stereo_eyeseparation(struct view_element *view);


/* called with the start position and the window origin + size */
void gl_startTrackball(GLdouble x, GLdouble y, struct view_element *view);
/* calculated rotation based on current mouse position */
void gl_rollToTrackball(GLdouble x, GLdouble y, struct view_element *view);
/* add a GL rotation (dA) to an existing GL rotation (A) */
void gl_drag_addToRotationTrackball(struct view_element *view);


void gl_mousedolly_struct(struct view_element *view, GLdouble start[2],
                          GLdouble x_dolly, GLdouble y_dolly);
void gl_mousepan_struct(struct view_element *view, GLdouble start[2], 
                        GLdouble x_pan, GLdouble y_pan);
void gl_zooming_struct(struct view_element *view, GLdouble wheelDelta);

void reset_to_init_angle(struct view_element *view);
void reset_all_view_parameter(struct view_element *view);


void view_for_xy_plane(struct view_element *view);
void view_for_yz_plane(struct view_element *view);
void view_for_xz_plane(struct view_element *view);


void set_3d_position_to_window_d(int point_screen[2], GLdouble xx[3], 
                                 int nx_win, int ny_win, struct view_element *view);

#endif
