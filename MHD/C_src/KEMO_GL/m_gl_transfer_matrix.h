
/* m_gl_transfer_matrix.h */

#ifndef M_GL_TRANSFER_MATRIX_
#define M_GL_TRANSFER_MATRIX_

#include<stdio.h>
#include<stdlib.h>
#include<math.h>

#include "kemoviewer_param_c.h"
#include "trackball.h"

#define INITIAL_APATURE    10
#define INITIAL_ASPECT      1
#define INITIAL_NEAR        0.02
#define INITIAL_FAR      1000

#define INITIAL_FOCAL      2
#define INITIAL_EYE_SEP  0.1
#define INITIAL_SEP_AGL  35

struct view_element{
	int iflag_retina;
    int iflag_draw_mode;
	int iflag_view_type;
	int shading_mode;
	
	int gl_drawID;
	int nx_window, ny_window;
    int nx_frame, ny_frame;
	
	double x_lookfrom[3];
	double x_lookat[3];
	double shift[3];
	double rotation[4];
	double r_max;
	double iso_scale;
	double max_point[3];
	double min_point[3];
	
	double rotate_dragging[4];
	double rotate_animation[4];
	
	double mat_object_2_eye[16];
	double mat_eye_2_clip[16];
	
	double aperture; /*camera aperture*/
	double aspect; /*camera aspect ratio*/
	double near; /*camera near distance*/
	double far; /*camera far   distance*/
    
	double rotpoint[3];   /*rotation point*/
    
    int iflag_quilt_mode;        /*Integer flag to set if eye separation is updated */
    int num_raws;
    int num_columns;
    int num_views;
    
	double eye_separation_angle; /*eye separation angle for quilt format*/
	double eye_separation; /*eye separation for streo view*/
	double focal_length;   /*focal length for streo view*/
    
	/* spin for Animation */
	double rRot_animate[3];
	double rVel_animate[3];
	double rAccel_animate[3];
};

/* prototypes */

void identity_glmat_c(double mat[16]);

double * orthogonal_projection_mat_c(double left, double right,
                                     double bottom, double top,
                                     double near, double far);

void set_view_by_identity(void);
void modify_view_by_struct(struct view_element *view);
void modify_left_view_by_struct(struct view_element *view);
void modify_right_view_by_struct(struct view_element *view);
void modify_step_view_by_struct(int istep, struct view_element *view);

void set_view_for_message(struct view_element *view);

void set_projection_by_identity(void);
void init_projection_struct(struct view_element *view);
void update_projection_struct(struct view_element *view);
void update_left_projection_struct(struct view_element *view);
void update_right_projection_struct(struct view_element *view);
void update_step_projection_struct(int istep, struct view_element *view);

void copy_lookatpoint_struct(struct view_element *origin, struct view_element *dest);
void copy_viewmatrix_struct(struct view_element *origin, struct view_element *dest);



void set_position_in_model(struct view_element *view, int nnod,
                           double **xx, double **end_eye);
void set_distance_in_model(struct view_element *view, long nnod,
                           double **xx, double *z_eye);
void set_3d_position_to_window(int point_screen[2], double xx[3], 
                               int nx_win, int ny_win, struct view_element *view);

void init_kemoview_perspective(struct view_element *view);

void set_gl_windowsize(struct view_element *view, int npixel_x, int npixel_y,
                       int nwindow_x, int nwindow_y);
int send_gl_windowsize_x(struct view_element *view);
int send_gl_windowsize_y(struct view_element *view);
void update_projection_by_windowsize(struct view_element *view, int npixel_x, int npixel_y,
                                     int nwindow_x, int nwindow_y);

void set_gl_retinamode(struct view_element *view, int i_retina);
int send_gl_retinamode(struct view_element *view);

void set_gl_draw_mode(struct view_element *view, int imode);
int send_gl_draw_mode(struct view_element *view);

void set_gl_rotation_parameter(struct view_element *view, int i, double rot_vect);
void set_gl_dragging_rotation(struct view_element *view, double rot_vect[4]);
void set_gl_animation_rotation(struct view_element *view, double rot_vect[4]);
void set_gl_animation_rot_axis(struct view_element *view, int iaxis);
void set_gl_animation_rot_angle(struct view_element *view, int int_degree);
void set_gl_shift_vector(struct view_element *view, int i, double position);
void set_gl_scalar_scale_factor(struct view_element *view, double scale_s);
void set_gl_projection_aperture(struct view_element *view, double aperture_s);

void set_gl_focal_length(struct view_element *view, double focal);
void set_gl_eye_separation_distance(struct view_element *view, double eye_sep);
void set_gl_eye_separation_angle(struct view_element *view, double sep_angle);

void set_quilt_mode_flag(struct view_element *view, int num);
void set_quilt_image_num_raws(struct view_element *view, int num);
void set_quilt_image_num_columns(struct view_element *view, int num);
void set_quilt_image_num_views(struct view_element *view, int num);

double send_gl_rotation_parameter(struct view_element *view, int i);
void send_gl_dragging_rotation(struct view_element *view, double rot_vect[4]);
void send_gl_animation_rotation(struct view_element *view, double rot_vect[4]);
double send_gl_shift_vector(struct view_element *view, int i);
double send_gl_lookat_vector(struct view_element *view, int i);
double send_scalar_scale_factor(struct view_element *view);

double send_gl_projection_aperture(struct view_element *view);
double send_gl_projection_far(struct view_element *view);
double send_gl_projection_near(struct view_element *view);
double send_gl_projection_aspect(struct view_element *view);

double send_gl_stereo_focus(struct view_element *view);
double send_gl_stereo_eyeseparation(struct view_element *view);
double send_gl_stereo_eparation_angle(struct view_element *view);

int send_quilt_mode_flag(struct view_element *view);
int send_quilt_image_num_raws(struct view_element *view);
int send_quilt_image_num_columns(struct view_element *view);
int send_quilt_image_num_views(struct view_element *view);


/* called with the start position and the window origin + size */
void gl_startTrackball(double x, double y, struct view_element *view);
/* calculated rotation based on current mouse position */
void gl_rollToTrackball(double x, double y, struct view_element *view);
/* add a GL rotation (dA) to an existing GL rotation (A) */
void gl_drag_addToRotationTrackball(struct view_element *view);


void gl_mousedolly_struct(struct view_element *view, double start[2],
                          double x_dolly, double y_dolly);
void gl_mousepan_struct(struct view_element *view, double start[2], 
                        double x_pan, double y_pan);
void gl_zooming_struct(struct view_element *view, double wheelDelta);

void reset_to_init_angle(struct view_element *view);
void reset_all_view_parameter(struct view_element *view);


void view_for_xy_plane(struct view_element *view);
void view_for_yz_plane(struct view_element *view);
void view_for_xz_plane(struct view_element *view);


void set_3d_position_to_window_d(int point_screen[2], double xx[3], 
                                 int nx_win, int ny_win, struct view_element *view);

#endif
