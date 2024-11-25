
/* kemoviewer.h*/

#ifndef KEMOVIEWER__
#define KEMOVIEWER__


#define LENGTHBUF 4096     /* length of text buffer */

#define INBUFSIZ   65536        /*  buffer size for original data (arbitraly) */
#define OUTBUFSIZ  65536         /* buffer size for output data（arbitraly） */

#define IZERO    0
#define IONE     1
#define ITWO     2
#define ITHREE   3
#define IFOUR    4

#define ZERO     0.0
#define HALF     0.5
#define ONE      1.0
#define TEN   10.0

#define TWO_CENT   0.02
#define TWO_MILI   0.002

#define SURFACE_RENDERING     0
#define FIELDLINE_RENDERING   1
#define TRACER_RENDERING      2


#define TRIPLE_UPDATE 99
#define FULL_DRAW      0
#define MOVIE_DRAW     1
#define SIMPLE_DRAW    2
#define QUILT_DRAW     3

#define VIEW_3D        0
#define VIEW_STEREO    1
#define VIEW_MAP       2
#define VIEW_XY        3
#define VIEW_XZ        4
#define VIEW_YZ        5
#define LIGHT_CHECK   21
#define RESET         10

#define AXIS_TOGGLE             1
#define AXIS_POSITION          20
#define SHADING_SWITCH          2
#define POLYGON_SWITCH          3
#define SET_NODE_SIZE           4
#define SET_DISTANCE_DOMAIN     5
#define OUTPUT_V_MATRIX         6
#define INPUT_V_MATRIX         16
#define COASTLINE_SWITCH        7
#define SPHEREGRID_SWITCH       8
#define TANGENT_CYLINDER_SWITCH   41
#define SET_COAST_RADIUS         300
#define FILE_STEP_LABEL_SWITCH     9
#define TIME_LABEL_SWITCH         10
#define FILE_STEP_LABEL_AVAIL     11
#define TIME_LABEL_AVAIL          12

#define MESH_OFF          100
#define SURFNOD_TOGGLE    1
#define SURFSOLID_TOGGLE  2
#define SURFGRID_TOGGLE   3

#define LOWER_LEFT_AXIS   1
#define CENTER_AXIS       0


#define DOMAIN_FLAG     0
#define NODE_GRP_FLAG   1
#define ELEM_GRP_FLAG   2
#define SURF_GRP_FLAG   3


#define PSFSOLID_TOGGLE      1
#define PSFGRID_TOGGLE       2
#define ZEROGRID_TOGGLE      3
#define ISET_RANGE           4
#define ISET_NLINE           5
#define ISET_PSF_OPACITY     7
#define PSF_POLYGON_SWITCH   9
#define ISET_COLORMAP       31
#define ISET_NUM_COLOR      32
#define ISET_NUM_OPACITY    33
#define ISET_VECTOR_COLOR   41

#define RAINBOW_PSF_SURF    10
#define WHITE_PSF_SURF      11
#define TEXTURE_PSF_SURF    12
#define SGL_COLOR_PSF_SURF  13
#define CHANGE_PSF_COLOR    14

#define RAINBOW_PSF_LINE    20
#define BLACK_PSF_LINE      21
#define WHITE_PSF_LINE      22

#define PSF_OFF             30
#define PSFREFV_TOGGLE      41
#define ISET_PSF_VEC_INC    44
#define PSFTANVEC_TOGGLE    47

#define ADD_PSF_COLOR       61
#define ADD_PSF_OPACITY     64

#define PSF_NOTHING_TODO    99

#define ISET_FLINE_TYPE   11
#define ISET_FLINE_THICK  200
#define FLINE_OFF         50

#define SAVE_EPS      10
#define SAVE_PS       11
#define SAVE_PDF      20
#define SAVE_PNG       1
#define SAVE_BMP       2
#define SAVE_PPM_B     3
#define SAVE_PPM_A     4
#define SAVE_TIFF      5
#define SAVE_QT_MOVIE  999
#define NO_SAVE_FILE   0
#define SAVE_UNDEFINED  -1

#define RGBA_COLOR  1
#define RGB_COLOR   0
#define BW_ALPHA    11
#define B_AND_W     10

#define COLORED_BY_DATA  0
#define WHITE_SURFACE    1
#define GREEN_SURFACE    2
#define DOMAIN_COLOR     3
#define GROUP_COLOR      4
#define SET_OPACITY      5
#define SINGLE_COLOR     6
#define TEXTURED_SURFACE 7

#define BLACK_LINE     1
#define GREEN_LINE     2
#define RAINBOW_LINE   3
#define TWO_COLOR_LINE 4
#define TWO_GRAY_LINE  5
#define WHITE_LINE     6

#define RAINBOW_MODE    0
#define GRAYSCALE_MODE  1
#define RED_BLUE_MODE   2
#define SYM_GRAY_MODE   3
#define ORANGE_CYAN_MODE   4
#define MOLTEN_METAL_MODE  5
#define SPACE_COLOR_MODE   6

#define FLAT_SHADE    0
#define SMOOTH_SHADE  1

#define NORMAL_POLYGON   1
#define REVERSE_POLYGON -1
#define BOTH_SURFACES    0

#define FULL_COMPONENT        0
#define TANGENTIAL_COMPONENT  1

#define IFLAG_LINE 0
#define IFLAG_PIPE 1

#define GRAYSCALE       0
#define RAINBOW_COLOR   1
#define SET_NUM_COLORS  2

#define MENU_HEIGHT 32
#define MENU_WIDTH  64

#define NMAX_PSF     10

#define IFLAG_MESH       99
#define IFLAG_SURFACES   2
#define IFLAG_LINES      1

#define IFLAG_SURF_MESH   1
#define IFLAG_SURF_UDT   10
#define IFLAG_SURF_UCD   11
#define IFLAG_SURF_VTD   20
#define IFLAG_SURF_VTK   21
#define IFLAG_SURF_SDT   30
#define IFLAG_PSF_BIN    31

#define IFLAG_SURF_MESH_GZ  101
#define IFLAG_SURF_UDT_GZ   110
#define IFLAG_SURF_UCD_GZ   111
#define IFLAG_SURF_VTD_GZ   120
#define IFLAG_SURF_VTK_GZ   121
#define IFLAG_SURF_SDT_GZ   130
#define IFLAG_PSF_BIN_GZ    131

#define ISET_ROTATE     0
#define ISET_SHIFT      1
#define ISET_VWPOINT    2
#define ISET_SCALE      3

#define ISET_APERTURE  10
#define ISET_NEAR      11
#define ISET_FAR       12
#define ISET_ASPECT    13

#define ISET_FOCUS     21
#define ISET_EYESEP    22
#define ISET_EYEAGL    23

#define ISET_QUILT_MODE      30
#define ISET_QUILT_RAW       31
#define ISET_QUILT_COLUMN    32
#define ISET_QUILT_NUM       33

#define ISET_PIXEL_X            0
#define ISET_PIXEL_Y            1
#define ISET_ROTATE_AXIS       11
#define ISET_ROTATE_INCREMENT  12
#define ISET_DRAW_MODE         21

#define AMBIENT_FLAG    0
#define DIFFUSE_FLAG    1
#define SPECULAR_FLAG   2
#define SHINENESS_FLAG  3

#define NUM_LOADED    0
#define MAX_LOADED    1
#define SET_CURRENT   2
#define DRAW_SWITCH   3

#define ISET_COLOR_MIN      10
#define ISET_COLOR_MAX      11
#define ISET_OPACITY_MIN    20
#define ISET_OPACITY_MAX    21
#define ISET_WIDTH          31
#define ISET_PSF_REFVECT    32
#define ISET_PSF_V_THICK    41
#define ISET_VECTOR_INC     42

#define FIELD_SEL_FLAG       0
#define COMPONENT_SEL_FLAG   1
#define NUM_FIELD_FLAG       2
#define NTOT_COMPONENT_FLAG  3
#define DRAW_ADDRESS_FLAG    4
#define COORDINATE_FLAG      5
#define LIGHTING_CHECK      31
#define LINETYPE_FLAG          46
#define NUM_TUBE_CORNERS_FLAG  47
#define COASTLINE_TUBE         48
#define IMAGE_FORMAT_FLAG      51

#define EPSILON  1.e-9

#define OFF 0
#define ON  1

#define IFLAG_OFF  0
#define IFLAG_ON   1

/*
 Kemoviewer_t: Structure to run kemoviewer in one window.
 */

struct kv_string{
    char *string;
};

struct kemoviewer_type;
struct mul_kemoviewer_type;

/* prototypes */

#ifdef __cplusplus
extern "C" {
#endif
	void kemoview_alloc_kvstringitem(unsigned long length, struct kv_string *kvstring);
	void kemoview_alloc_copy_string(const char *org_string, struct kv_string *kvstring);
	struct kv_string* kemoview_alloc_kvstring(void);
	struct kv_string* kemoview_init_kvstring_by_string(const char *org_string);
	void kemoview_free_kvstring(struct kv_string *kvstring);

	struct kemoviewer_type * kemoview_allocate_single_viwewer_struct(void);
    void kemoview_deallocate_pointers(struct kemoviewer_type *kemoviewer_data);

    void kemoview_set_current_viewer_id(int id_window, struct mul_kemoviewer_type *kemoview_array);

    void kemoview_set_number_of_threads(int input, struct kemoviewer_type *kemoviewer);
    int kemoview_get_number_of_threads(struct kemoviewer_type *kemoviewer);

    void kemoview_init_background_color(struct kemoviewer_type *kemoviewer);
    void kemoview_set_background_color(float color[4],
                                       struct kemoviewer_type *kemoviewer);
    void kemoview_get_background_color(struct kemoviewer_type *kemoviewer,
                                       float color[4]);

    void kemoview_viewer_evolution(int istep, struct kemoviewer_type *kemoviewer);
    
    void kemoview_write_modelview_file(struct kv_string *filename,
                                       struct kemoviewer_type *kemoviewer);
    void kemoview_load_modelview_file(struct kv_string *filename,
                                      struct kemoviewer_type *kemoviewer);
    
    int kemoview_open_data(struct kv_string *filename,
                           struct kemoviewer_type *kemoviewer);

    void kemoview_close_mesh_view(struct kemoviewer_type *kemoviewer);
    int  kemoview_close_PSF_view(struct kemoviewer_type *kemoviewer);
    void kemoview_close_fieldline_view(struct kemoviewer_type *kemoviewer);
    void kemoview_close_tracer_view(struct kemoviewer_type *kemoviewer);

    void kemoview_set_viewtype(int sel, struct kemoviewer_type *kemoviewer);
    
    
    void kemoview_set_coastline_radius(double radius, struct kemoviewer_type *kemoviewer);
    double kemoview_get_coastline_radius(struct kemoviewer_type *kemoviewer);
    
    void kemoview_set_inner_core_radius(double r_ICB, struct kemoviewer_type *kemoviewer);
    double kemoview_get_inner_core_radius(struct kemoviewer_type *kemoviewer);

    void kemoview_set_object_property_flags(int selected, int iflag,
                                            struct kemoviewer_type *kemoviewer);
    int kemoview_get_object_property_flags(struct kemoviewer_type *kemoviewer, int selected);
    int kemoview_toggle_object_properties(int selected, struct kemoviewer_type *kemoviewer);
	
/* Lighting parameters */
    void kemoview_init_lighting(struct kemoviewer_type *kemoviewer);
	void kemoview_alloc_phong_light_list(int num, struct kemoviewer_type *kemoviewer);
	void kemoview_delete_phong_light_list(int i_delete, struct kemoviewer_type *kemoviewer);
	void kemoview_add_phong_light_list(int i_add, float r, float t, float p,
                                       struct kemoviewer_type *kemoviewer);
	
	void kemoview_init_phong_light_list(struct kemoviewer_type *kemoviewer);
	
	
	void kemoview_set_each_light_position(int i_point, float r, float t, float p,
                                          struct kemoviewer_type *kemoviewer);
	int kemoview_get_num_light_position(struct kemoviewer_type *kemoviewer);
	void kemoview_get_each_light_rtp(struct kemoviewer_type *kemoviewer,
                                     int i_point, float *r, float *t, float *p);
    void kemoview_get_each_light_xyz(struct kemoviewer_type *kemoviewer,
                                     int i_point, float *x, float *y, float *z);

	void kemoview_set_material_parameter(int itype, float shiness_in,
                                         struct kemoviewer_type *kemoviewer);
	float kemoview_get_material_parameter(struct kemoviewer_type *kemoviewer, int itype);
	
    
/* Parameters for Views */
    int kemoview_get_view_type_flag(struct kemoviewer_type *kemoviewer);
    
    void kemoview_add_bgra_to_quilt(struct kemoviewer_type *kemoviewer,
                                    int istep_quilt, int npix_x, int npix_y,
                                    unsigned char *glimage,
                                    unsigned char *fliped_quilt);

    void kemoview_const_buffers(struct kemoviewer_type *kemoviewer);
    void kemoview_transparent_buffers(struct kemoviewer_type *kemoviewer);
    void kemoview_fast_buffers(struct kemoviewer_type *kemoviewer);

    void kemoview_mono_viewmatrix(struct kemoviewer_type *kemoviewer);
    void kemoview_step_viewmatrix(int istep, struct kemoviewer_type *kemoviewer);
    void kemoview_left_viewmatrix(struct kemoviewer_type *kemoviewer);
    void kemoview_right_viewmatrix(struct kemoviewer_type *kemoviewer);

    void kemoviewer_reset_to_init_angle(struct kemoviewer_type *kemoviewer);
    
    void kemoview_set_retinamode(int i_retina, struct kemoviewer_type *kemoviewer);
    int kemoview_get_retinamode(struct kemoviewer_type *kemoviewer);
    
    void kemoview_set_windowsize(int npixel_x, int npixel_y,
                                 int nwindow_x, int nwindow_y,
                                 struct kemoviewer_type *kemoviewer);
    void kemoview_update_projection_by_viewer_size(int npixel_x, int npixel_y,
                                                   int nwindow_x, int nwindow_y,
                                                   struct kemoviewer_type *kemoviewer);
    void kemoview_set_message_opacity(float opacity,
                                      struct kemoviewer_type *kemoviewer);

    void kemoview_set_view_integer(int selected, int ivalue,
                                   struct kemoviewer_type *kemoviewer);

	void kemoview_set_view_parameter(int selected, int i, double value,
                                     struct kemoviewer_type *kemoviewer);
    void kemoview_set_stereo_parameter(int selected, double value,
                                       struct kemoviewer_type *kemoviewer);
    void kemoview_set_quilt_nums(int selected, int ivalue,
                                 struct kemoviewer_type *kemoviewer);

	int kemoview_get_view_integer(struct kemoviewer_type *kemoviewer,
                                  int selected);
	double kemoview_get_view_parameter(struct kemoviewer_type *kemoviewer,
                                       int selected, int i);
    int kemoview_get_quilt_nums(struct kemoviewer_type *kemoviewer,
                                int selected);

    void kemoview_mousedolly(double start[2], double x_dolly, double y_dolly,
                             struct kemoviewer_type *kemoviewer);
    void kemoview_mousepan(double start[2], double x_pan, double y_pan,
                           struct kemoviewer_type *kemoviewer);
    void kemoview_zooming(double wheelDelta, struct kemoviewer_type *kemoviewer);
    
    void kemoview_startTrackball(double x, double y,
                                 struct kemoviewer_type *kemoviewer);
    /* calculated rotation based on current mouse position */
    void kemoview_rollToTrackball(double x, double y,
                                  struct kemoviewer_type *kemoviewer);
    /* add a GL rotation (dA) to an existing GL rotation (A) */
    void kemoview_drugging_addToRotationTrackball(struct kemoviewer_type *kemoviewer);
    void kemoview_animation_add_rotation(double dt,
                                         struct kemoviewer_type *kemoviewer);
    void kemoview_reset_animation(struct kemoviewer_type *kemoviewer);

    void kemoview_set_coastline_thickness_w_exp(double value, int i_digit,
                                                struct kemoviewer_type *kemoviewer);
    void kemoview_get_coastline_thickness_w_exp(struct kemoviewer_type *kemoviewer,
                                                double *value, int *i_digit);

    void kemoview_set_axis_thickness_w_exp(double value, int i_digit,
                                           struct kemoviewer_type *kemoviewer);
    void kemoview_get_axis_thickness_w_exp(struct kemoviewer_type *kemoviewer,
                                           double *value, int *i_digit);

/* subroutines for surafces */
    int kemoview_get_PSF_maximum_load(struct kemoviewer_type *kemoviewer);
	void kemoview_set_PSF_loaded_params(int selected, int input,
                                        struct kemoviewer_type *kemoviewer);

	int kemoview_get_PSF_loaded_params(struct kemoviewer_type *kemoviewer, int selected);
    int kemoview_get_PSF_loaded_flag(struct kemoviewer_type *kemoviewer, int id_psf);
    
    void kemoview_get_full_path_file_name(struct kemoviewer_type *kemoviewer,
                                          int id_model, struct kv_string *ucd_m);
    int kemoview_get_full_path_file_prefix_step(struct kemoviewer_type *kemoviewer, int id_model,
                                                struct kv_string *psf_filehead, int *i_file_step);
    
    void kemoview_set_VIZ_field_param(int input, int id_model, int selected,
                                      struct kemoviewer_type *kemoviewer);
    int kemoview_get_VIZ_field_param(struct kemoviewer_type *kemoviewer,
                                     int id_model, int selected);
    void kemoview_set_VIZ_draw_flag(int id_model, int iflag,
                                    struct kemoviewer_type *kemoviewer);
    int kemoview_get_VIZ_draw_flags(struct kemoviewer_type *kemoviewer,
                                    int id_model);
    int kemoview_check_all_VIZ_draw_flags(struct kemoviewer_type *kemoviewer);

    long kemoview_get_VIZ_num_component(struct kemoviewer_type *kemoviewer,
                                        int id_model, int i);
    void kemoview_get_VIZ_field_name(struct kemoviewer_type *kemoviewer, int id_model,
                                     struct kv_string *colorname, int i);
    
    void kemoview_set_PSF_by_rgba_texture(int width, int height, 
                                          const unsigned char *bgra_in,
                                          struct kemoviewer_type *kemoviewer);
    
    void kemoview_set_PSF_polygon_mode(int iflag, struct kemoviewer_type *kemoviewer);
    void kemoview_set_PSF_tangential_vec_mode(int iflag, struct kemoviewer_type *kemoviewer);
    
    int kemoview_get_PSF_draw_refv(struct kemoviewer_type *kemoviewer);

    void kemoview_set_colorbar_draw_flag(int iflag, int id_model,
                                         struct kemoviewer_type *kemoviewer);
    int kemoview_get_colorbar_draw_flag(struct kemoviewer_type *kemoviewer,
                                        int id_model);

    void kemoview_set_VIZ_vector_draw_flags(int iflag, int id_model,
                                            struct kemoviewer_type *kemoviewer);
    int kemoview_get_VIZ_vector_draw_flags(struct kemoviewer_type *kemoviewer,
                                           int id_model);

    void kemoview_set_PSF_draw_flags(int iflag, int selected, 
                                     struct kemoviewer_type *kemoviewer);
	int kemoview_get_PSF_draw_flags(struct kemoviewer_type *kemoviewer,
                                    int selected);
	
    void kemoview_update_PSF_textured_id(struct kemoviewer_type *kemoviewer);
	void kemoview_set_PSF_color_param(int selected, int input,
                                      struct kemoviewer_type *kemoviewer);

    void kemoview_set_PSF_patch_color_mode(int input, struct kemoviewer_type *kemoviewer);
    void kemoview_set_VIZ_patch_color_mode(int input, int id_model,
                                           struct kemoviewer_type *kemoviewer);
    
    int kemoview_get_VIZ_patch_color_mode(struct kemoviewer_type *kemoviewer,
                                          int id_model);

    int kemoview_get_PSF_color_param(struct kemoviewer_type *kemoviewer,
                                     int selected);

    void kemoview_set_colormap_param(int id_model, int selected, int input,
                                     struct kemoviewer_type *kemoviewer);
    int kemoview_get_viz_colormap_param(struct kemoviewer_type *kemoviewer,
                                        int id_model, int selected);
    void kemoview_get_VIZ_color_RGB_value(struct kemoviewer_type *kemoviewer, int id_model,
                                          int i_point, double *value, double *color);
    void kemoview_set_VIZ_color_point(int i_point, double value, double color,
                                      int id_model, struct kemoviewer_type *kemoviewer);
    void kemoview_add_VIZ_color_list(double add_value, double add_color, int id_model,
                                     struct kemoviewer_type *kemoviewer);
    void kemoview_add_VIZ_opacity_list(double add_value, double add_opacity, int id_model,
                                       struct kemoviewer_type *kemoviewer);

	void kemoview_delete_VIZ_color_list(int i_delete, int id_model,
                                        struct kemoviewer_type *kemoviewer);
	void kemoview_delete_VIZ_opacity_list(int i_delete, int id_model,
                                          struct kemoviewer_type *kemoviewer);
	
    void kemoview_set_VIZ_opacity_data(int i_point, double value, double opacity,
                                       int id_model, struct kemoviewer_type *kemoviewer);

    void kemoview_set_VIZ_color_value_w_exp(int id_model, int selected,
                                            double value, int i_digit,
                                            struct kemoviewer_type *kemoviewer);


	void kemoview_set_each_VIZ_vector_w_exp(int selected, double value, int i_digit,
                                            int id_model, struct kemoviewer_type *kemoviewer);
    void kemoview_get_VIZ_vector_w_exp(struct kemoviewer_type *kemoviewer, int id_model,
                                       int selected, double *value, int *i_digit);

    void kemoview_get_VIZ_color_w_exp(struct kemoviewer_type *kemoviewer, int id_model,
                                      int selected, double *value, int *i_digit);
	
    void kemoview_set_VIZ_single_color(double *rgba, int id_model,
                                       struct kemoviewer_type *kemoviewer);
    void kemoview_set_constant_opacity(double opacity, int id_model,
                                       struct kemoviewer_type *kemoviewer);

    void kemoview_set_linear_colormap(double minvalue, int i_min_digit,
                                      double maxvalue, int i_max_digit,
                                      int id_model,
                                      struct kemoviewer_type *kemoviewer);

    
    void kemoview_get_PSF_rgb_at_value(struct kemoviewer_type *kemoviewer,
                                       int id_model, double value,
                                       double *red, double *green, double *blue);
    double kemoview_get_PSF_opacity_at_value(struct kemoviewer_type *kemoviewer,
                                             int id_model, double value);

	double kemoview_get_VIZ_data_range(struct kemoviewer_type *kemoviewer,
                                       int id_model, int selected, int icomp);
    double kemoview_get_VIZ_opacity_range(struct kemoviewer_type *kemoviewer,
                                          int id_model, int selected);

    void kemoview_get_PSF_color_items(struct kemoviewer_type *kemoviewer,
                                      int i_point, double *value, double *color);
    void kemoview_get_PSF_opacity_items(struct kemoviewer_type *kemoviewer, int id_model,
                                        int i_point, double *value, double *opacity);

    void kemoview_write_colormap_file(struct kv_string *filename, int id_model,
                                      struct kemoviewer_type *kemoviewer);
    void kemoview_read_colormap_file(struct kv_string *filename, int id_model,
                                     struct kemoviewer_type *kemoviewer);
    
    
    /* Subroutines for field lines */
    
    void kemoview_set_fline_file_step(int istep, struct kemoviewer_type *kemoviewer);
    
    void kemoview_set_line_type_flag(int input, struct kemoviewer_type *kemoviewer);
    int kemoview_get_line_type_flag(struct kemoviewer_type *kemoviewer);

	
/* subroutines for mesh */
    void kemoview_draw_with_modified_domain_distance(struct kemoviewer_type *kemoviewer);

    void kemoview_set_mesh_color_mode(int icolor, struct kemoviewer_type *kemoviewer);
    int kemoview_get_mesh_color_mode(struct kemoviewer_type *kemoviewer);
    void kemoview_set_num_of_color_loop(int icolor, struct kemoviewer_type *kemoviewer);
    int kemoview_get_num_of_color_loop(struct kemoviewer_type *kemoviewer);

    void kemoview_set_node_diamater(double factor, int i_digit,
                                    struct kemoviewer_type *kemoviewer);
    void kemoview_get_node_diamater(struct kemoviewer_type *kemoviewer,
                                    double *factor, int *i_digit);

    void kemoview_set_domain_distance(double dist, struct kemoviewer_type *kemoviewer);
    double kemoview_get_domain_distance(struct kemoviewer_type *kemoviewer);

    void kemoview_set_mesh_color_flag(int iflag_group, int selected, int icolor,
                                      struct kemoviewer_type *kemoviewer);
    int kemoview_get_mesh_color_flag(struct kemoviewer_type *kemoviewer,
                                     int iflag_group, int selected);
    
    void kemoview_set_mesh_color_code(int iflag_group, int selected, float color_code4[4],
                                      struct kemoviewer_type *kemoviewer);
    void kemoview_get_mesh_color_code(struct kemoviewer_type *kemoviewer,
                                      int iflag_group, int selected, float color_code4[4]);
    
    void kemoview_set_mesh_opacity(int iflag_group, double opacity_in,
                                   struct kemoviewer_type *kemoviewer);
    double kemoview_get_mesh_opacity(struct kemoviewer_type *kemoviewer,
                                     int iflag_group);
    

    void kemoview_set_mesh_draw_flag(int selected, int iflag,
                                     struct kemoviewer_type *kemoviewer);
    void kemoview_set_draw_mesh_item(int iflag_group, int selected, int igrp, int iflag,
                                     struct kemoviewer_type *kemoviewer);

    int kemoview_get_draw_mesh_flag(struct kemoviewer_type *kemoviewer);
    int kemoview_get_num_of_mesh_group(struct kemoviewer_type *kemoviewer,
                                       int iflag_group);
    int kemoview_get_draw_mesh_item(struct kemoviewer_type *kemoviewer,
                                    int iflag_group, int selected, int igrp);

    struct kv_string * kemoview_get_group_name(struct kemoviewer_type *kemoviewer,
                                               int selected, int i);

#ifdef __cplusplus
}
#endif

#endif
