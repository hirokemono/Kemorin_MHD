
/* kemoviewer.h*/

#ifndef KEMOVIEWER__
#define KEMOVIEWER__


#ifdef __APPLE__
#include<OpenGL/gl.h>
#include<OpenGL/glu.h>
#else
#include<GL/gl.h>
#include<GL/glu.h>
#endif

#ifdef FC_NAME_LOWER_USCORE
#define open_wt_rawfile           open_wt_rawfile_
#define open_ad_rawfile           open_ad_rawfile_
#define open_rd_rawfile           open_rd_rawfile_
#define close_rawfile             close_rawfile_
#define rawread_f                 rawread_f_
#define rawwrite_f                rawwrite_f_
#define rawseek_go_fwd_f          rawseek_go_fwd_f_

#define open_wt_gzfile            open_wt_gzfile_
#define open_ad_gzfile            open_ad_gzfile_
#define open_rd_gzfile            open_rd_gzfile_
#define close_gzfile              close_gzfile_
#define check_gzfile_eof          check_gzfile_eof_
#define write_compress_txt        write_compress_txt_
#define write_compress_txt_nolf   write_compress_txt_nolf_
#define gzseek_go_fwd_f           gzseek_go_fwd_f_
#define gzread_f                  gzread_f_
#define gzwrite_f                 gzwrite_f_
#define get_one_line_from_gz      get_one_line_from_gz_
#define compress_file             compress_file_
#define decompress_file           decompress_file_
#define gzip_defleat_once         gzip_defleat_once_
#define gzip_defleat_begin        gzip_defleat_begin_
#define gzip_defleat_cont         gzip_defleat_cont_
#define gzip_defleat_last         gzip_defleat_last_
#define gzip_infleat_once         gzip_infleat_once_
#define gzip_infleat_begin        gzip_infleat_begin_
#define gzip_infleat_cont         gzip_infleat_cont_
#define gzip_infleat_last         gzip_infleat_last_

#define write_png_rgba_c          write_png_rgba_c_
#define write_png_rgb_c           write_png_rgb_c_

#define kemo_fftw_plan_dft_r2c_1d    kemo_fftw_plan_dft_r2c_1d_
#define kemo_fftw_plan_dft_c2r_1d    kemo_fftw_plan_dft_c2r_1d_
#define kemo_fftw_plan_many_dft_r2c  kemo_fftw_plan_many_dft_r2c_
#define kemo_fftw_plan_many_dft_c2r  kemo_fftw_plan_many_dft_c2r_
#define kemo_fftw_destroy_plan       kemo_fftw_destroy_plan_
#define kemo_fftw_cleanup            kemo_fftw_cleanup_
#define kemo_fftw_execute            kemo_fftw_execute_
#define kemo_fftw_execute_dft_r2c    kemo_fftw_execute_dft_r2c_
#define kemo_fftw_execute_dft_c2r    kemo_fftw_execute_dft_c2r_

#elif FC_NAME_UPPER
#define open_wt_rawfile           OPEN_WT_RAWFILE
#define open_ad_rawfile           OPEN_AD_RAWFILE
#define open_rd_rawfile           OPEN_RD_RAWFILE
#define close_rawfile             CLOSE_RAWFILE
#define rawread_f                 RAWREAD_F
#define rawwrite_f                RAWWRITE_F
#define rawseek_go_fwd_f          RAWSEEK_GO_FWD_F

#define open_wt_gzfile            OPEN_WT_GZFILE
#define open_ad_gzfile            OPEN_AD_GZFILE
#define open_rd_gzfile            OPEN_RD_GZFILE
#define close_gzfile              CLOSE_GZFILE
#define check_gzfile_eof          CHECK_GZFILE_EOF
#define write_compress_txt        WRITE_COMPRESS_TXT
#define write_compress_txt_nolf   WRITE_COMPRESS_TXT_NOLF
#define gzseek_go_fwd_f           GZSEEK_GO_FWD_F
#define gzread_f                  GZREAD_F
#define gzwrite_f                 GZWRITE_F
#define get_one_line_from_gz      GET_ONE_LINE_FROM_GZ
#define compress_file             COMPRESS_FILE
#define decompress_file           DECOMPRESS_FILE
#define gzip_defleat_once         GZIP_DEFLEAT_ONCE
#define gzip_defleat_begin        GZIP_DEFLEAT_BEGIN
#define gzip_defleat_cont         GZIP_DEFLEAT_CONT
#define gzip_defleat_last         GZIP_DEFLEAT_LAST
#define gzip_infleat_once         GZIP_INFLEAT_ONCE
#define gzip_infleat_begin        GZIP_INFLEAT_BEGIN
#define gzip_infleat_cont         GZIP_INFLEAT_CONT
#define gzip_infleat_last         GZIP_INFLEAT_LAST

#define write_png_rgba_c          WRITE_PNG_RGBA_C
#define write_png_rgb_c           WRITE_PNG_RGB_C

#define kemo_fftw_plan_dft_r2c_1d    KEMO_FFTW_PLAN_DFT_R2C_1D
#define kemo_fftw_plan_dft_c2r_1d    KEMO_FFTW_PLAN_DFT_C2R_1D
#define kemo_fftw_plan_many_dft_r2c  KEMO_FFTW_PLAN_MANY_DFT_R2C
#define kemo_fftw_plan_many_dft_c2r  KEMO_FFTW_PLAN_MANY_DFT_C2R
#define kemo_fftw_destroy_plan       KEMO_FFTW_DESTROY_PLAN
#define kemo_fftw_cleanup            KEMO_FFTW_CLEANUP
#define kemo_fftw_execute            KEMO_FFTW_EXECUTE
#define kemo_fftw_execute_dft_r2c    KEMO_FFTW_EXECUTE_DFT_R2C
#define kemo_fftw_execute_dft_c2r    KEMO_FFTW_EXECUTE_DFT_C2R

#elif FC_NAME_UPPER_STDCALL
#define open_wt_rawfile           OPEN_WT_RAWFILE
#define open_ad_rawfile           OPEN_AD_RAWFILE
#define open_rd_rawfile           OPEN_RD_RAWFILE
#define close_rawfile             CLOSE_RAWFILE
#define rawread_f                 RAWREAD_F
#define rawwrite_f                RAWWRITE_F
#define rawseek_go_fwd_f          RAWSEEK_GO_FWD_F

#define open_wt_gzfile            OPEN_WT_GZFILE
#define open_ad_gzfile            OPEN_AD_GZFILE
#define open_rd_gzfile            OPEN_RD_GZFILE
#define close_gzfile              CLOSE_GZFILE
#define check_gzfile_eof          CHECK_GZFILE_EOF
#define write_compress_txt        WRITE_COMPRESS_TXT
#define write_compress_txt_nolf   WRITE_COMPRESS_TXT_NOLF
#define gzseek_go_fwd_f           GZSEEK_GO_FWD_F
#define gzread_f                  GZREAD_F
#define gzwrite_f                 GZWRITE_F
#define get_one_line_from_gz      GET_ONE_LINE_FROM_GZ
#define compress_file             COMPRESS_FILE
#define decompress_file           DECOMPRESS_FILE
#define gzip_defleat_once         GZIP_DEFLEAT_ONCE
#define gzip_defleat_begin        GZIP_DEFLEAT_BEGIN
#define gzip_defleat_cont         GZIP_DEFLEAT_CONT
#define gzip_defleat_last         GZIP_DEFLEAT_LAST
#define gzip_infleat_once         GZIP_INFLEAT_ONCE
#define gzip_infleat_begin        GZIP_INFLEAT_BEGIN
#define gzip_infleat_cont         GZIP_INFLEAT_CONT
#define gzip_infleat_last         GZIP_INFLEAT_LAST

#define write_png_rgba_c          WRITE_PNG_RGBA_C
#define write_png_rgb_c           WRITE_PNG_RGB_C

#define kemo_fftw_plan_dft_r2c_1d    KEMO_FFTW_PLAN_DFT_R2C_1D
#define kemo_fftw_plan_dft_c2r_1d    KEMO_FFTW_PLAN_DFT_C2R_1D
#define kemo_fftw_plan_many_dft_r2c  KEMO_FFTW_PLAN_MANY_DFT_R2C
#define kemo_fftw_plan_many_dft_c2r  KEMO_FFTW_PLAN_MANY_DFT_C2R
#define kemo_fftw_destroy_plan       KEMO_FFTW_DESTROY_PLAN
#define kemo_fftw_cleanup            KEMO_FFTW_CLEANUP
#define kemo_fftw_execute            KEMO_FFTW_EXECUTE
#define kemo_fftw_execute_dft_r2c    KEMO_FFTW_EXECUTE_DFT_R2C
#define kemo_fftw_execute_dft_c2r    KEMO_FFTW_EXECUTE_DFT_C2R

#else
#define open_wt_rawfile           open_wt_rawfile
#define open_ad_rawfile           open_ad_rawfile
#define open_rd_rawfile           open_rd_rawfile
#define close_rawfile             close_rawfile
#define rawread_f                 rawread_f
#define rawwrite_f                rawwrite_f
#define rawseek_go_fwd_f          rawseek_go_fwd_f

#define open_wt_gzfile            open_wt_gzfile
#define open_ad_gzfile            open_ad_gzfile
#define open_rd_gzfile            open_rd_gzfile
#define close_gzfile              close_gzfile
#define check_gzfile_eof          check_gzfile_eof
#define write_compress_txt        write_compress_txt
#define write_compress_txt_nolf   write_compress_txt_nolf
#define gzseek_go_fwd_f           gzseek_go_fwd_f
#define gzread_f                  gzread_f
#define gzwrite_f                 gzwrite_f
#define get_one_line_from_gz      get_one_line_from_gz
#define compress_file             compress_file
#define decompress_file           decompress_file
#define gzip_defleat_once         gzip_defleat_once
#define gzip_defleat_begin        gzip_defleat_begin
#define gzip_defleat_cont         gzip_defleat_cont
#define gzip_defleat_last         gzip_defleat_last
#define gzip_infleat_once         gzip_infleat_once
#define gzip_infleat_begin        gzip_infleat_begin
#define gzip_infleat_cont         gzip_infleat_cont
#define gzip_infleat_last         gzip_infleat_last

#define write_png_rgba_c          write_png_rgba_c
#define write_png_rgb_c           write_png_rgb_c

#define kemo_fftw_plan_dft_r2c_1d    kemo_fftw_plan_dft_r2c_1d
#define kemo_fftw_plan_dft_c2r_1d    kemo_fftw_plan_dft_c2r_1d
#define kemo_fftw_plan_many_dft_r2c  kemo_fftw_plan_many_dft_r2c
#define kemo_fftw_plan_many_dft_c2r  kemo_fftw_plan_many_dft_c2r
#define kemo_fftw_destroy_plan       kemo_fftw_destroy_plan
#define kemo_fftw_cleanup            kemo_fftw_cleanup
#define kemo_fftw_execute            kemo_fftw_execute
#define kemo_fftw_execute_dft_r2c    kemo_fftw_execute_dft_r2c
#define kemo_fftw_execute_dft_c2r    kemo_fftw_execute_dft_c2r
#endif

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

#define VIEW_3D        0
#define VIEW_STEREO    1
#define VIEW_MAP       2
#define VIEW_XY        3
#define VIEW_XZ        4
#define VIEW_YZ        5
#define RESET         10

#define AXIS_TOGGLE          1
#define SHADING_SWITCH       2
#define POLYGON_SWITCH       3
#define SET_NODE_SIZE        4
#define SET_DISTANCE_DOMAIN  5
#define OUTPUT_V_MATRIX      6
#define INPUT_V_MATRIX      16
#define COASTLINE_SWITCH     7
#define SPHEREGRID_SWITCH    8
#define SET_COAST_RADIUS     9

#define MESH_OFF          0
#define SURFNOD_TOGGLE    1
#define SURFSOLID_TOGGLE  2
#define SURFGRID_TOGGLE   3

#define PSFSOLID_TOGGLE      1
#define PSFGRID_TOGGLE       2
#define ZEROGRID_TOGGLE      3
#define ISET_RANGE           4
#define ISET_NLINE           5
#define ISET_PSF_OPACITY     7
#define COLORBAR_TOGGLE      8
#define PSF_POLYGON_SWITCH   9

#define RAINBOW_PSF_SURF    10
#define WHITE_PSF_SURF      11
#define TEXTURE_PSF_SURF    12
#define SGL_COLOR_PSF_SURF  13

#define RAINBOW_PSF_LINE    20
#define BLACK_PSF_LINE      21
#define WHITE_PSF_LINE      22

#define PSF_OFF             30
#define PSFVECT_TOGGLE      40
#define PSFREFV_TOGGLE      41
#define WHITE_PSF_VECT      42
#define RAINBOW_PSF_VECT    43
#define ISET_PSF_VEC_INC    44
#define ISET_PSF_REFVECT    45
#define ISET_PSF_V_THICK    46
#define PSFTANVEC_TOGGLE    47
#define WRITE_CMAP          50
#define READ_CMAP           51

#define ADD_PSF_COLOR       61
#define MODIFY_PSF_COLOR    62
#define DELETE_PSF_COLOR    63
#define ADD_PSF_OPACITY     64
#define MODIFY_PSF_OPACITY  65
#define DELETE_PSF_OPACITY  66

#define PSF_NOTHING_TODO    99

#define ISET_FLINE_TYPE   11
#define ISET_FLINE_THICK  12
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

#define RAINBOW_SURFACE  0
#define WHITE_SURFACE    1
#define GREEN_SURFACE    2
#define DOMAIN_COLOR     3
#define GROUP_COLOR      4
#define SET_OPACITY      5
#define SINGLE_COLOR     6
#define TEXTURED_SURFACE 7
#define BLUE_RED_SURFACE 8

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

#define FLAT_SHADE    0
#define SMOOTH_SHADE  1

#define NORMAL_POLYGON  0
#define REVERSE_POLYGON 1

#define FULL_COMPONENT        0
#define TANGENTIAL_COMPONENT  1

#define IFLAG_LINE 0
#define IFLAG_PIPE 1

#define GRAYSCALE       0
#define RAINBOW_COLOR   1
#define SET_NUM_COLORS  2

#define MENU_HEIGHT 30
#define MENU_WIDTH  80

#define NMAX_PSF     10

#define SHUTTER_OFF  0
#define SHUTTER_ON   1
#define ANAGLYPH_OFF 0
#define ANAGLYPH_ON  1

#define IFLAG_MESH       99
#define IFLAG_SURFACES   2
#define IFLAG_LINES      1

#define IFLAG_FULL_MESH   0
#define IFLAG_SURF_MESH   1
#define IFLAG_SURF_UDT   10
#define IFLAG_SURF_UCD   11
#define IFLAG_SURF_VTD   20
#define IFLAG_SURF_VTK   21

#define IFLAG_FULL_MESH_GZ  100
#define IFLAG_SURF_MESH_GZ  101
#define IFLAG_SURF_UDT_GZ   110
#define IFLAG_SURF_UCD_GZ   111
#define IFLAG_SURF_VTD_GZ   120
#define IFLAG_SURF_VTK_GZ   121

#define IFLAG_SWAP  1

#define EPSILON  1.e-9

/*
 Kemoviewer_t: Structure to run kemoviewer in one window.
 */

struct kemoviewer_type;
struct mul_kemoviewer_type;

/* prototypes */

#ifdef __cplusplus
extern "C" {
#endif

    void allocate_kemoviwewer_struct(struct kemoviewer_type *kemoviewer_data, int iflag_dmesh);
    void allocate_single_kemoviwewer_struct(struct kemoviewer_type *kemoviewer_data);
    void allocate_kemoviwewer_pointers();
    void deallocate_kemoviwewer_pointers();
    
    int send_nlimit_load_psf();
    
    void set_single_kemoview_ID(int id_window);

    void set_current_kemoview(int id_window, struct mul_kemoviewer_type *kemoview_array);
    int send_current_kemoview();

    void draw_kemoviewer_c();
    void draw_kemoviewer_to_ps();
    void kemoviewer_initial_lighting();
    
    void init_kemoview_background_color();
    void set_kemoview_background_color(GLfloat color[4]);
    void send_background_color(GLfloat color[4]);
    
    void evolution_viewer(int istep);
    
    void write_modelview_file_glut(const char *file_name);
    void load_modelview_file_glut(const char *file_name);
    int kemoview_open_data_glut(const char *file_name);
    
    void close_mesh_view();
    int  close_psf_view();
    void close_fline_view();
    
    void set_to_pick_surface_command(const char *command);
    void send_pick_surface_command(char *command);
    
    void set_viewtype_glut(int sel);
    
    
    void draw_modified_object_distance();
    
    void set_to_coastline_radius(double radius);
    double send_coastline_radius();
    
    void set_object_property_flags(int selected, int iflag);
    int send_object_property_flags(int selected);
    int object_properties_toggle(int selected);
    
    void set_to_mesh_color_mode(int icolor);
    void set_to_num_of_color_loop(int icolor);
    
    void set_to_node_diam(double diam);
    void set_to_dist_domains(double dist);
    
    
    void set_to_domain_surface_opacity(double opacity_in);
    void set_to_ele_surface_opacity(double opacity_in);
    void set_to_surf_surface_opacity(double opacity_in);
    
    void set_domain_color_flag(int selected, int icolor);
    void set_node_grp_color_flag(int icolor);
    void set_ele_grp_color_flag(int selected, int icolor);
    void set_surf_grp_color_flag(int selected, int icolor);
    
    void set_domain_color_code(int selected, GLfloat color_code4[4]);
    void set_to_node_grp_color_code(GLfloat color_code4[4]);
    void set_ele_grp_color_code(int selected, GLfloat color_code4[4]);
    void set_surf_grp_color_code(int selected, GLfloat color_code4[4]);
    
    double send_domain_surface_opacity();
    double send_ele_surface_opacity();
    double send_surf_surface_opacity();
    
    
    int send_draw_surface_nod();
    int send_draw_surface_grid();
    int send_draw_surface_solid();
    
    void set_kemoview_mesh_draw(int selected, int iflag);
    void kemoview_mesh_draw_toggle(int selected);
    
    void set_to_draw_domain_nod(int iflag, int i);
    void set_to_draw_domain_grid(int iflag, int i);
    void set_to_draw_domain_solid(int iflag, int i);
    
    void set_to_draw_nodgrp_nod(int iflag, int i);
    int send_draw_nodgrp_nod(int i);
    
    void set_to_draw_elegrp_grid(int iflag, int i);
    int send_draw_elegrp_grid(int i);
    
    void set_to_draw_elegrp_nod(int iflag, int i);
    int send_draw_elegrp_nod(int i);
    
    void set_to_draw_elegrp_solid(int iflag, int i);
    int send_draw_elegrp_solid(int i);
    
	
	
    void set_to_draw_surfgrp_nod(int iflag, int i);
    int send_draw_surfgrp_nod(int i);
    
    void set_to_draw_surfgrp_grid(int iflag, int i);
    int send_draw_surfgrp_grid(int i);
    
    void set_to_draw_surfgrp_solid(int iflag, int i);
    int send_draw_surfgrp_solid(int i);
    
    void kemoview_nod_grp_toggle(int selected);
    
    void kemoview_ele_grp_toggle(int selected);
    void kemoview_ele_grp_nod_toggle(int selected);
    void kemoview_ele_grp_grid_toggle(int selected);
    
    void kemoview_surf_grp_toggle(int selected);
    void kemoview_surf_grp_nod_toggle(int selected);
    void kemoview_surf_grp_grid_toggle(int selected);
    
    int send_iflag_draw_mesh();
    
    int send_num_pe_sf();
    int send_ngrp_nod_sf();
    int send_ngrp_ele_sf();
    int send_ngrp_surf_sf();
    
    void send_nod_gp_name_sf(char *name, int i);
    void send_ele_gp_name_sf(char *name, int i);
    void send_surf_gp_name_sf(char *name, int i);
    
    
    int send_iflag_draw_type();
    int send_iflag_view_type();
    
    int send_num_of_color_loop();
    
    
    double send_node_diam();
    double send_dist_domains();
    
    
    void get_ext_from_file_name(const char *file_head, char *stripped_fhead, char *stripped_ext);
    void add_ext_to_file_name(const char *file_head, const char *added_ext, char *file_name);
    
    
    void set_to_text_color_code(float c_code[4]);
    void send_text_color_code(float c_code[4]);
    
    void get_kemoviewer_fliped_img(int npixel_x, int npixel_y,
                                   unsigned char *glimage, unsigned char *fliped_img);
	void write_kemoviewer_window_to_vector_file(int iflag_img, const char *fhead);

    void modify_view_kemoview();
    void rotate_kemoview();
    
    void reset_kemoviewer_to_init_angle();
    
    void set_kemoview_retinamode(int i_retina);
    void set_kemoview_windowsize(GLint npixel_x, GLint npixel_y);
    void update_projection_by_kemoviewer_size(GLint npixel_x, GLint npixel_y);
    
    void update_kemoviewer_distance();
    
    void set_kemoview_rotation_parameter(GLdouble rot_vect[4]);
    void set_kemoview_dragging_rotation(GLdouble rot_vect[4]);
    void set_kemoview_animation_rot_axis(int iaxis);
    void set_kemoview_animation_rot_angle(int int_degree);
    void set_kemoview_shift_vector(GLdouble position[3]);
    void set_kemoview_scale_factor(GLdouble scale_s);
    void set_kemoview_projection_aperture(GLdouble aperture_s);
    void set_kemoview_stereo_parameter(GLdouble focus, GLdouble eye_sep);
    
    void send_kemoview_windowsize(GLint *npixel_x, GLint *npixel_y);
    void send_kemoview_rotation_parameter(GLdouble rot_vect[4]);
    void send_kemoview_shift_vector(GLdouble position[3]);
    void send_kemoview_lookat_vector(GLdouble position[3]);
    GLdouble send_kemoview_scale_factor();
    GLdouble send_kemoview_projection_aperture();
    void send_kemoview_projection_parameters(GLdouble *aperture_s, GLdouble *near_s,
                                             GLdouble *far_s, GLdouble *aspect_s);
    GLdouble send_kemoview_stereo_parameters();
    GLdouble send_kemoview_stereo_eyeseparation();
    
    void kemoviewer_mousedolly(GLdouble start[2], GLdouble x_dolly, GLdouble y_dolly);
    void kemoviewer_mousepan(GLdouble start[2], GLdouble x_pan, GLdouble y_pan);
    void kemoviewer_zooming(GLdouble wheelDelta);
    
    void kemoview_startTrackball(GLdouble x, GLdouble y);
    /* calculated rotation based on current mouse position */
    void kemoview_rollToTrackball(GLdouble x, GLdouble y);
    /* add a GL rotation (dA) to an existing GL rotation (A) */
    void drugging_addToRotationTrackball();
    void add_kemoview_animation_rot(GLdouble dt);
    void reset_kemoviewer_animation();
    
    void set_to_stereo_shutter(int iflag);
    void set_to_iflag_anaglyph(int iflag);
    int send_stereo_shutter();
    int send_iflag_anaglyph();
    
    void draw_menubottun_glut();
    
    /* subroutines for surafces */
    void set_num_loaded_PSF(int num);
    void set_max_loaded_PSF(int num);
    void set_to_loaded_PSF_flag(int id_psf, int iflag);
    void set_to_current_PSF(int id_psf);
    int send_num_loaded_PSF();
    int send_max_loaded_PSF();
    int send_loaded_PSF_flag(int id_psf);
    int send_current_PSF();
    
    int send_current_psf_full_path_header(char *file_head, int *iflag);
    void send_current_psf_file_header(char *file_head);
    
    void set_current_psf_field_flag(int sel);
    void set_current_psf_component_flag(int sel);
    
    int send_nfield_current_psf();
    int send_ncomptot_current_psf();
    int send_ncomp_current_psf(int i);
    int send_istack_comp_current_psf(int i);
    void send_current_psf_data_name(char *name, int i);
    
    int send_iflag_draw_current_psf();
    
    int send_draw_field_current_psf();
    int send_draw_comp_id_current_psf();
    int send_draw_component_current_psf();
    int send_coordinate_id_current_psf();
    
    void set_texture_rgba_to_current_psf(int width, int height, const unsigned char *bgra_in);
    
    void set_current_psf_polygon_mode(int iflag);
    void set_current_psf_tanvec_mode(int iflag);
    
    int send_draw_current_psf_refv();
    int toggle_draw_current_psf_refv();
    
    void set_current_psf_patch_color_mode(int iflag);
    void set_current_isoline_color(int iflag);
    void set_current_n_isoline(int nlline);
    void set_current_increment_vect(int increment);
    void set_current_scale_vect(double scale);
    void set_current_vector_thick(double size);
    
    int send_current_psf_patch_color();
    int send_current_isoline_color();
    int send_current_num_isoline();
    int send_current_vector_patch_color();
    int send_current_increment_vect();
    double send_current_scale_vect();
    double send_current_vector_thick();
    
    int send_kemoview_psf_draw_flags(int selected);
    int kemoview_psf_draw_switch_select(int selected);
    
    void set_current_PSF_color_mode_id(int isel);
    int send_current_PSF_color_mode_id();
    
    double send_current_psf_data_min(int i);
    double send_current_psf_data_max(int i);
    
	void delete_current_PSF_color_idx_list(int i_delete);
	void delete_current_PSF_opacity_idx_list(int i_delete);
	void add_current_PSF_color_idx_list(double add_value, double add_color);
	void add_current_PSF_opacity_idx_list(double add_value, double add_opacity);
	
    void set_current_PSF_linear_colormap(double minvalue, double maxvalue);
    void set_current_PSF_fixed_color(double *rgba);
    void set_current_PSF_constant_opacity(double opacity);
    
    void set_current_PSF_rgb_from_value(double value, double *red, double *green, double *blue);
    double kemoview_get_PSF_opacity_at_value(double value);
    void set_current_PSF_color_point(int i_point, double value, double color);
    void set_current_PSF_opacity_point(int i_point, double value, double opacity);
    
    double kemoview_get_PSF_color_table_min();
    double kemoview_get_PSF_color_table_max();
    double kemoview_get_PSF_min_opacity();
    double kemoview_get_PSF_max_opacity();
    int kemoview_get_PSF_color_table_num();
    int kemoview_get_PSF_opacity_table_num();
    
    void kemoview_get_PSF_color_items(int i_point, double *value, double *color);
    void kemoview_get_PSF_opacity_items(int i_point, double *value, double *opacity);
    
    void kemoview_write_PSF_colormap_file(const char *file_name);
    void kemoview_read_PSF_colormap_file(const char *file_name);
	void kemoview_check_PSF_colormap_control();
    
    
    /* Subroutines for field lines */
    
    int kemoview_get_fline_file_step_prefix(char *file_head);
    void kemoview_set_fline_file_step(int istep);
    
    void kemoview_set_fline_switch(int iflag);
    void kemoview_set_fline_color_type(int iflag);
    void kemoview_set_fline_color_field(int sel);
    void kemoview_set_fline_color_component(int sel);    
    
    int kemoview_get_fline_switch();
    int kemoview_get_fline_color_num_field();
    int kemoview_get_fline_color_ncomptot();
    int kemoview_get_fline_color_num_comps(int i);
    int kemoview_get_fline_color_istack(int i);
    void kemoview_get_fline_color_data_name(char *name, int i);
    int kemoview_get_fline_color_field();
    int kemoview_get_fline_color_component();
    int kemoview_get_fline_color_data_adress();
    int kemoview_get_fline_colormode();
    
	void kemoview_set_fline_type(int iflag);
	int kemoview_get_fline_type();
	int kemoview_toggle_fline_type();
	
	void kemoview_set_fline_thickness(double thick);
	double kemoview_get_fline_thickness();
    
    double kemoview_get_fline_data_min(int i);
    double kemoview_get_fline_data_max(int i);
    
    void kemoview_set_fline_linear_colormap(double minvalue, double maxvalue);
    void kemoview_set_fline_constant_opacity(double opacity);
    
    void kemoview_realloc_fline_colormap(int num);
    void kemoview_realloc_fline_opacitymap(int num);
    
    void kemoview_get_fline_rgb_at_value(double value, double *red, double *green, double *blue);
    double kemoview_get_fline_opacity_at_value(double value);
    
    void kemoview_set_fline_color_data(int i_point, double value, double color);
    void kemoview_set_fline_opacity_data(int i_point, double value, double opacity);
    void kemoview_set_fline_color_mode_id(int isel);
    
    double kemoview_get_fline_min_color();
    double kemoview_get_fline_max_color();
    double kemoview_get_fline_min_opacity();
    double kemoview_get_fline_max_opacity();
    
    int kemoview_get_fline_color_num();
    int kemoview_get_fline_opacity_num();
    void kemoview_get_fline_color_item(int i_point, double *value, double *color);
    void kemoview_get_fline_opacity_item(int i_point, double *value, double *opacity);
    
    void kemoview_write_fline_colormap_file(const char *file_name);
    void kemoview_read_fline_colormap_file(const char *file_name);

    
/** Load texture onto current sectioning image */
    void kemoview_set_texture_to_PSF(int img_fmt, const char *img_head);

/** Set Image file format by ID */
    int kemoview_set_image_file_format_id(char *image_fmt);
/** Write Kemoviwer window image to file without step number */
    void kemoview_write_window_to_file(int iflag_img, const char *fhead);
/** Write Kemoviwer window image to file with step number */
    void kemoview_write_window_to_file_w_step(int iflag_img, int istep, const char *fhead);
#ifdef __cplusplus
}
#endif

#endif
