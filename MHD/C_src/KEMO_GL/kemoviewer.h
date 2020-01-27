
/* kemoviewer.h*/

#ifndef KEMOVIEWER__
#define KEMOVIEWER__


#ifdef __APPLE__
#include<OpenGL/gl3.h>
#else
#include<GL/gl.h>
#endif

#ifdef FC_NAME_LOWER_USCORE
#define open_wt_rawfile           open_wt_rawfile_
#define open_ad_rawfile           open_ad_rawfile_
#define open_rd_rawfile           open_rd_rawfile_
#define close_rawfile             close_rawfile_
#define rawread_32bit_f           rawread_32bit_f_
#define rawread_64bit_f           rawread_64bit_f_
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
#define gzread_32bit_f            gzread_32bit_f_
#define gzread_64bit_f            gzread_64bit_f_
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

#define write_png_rgba_c           write_png_rgba_c_
#define write_png_rgb_c            write_png_rgb_c_
#define read_png_file_c            read_png_file_c_
#define copy_rgb_from_png_c        copy_rgb_from_png_c_
#define copy_rgba_from_png_c       copy_rgba_from_png_c_
#define copy_grayscale_from_png_c  copy_grayscale_from_png_c_
#define copy_grayalpha_from_png_c  copy_grayalpha_from_png_c_


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
#define rawread_32bit_f           RAWREAD_32BIT_F
#define rawread_64bit_f           RAWREAD_64BIT_F
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
#define gzread_32bit_f            GZREAD_32BIT_F
#define gzread_64bit_f            GZREAD_64BIT_F
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

#define write_png_rgba_c           WRITE_PNG_RGBA_C
#define write_png_rgb_c            WRITE_PNG_RGB_C
#define read_png_file_c            READ_PNG_FILE_C
#define copy_rgb_from_png_c        COPY_RGB_FROM_PNG_C
#define copy_rgba_from_png_c       COPY_RGBA_FROM_PNG_C
#define copy_grayscale_from_png_c  COPY_GRAYSCALE_FROM_PNG_C
#define copy_grayalpha_from_png_c  COPY_GRAYALPHA_FROM_PNG_C

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
#define rawread_32bit_f           RAWREAD_32BIT_F
#define rawread_64bit_f           RAWREAD_64BIT_F
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
#define gzread_32bit_f            GZREAD_32BIT_F
#define gzread_64bit_f            GZREAD_64BIT_F
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

#define write_png_rgba_c           WRITE_PNG_RGBA_C
#define write_png_rgb_c            WRITE_PNG_RGB_C
#define read_png_file_c            READ_PNG_FILE_C
#define copy_rgb_from_png_c        COPY_RGB_FROM_PNG_C
#define copy_rgba_from_png_c       COPY_RGBA_FROM_PNG_C
#define copy_grayscale_from_png_c  COPY_GRAYSCALE_FROM_PNG_C
#define copy_grayalpha_from_png_c  COPY_GRAYALPHA_FROM_PNG_C

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
#define rawread_32bit_f           rawread_32bit_f
#define rawread_64bit_f           rawread_64bit_f
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
#define gzread_32bit_f            gzread_32bit_f
#define gzread_64bit_f            gzread_64bit_f
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

#define write_png_rgba_c           write_png_rgba_c
#define write_png_rgb_c            write_png_rgb_c
#define read_png_file_c            read_png_file_c
#define copy_rgb_from_png_c        copy_rgb_from_png_c
#define copy_rgba_from_png_c       copy_rgba_from_png_c
#define copy_grayscale_from_png_c  copy_grayscale_from_png_c
#define copy_grayalpha_from_png_c  copy_grayalpha_from_png_c

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
#define SET_COAST_RADIUS   300

#define MESH_OFF          100
#define SURFNOD_TOGGLE    1
#define SURFSOLID_TOGGLE  2
#define SURFGRID_TOGGLE   3

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
#define COLORBAR_TOGGLE      8
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
#define PSFVECT_TOGGLE      40
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

#define RAINBOW_SURFACE  0
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

#define MENU_HEIGHT 32
#define MENU_WIDTH  64

#define NMAX_PSF     10

#define SHUTTER_OFF  0
#define SHUTTER_ON   1
#define ANAGLYPH_OFF 0
#define ANAGLYPH_ON  1

#define IFLAG_MESH       99
#define IFLAG_SURFACES   2
#define IFLAG_LINES      1

#define IFLAG_SURF_MESH   1
#define IFLAG_SURF_UDT   10
#define IFLAG_SURF_UCD   11
#define IFLAG_SURF_VTD   20
#define IFLAG_SURF_VTK   21

#define IFLAG_SURF_MESH_GZ  101
#define IFLAG_SURF_UDT_GZ   110
#define IFLAG_SURF_UCD_GZ   111
#define IFLAG_SURF_VTD_GZ   120
#define IFLAG_SURF_VTK_GZ   121

#define IFLAG_SWAP  1

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

#define ISET_PIXEL_X            0
#define ISET_PIXEL_Y            1
#define ISET_ROTATE_AXIS       11
#define ISET_ROTATE_INCREMENT  12
#define ISET_SHUTTER           41
#define ISET_ANAGYLYPH         42

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
#define LINETYPE_FLAG       46

#define EPSILON  1.e-9

#define OFF 0
#define ON  1

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

    void kemoview_allocate_viwewer_struct(struct kemoviewer_type *kemoviewer_data, int iflag_dmesh);
	struct kemoviewer_type * kemoview_allocate_single_viwewer_struct(void);
    void kemoview_allocate_pointers(void);
    void kemoview_deallocate_pointers(struct kemoviewer_type *kemoviewer_data);
    
    int kemoview_get_PSF_maximum_load(void);
    
    void kemoview_set_single_viewer_id(int id_window);

    void kemoview_set_current_viewer_id(int id_window, struct mul_kemoviewer_type *kemoview_array);
    int kemoview_get_current_viewer_id(void);

	void kemoview_draw_fast_gl3(void);
	
    void kemoview_init_lighting(void);
    
	void kemoview_orthogonalGL(double left, double right, double bottom, double top,
							   double near, double far);
	void kemoview_indentity_projectionmatrix(void);
	void kemoview_indentity_viewmatrix(void);
	void kemoview_message_viewmatrix(void);
	
    void kemoview_init_background_color(void);
    void kemoview_set_background_color(float color[4]);
    void kemoview_get_background_color(float color[4]);
    
    void kemoview_viewer_evolution(int istep);
    
    void kemoview_write_modelview_file(struct kv_string *filename);
    void kemoview_load_modelview_file(struct kv_string *filename);
    
    int kemoview_set_data_format_flag(struct kv_string *filename, 
                                      struct kv_string *stripped_prefix, struct kv_string *stripped_ext);
    int kemoview_open_data(struct kv_string *filename);
    
    void kemoview_close_mesh_view(void);
    int  kemoview_close_PSF_view(void);
    void kemoview_close_fieldline_view(void);
    
    void kemoview_set_viewtype(int sel);
    
    
    void kemoview_draw_with_modified_domain_distance(void);
    
    void kemoview_set_coastline_radius(double radius);
    double kemoview_get_coastline_radius(void);
    
    void kemoview_set_object_property_flags(int selected, int iflag);
    int kemoview_get_object_property_flags(int selected);
    int kemoview_toggle_object_properties(int selected);
	
	
	void kemoview_alloc_phong_light_list(int num);
	void kemoview_dealloc_phong_light_list(void);
	void kemoview_realloc_phong_light_list(int num);
	
	void kemoview_delete_phong_light_list(int i_delete);
	void kemoview_add_phong_light_list(int i_add, float r, float t, float p);
	
	void kemoview_init_phong_light_list(void);
	
	
	void kemoview_set_each_light_position(int i_point, float r, float t, float p);
	int kemoview_get_num_light_position(void);
	void kemoview_get_each_light_rtp(int i_point, float *r, float *t, float *p);
	
	void kemoview_set_material_parameter(int itype, float shiness_in);
	float kemoview_get_material_parameter(int itype);
	
	
    void kemoview_set_mesh_color_mode(int icolor);
    void kemoview_set_num_of_color_loop(int icolor);
    
    void kemoview_set_node_diamater(double factor, int i_digit);
	void kemoview_get_node_diamater(double *factor, int *i_digit);

	void kemoview_set_domain_distance(double dist);
    
	void kemoview_set_mesh_color_flag(int iflag_group, int selected, int icolor);
	int kemoview_get_mesh_color_flag(int iflag_group, int selected);
    
	void kemoview_set_mesh_color_code(int iflag_group, int selected, float color_code4[4]);
	void kemoview_get_mesh_color_code(int iflag_group, int selected, float color_code4[4]);
	
	void kemoview_set_mesh_opacity(int iflag_group, double opacity_in);
	double kemoview_get_mesh_opacity(int iflag_group);
    

    void kemoview_set_mesh_draw_flag(int selected, int iflag);
    void kemoview_mesh_draw_toggle(int selected);
    
	int kemoview_get_draw_mesh_flag(void);
	int kemoview_get_num_of_mesh_group(int iflag_group);
	void kemoview_set_draw_mesh_item(int iflag_group, int selected, int igrp, int iflag);
	void kemoview_toggle_draw_mesh_item(int iflag_group, int selected, int igrp);
	int kemoview_get_draw_mesh_item(int iflag_group, int selected, int igrp);
    
    void kemoview_get_node_grp_name(struct kv_string *groupname, int i);
    void kemoview_get_ele_grp_name(struct kv_string *groupname, int i);
    void kemoview_get_surf_grp_name(struct kv_string *groupname, int i);
    
    
    int kemoview_get_view_type_flag(void);
    
	int kemoview_get_mesh_color_mode(void);
	int kemoview_get_num_of_color_loop(void);
    
	double kemoview_get_domain_distance(void);
    
    
    void kemoview_get_ext_from_file_name(struct kv_string *filename,
                                         struct kv_string *stripped_prefix, struct kv_string *stripped_ext);
    void kemoview_add_ext_to_file_name(struct kv_string *file_prefix, struct kv_string *added_ext,
                                       struct kv_string *file_name);
    
    
    void kemoview_set_text_color_code(float c_code[4]);
    void kemoview_get_text_color_code(float c_code[4]);
    
    void kemoview_get_fliped_img(int npixel_x, int npixel_y,
                                 unsigned char *glimage, unsigned char *fliped_img);

	void kemoview_quick_view(void);
    void kemoview_modify_view(void);
    void kemoview_rotate(void);
    
    void kemoviewer_reset_to_init_angle(void);
    
    void kemoview_set_retinamode(int i_retina);
    int kemoview_get_retinamode(void);
    
    void kemoview_set_windowsize(int npixel_x, int npixel_y);
    void kemoview_update_projection_by_viewer_size(int npixel_x, int npixel_y);
    void kemoview_set_windowsize_message(int iflag);
    
    void kemoview_update_distance(void);
    
	void kemoview_set_view_integer(int selected, int ivalue);
	void kemoview_set_view_parameter(int selected, int i, double value);
    void kemoview_set_stereo_parameter(double focus, double eye_sep);
    
	int kemoview_get_view_integer(int selected);
	double kemoview_get_view_parameter(int selected, int i);

    void kemoview_mousedolly(double start[2], double x_dolly, double y_dolly);
    void kemoview_mousepan(double start[2], double x_pan, double y_pan);
    void kemoview_zooming(double wheelDelta);
    
    void kemoview_startTrackball(double x, double y);
    /* calculated rotation based on current mouse position */
    void kemoview_rollToTrackball(double x, double y);
    /* add a GL rotation (dA) to an existing GL rotation (A) */
    void kemoview_drugging_addToRotationTrackball(void);
    void kemoview_animation_add_rotation(double dt);
    void kemoview_reset_animation(void);

    /* subroutines for surafces */
	void kemoview_set_PSF_loaded_params(int selected, int input);
    void kemoview_set_loaded_PSF_flag(int id_psf, int iflag);

	int kemoview_get_PSF_loaded_params(int selected);
    int kemoview_get_PSF_loaded_flag(int id_psf);
    
    void kemoview_get_PSF_full_path_file_name(struct kv_string *ucd_m);
    int kemoview_get_PSF_full_path_file_prefix(struct kv_string *psf_filehead, int *iflag);
    int kemoview_get_PSF_file_prefix(struct kv_string *stripped_filehead);
    
	void kemoview_set_each_PSF_field_param(int selected, int input);    
	int kemoview_get_each_PSF_field_param(int selected);
	
    int kemoview_get_PSF_num_component(int i);
	void kemoview_get_PSF_field_name(struct kv_string *colorname, int i);
    
    void kemoview_set_PSF_by_rgba_texture(int width, int height, const unsigned char *bgra_in);
    
    void kemoview_set_PSF_polygon_mode(int iflag);
    void kemoview_set_PSF_tangential_vec_mode(int iflag);
    
    int kemoview_get_PSF_draw_refv(void);
    int kemoview_toggle_PSF_draw_refv(void);
    
	void * kemoview_link_active_colormap_param(void);
	
	int kemoview_select_PSF_draw_switch(int selected);
	int kemoview_get_PSF_draw_flags(int selected);
	
	void kemoview_set_PSF_color_param(int selected, int input);
	int kemoview_get_PSF_color_param(int selected);
	

	void kemoview_delete_PSF_color_list(int i_delete);
	void kemoview_delete_PSF_opacity_list(int i_delete);
	void kemoview_add_PSF_color_list(double add_value, double add_color);
	void kemoview_add_PSF_opacity_list(double add_value, double add_opacity);
	
	void kemoview_set_PSF_linear_colormap(double minvalue, int i_min_digit,
										  double maxvalue, int i_max_digit);
	void kemoview_set_each_PSF_color_w_exp(int selected, double value, int i_digit);
	void kemoview_get_each_PSF_color_w_exp(int selected, double *value, int *i_digit);
	
    void kemoview_set_PSF_single_color(double *rgba);
    void kemoview_set_PSF_constant_opacity(double opacity);
    
	void kemoview_get_PSF_rgb_at_value(double value, double *red, double *green, double *blue);
    double kemoview_get_PSF_opacity_at_value(double value);
    void kemoview_set_PSF_color_data(int i_point, double value, double color);
    void kemoview_set_PSF_opacity_data(int i_point, double value, double opacity);
    
	double kemoview_get_each_PSF_data_range(int selected, int icomp);
	double kemoview_get_each_PSF_colormap_range(int selected);
    
    void kemoview_get_PSF_color_items(int i_point, double *value, double *color);
    void kemoview_get_PSF_opacity_items(int i_point, double *value, double *opacity);
    
    void kemoview_write_PSF_colormap_file(struct kv_string *filename);
    void kemoview_read_PSF_colormap_file(struct kv_string *filename);
	void kemoview_check_PSF_colormap_control(void);
    
    
    /* Subroutines for field lines */
    
    void kemoview_get_fline_full_path_file_name(struct kv_string *ucd_m);
    int kemoview_get_fline_file_step_prefix(struct kv_string *fline_filehead);
    void kemoview_set_fline_file_step(int istep);
    
	void kemoview_set_fline_parameters(int selected, int iflag);
	int kemoview_get_fline_parameters(int selected);
	
	void kemoview_set_fline_color_param(int selected, int input);
	int kemoview_get_fline_color_param(int selected);
	    
    int kemoview_get_fline_color_num_comps(int i);
    void kemoview_get_fline_color_data_name(struct kv_string *colorname, int i);
    
	int kemoview_toggle_fline_type(void);
	
	void kemoview_set_fline_field_param(int selected, int input);
	int kemoview_get_fline_field_param(int selected);

    void kemoview_set_fline_linear_colormap(double minvalue, int i_min_digit,
											double maxvalue, int i_max_digit);
	void kemoview_set_fline_color_w_exp(int selected, double value, int i_digit);
	void kemoview_get_fline_color_w_exp(int selected, double *value, int *i_digit);

	void kemoview_set_fline_constant_opacity(double opacity);

    double kemoview_get_fline_opacity_at_value(double value);
    
    void kemoview_set_fline_color_data(int i_point, double value, double color);
    void kemoview_set_fline_opacity_data(int i_point, double value, double opacity);
    
	double kemoview_get_fline_data_range(int selected, int icomp);
	double kemoview_get_fline_colormap_range(int selected);

	
	void kemoview_get_fline_color_item(int i_point, double *value, double *color);
    void kemoview_get_fline_opacity_item(int i_point, double *value, double *opacity);
    
    void kemoview_write_fline_colormap_file(struct kv_string *filename);
    void kemoview_read_fline_colormap_file(struct kv_string *filename);

    
/** Load texture onto current sectioning image */
    void kemoview_set_texture_to_PSF(int img_fmt, struct kv_string *image_prefix);

/** Set Image file format by ID */
    int kemoview_set_image_file_format_id(struct kv_string *image_ext);
/** Write Kemoviwer window image to file without step number */
    void kemoview_write_window_to_file(int iflag_img, struct kv_string *image_prefix);
/** Write Kemoviwer window image to file with step number */
	void kemoview_write_window_to_file_w_step(int iflag_img, int istep, struct kv_string *image_prefix);
	
	void kemoview_draw_menu_setup(void);
	void kemo_Cleanup(void);
	
#ifdef __cplusplus
}
#endif

#endif
