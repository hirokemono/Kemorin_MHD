
/* m_kemoviewer_menu.h */

#ifndef M_KEMOVIEWER_MENU_
#define M_KEMOVIEWER_MENU_

#include <stdio.h>
#include <stdlib.h>

#include "kemoviewer.h"
#include "kemosrc_param_c.h"
#include "kemoviewer_param_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_color_table_c.h"
#include "m_colorbar_work.h"
#include "skip_comment_c.h"
#include "set_rgba_table_c.h"


#define VIEW_3D        0
#define VIEW_STEREO    1
#define VIEW_MAP       2
#define VIEW_XY        3
#define VIEW_XZ        4
#define VIEW_YZ        5
#define RESET         10

#define MESH_OFF          0
#define SURFNOD_TOGGLE    1
#define SURFSOLID_TOGGLE  2
#define SURFGRID_TOGGLE   3

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

#define IFLAG_MESH      99
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

#define OFF 0
#define ON  1


struct kemo_array_control{
	int nlimit_loaded;
	int num_loaded;
	int nmax_loaded;
	int id_current;
    int istep_sync;
	int *iflag_loaded;
    
    int ntot_psf_patch;
    int istack_solid_psf_txtur;
    int istack_solid_psf_patch;
    int istack_trans_psf_txtur;
    int istack_trans_psf_patch;
    
    double *z_ele_viz;
    int *ipsf_viz_far;
	int *iele_viz_far;
	
	struct cbar_work *cbar_wk;
};

struct psf_menu_val{
	struct kv_string *psf_header;
	int psf_step;
	int iflag_psf_file;
	
	int polygon_mode_psf;
    int ivect_tangential;
	
	int draw_psf_solid;
	int draw_psf_grid;
	int draw_psf_zero;
	int draw_psf_cbar;
	
	int if_draw_psf;
	int ic_draw_psf;
	int icomp_draw_psf;
	
	int psf_patch_color;
	int isoline_color;
	int n_isoline;
	
	int ist_positive_line;
	
	int texture_width;
	int texture_height;
	int texture_npix;
	GLuint texture_name[10];
	GLubyte *texture_rgba;
	
	struct colormap_params *cmap_psf;
	struct colormap_params **cmap_psf_comp;
	struct colormap_params **cmap_psf_fld;
	
	int draw_psf_vect;
	int draw_psf_refv;
	int vector_patch_color;
	int increment_vect;
	double scale_vect;
	double vector_thick;
};

struct fline_menu_val{
	struct kv_string *fline_header;
	int fline_step;
	int iformat_fline_file;
	
	int iflag_draw_fline;
	
	int if_draw_fline;
	int ic_draw_fline;
	int icomp_draw_fline;
	
	int fieldline_color;
	int fieldline_type;
	double fieldline_thick;
	
	struct colormap_params *cmap_fline;
	struct colormap_params **cmap_fline_comp;
	struct colormap_params **cmap_fline_fld;
};

/* Prototypes */

void alloc_kvstringitem(int length, struct kv_string *ucd_m);
struct kv_string* alloc_kvstring();
struct kv_string* init_kvstring_by_string(const char *org_string);
void dealloc_kvstring(struct kv_string *kvstring);

void alloc_copy_string(const char *org_string, struct kv_string *ucd_copied);
void alloc_set_ucd_field_file_name(int iformat_ucd_file, int istep, const char *ucd_header,
			struct kv_string *ucd_m);
void alloc_set_grd_field_file_name(int iformat_ucd_file, const char *ucd_header, 
			struct kv_string *ucd_m);

void alloc_psfs_sorting_list(struct kemo_array_control *psf_a);
void dealloc_psfs_sorting_list(struct kemo_array_control *psf_a);


void alloc_draw_psf_flags(struct psf_data *psf_s, struct psf_menu_val *psf_m);
void alloc_draw_fline_flags(struct psf_data *fline_s, struct fline_menu_val *fline_m);
void dealloc_draw_psf_flags(struct psf_data *psf_s, struct psf_menu_val *psf_m);
void dealloc_draw_fline_flags(struct psf_data *fline_s, struct fline_menu_val *fline_m);

void alloc_draw_psf_texture(struct psf_menu_val *psf_m);
void dealloc_draw_psf_texture(struct psf_menu_val *psf_m);

void alloc_kemoview_array(struct kemo_array_control *psf_a);
void set_max_psf_loading(int ntot_psf_data, struct kemo_array_control *psf_a);
void init_kemoview_array(struct kemo_array_control *psf_a);
void dealloc_kemoview_array(struct kemo_array_control *psf_a);

void init_psf_parameters(struct psf_menu_val *psf_m);
void init_fline_parameters(struct fline_menu_val *fline_m);

void set_linear_colormap(struct colormap_params *cmap_s,
			double val_min, double val_max);

void set_draw_flag_for_all(int iflag, int ngrp, int *iflag_draw);
void select_draw_flag_toggle(int selected, int ngrp, int *iflag_draw);

void set_PSF_field(int selected, struct psf_data *psf_s, struct psf_menu_val *psf_m);
void set_PSF_component(int selected, struct psf_data *psf_s, struct psf_menu_val *psf_m);

void set_fline_color_field(int selected, struct psf_data *fline_s, 
			struct fline_menu_val *fline_m);
void set_fline_color_component(int selected, struct psf_data *fline_s,
			struct fline_menu_val *fline_m);



int get_PSF_maximum_load(struct kemo_array_control *psf_a);

void psf_viewer_evolution(int istep, struct kemo_array_control *psf_a);

void set_PSF_num_loaded(int num, struct kemo_array_control *psf_a);
void set_PSF_max_loaded(int num, struct kemo_array_control *psf_a);
void set_loaded_PSF_flag(int id_psf, int iflag, struct kemo_array_control *psf_a);
void set_current_PSF_to_menu(int id_psf, struct kemo_array_control *psf_a);

int get_PSF_num_loaded(struct kemo_array_control *psf_a);
int get_PSF_max_loaded(struct kemo_array_control *psf_a);
int get_PSF_loaded_flag(int id_psf, struct kemo_array_control *psf_a);
int get_curent_PSF_ID(struct kemo_array_control *psf_a);
int get_curent_PSF_filename(struct kemo_array_control *psf_a);

int get_PSF_draw_switch(struct kemo_array_control *psf_a);

#endif
