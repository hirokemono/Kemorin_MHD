
/* m_kemoview_psf_menu.h */

#ifndef M_KEMOVIEW_PSF_MENU_
#define M_KEMOVIEW_PSF_MENU_

#include <stdio.h>
#include <stdlib.h>

#include "kemoviewer.h"
#include "calypso_param_c.h"
#include "kemoviewer_param_c.h"
#include "kemoviewer_base.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_color_table_c.h"
#include "m_colorbar_work.h"
#include "skip_comment_c.h"
#include "set_rgba_table_c.h"

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
	
    int iflag_avail_time;
    int iflag_draw_time;
    double time_disp;
    
    int iflag_avail_file_step;
    int iflag_draw_file_step;
    int file_step_disp;
    
	struct cbar_work *cbar_wk;
	struct tlabel_work *tlabel_wk;
};

struct psf_menu_val{
	struct kv_string *psf_header;
	int psf_step;
	int iflag_psf_file;
	
    int iflag_draw_time;
    double time;

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
	double isoline_width;
	
	int ist_positive_line;
	
	int texture_width;
	int texture_height;
	int texture_npix;
	GLuint texture_name[10];
	GLubyte *texture_rgba;
	
	struct colormap_params **cmap_psf_comp;
	struct colormap_params **cmap_psf_fld;
	
	int draw_psf_vect;
	int draw_psf_refv;
	int vector_patch_color;
	int increment_vect;
	double scale_vect;
	double vector_thick;
};

/* Prototypes */

void set_PSF_component_name(int ncomp, int id_coord, int icomp, char *comp_name);

void alloc_psfs_sorting_list(struct kemo_array_control *psf_a);
void dealloc_psfs_sorting_list(struct kemo_array_control *psf_a);


void alloc_draw_psf_flags(struct psf_data *psf_s, struct psf_menu_val *psf_m);
void dealloc_draw_psf_flags(struct psf_data *psf_s, struct psf_menu_val *psf_m);

void alloc_draw_psf_texture(struct psf_menu_val *psf_m);
void dealloc_draw_psf_texture(struct psf_menu_val *psf_m);

void alloc_kemoview_array(struct kemo_array_control *psf_a);
void set_max_psf_loading(int ntot_psf_data, struct kemo_array_control *psf_a);
void init_kemoview_array(struct kemo_array_control *psf_a);
void dealloc_kemoview_array(struct kemo_array_control *psf_a);

void init_psf_parameters(struct psf_menu_val *psf_m);

void set_PSF_field(int selected, struct psf_data *psf_s, struct psf_menu_val *psf_m);
void set_PSF_component(int selected, struct psf_data *psf_s, struct psf_menu_val *psf_m);



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

void set_iflag_draw_time(double time, struct psf_menu_val *psf_m);

#endif
