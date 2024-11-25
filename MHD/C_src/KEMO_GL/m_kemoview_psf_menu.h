
/* m_kemoview_psf_menu.h */

#ifndef M_KEMOVIEW_PSF_MENU_
#define M_KEMOVIEW_PSF_MENU_

#include <stdio.h>
#include <stdlib.h>

#include "kemoviewer.h"
#include "calypso_param_c.h"
#include "kemoviewer_param_c.h"
#include "kemoviewer_base.h"
#include "m_vertex_buffer.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_color_table_c.h"
#include "m_colorbar_buffer.h"
#include "skip_comment_c.h"
#include "set_rgba_table_c.h"
#include "set_psf_viewer.h"

struct kemo_array_control{
	int nlimit_loaded;
	int num_loaded;
	int nmax_loaded;
	int id_current;
    int istep_sync;
	int *iflag_loaded;
    
    long *istack_all_psf_node;

    long ntot_psf_patch;
    long istack_solid_psf_txtur;
    long istack_solid_psf_patch;
    long istack_trans_psf_txtur;
    long istack_trans_psf_patch;
    
    int *ipsf_viz_far;
	int *iele_viz_far;
	
    int iflag_avail_time;
    int iflag_draw_time;
    double time_disp;
    
    int iflag_avail_file_step;
    int iflag_draw_file_step;
    int file_step_disp;
    
    int ipsf_texured;
    struct gl_texure_image *psf_texure;
};

struct psf_menu_val{
	struct kv_string *viz_prefix_c;
	int viz_step_c;
	int iformat_viz_file;
	
    int iflag_draw_time;
    double time;
    
	int if_draw_viz;
	int ic_draw_viz;
	long icomp_draw_viz;
	
    int iflag_draw_viz;
	int iflag_draw_cbar;
    
	struct colormap_params **cmap_viz_comp;
	struct colormap_params **cmap_viz_fld;
	
///*    Color mode  ID      **///
	int viz_color_mode;
    int polygon_mode_psf;
    long viz_line_type;
    int ncorner_viz_line;
    
    int isoline_ncorner;
	double viz_line_width;
	
	int isoline_color;
	int n_isoline;
	
	int draw_psf_grid;
	int draw_psf_zero;
	
	int ist_positive_line;
	
	int draw_psf_vect;
	int draw_psf_refv;
	int vector_patch_color;
    int ivect_tangential;
	int increment_vect;
	double scale_vect;
	double vector_thick;
    
    long nadded_for_phi0;
    struct map_interpolate *map_itp;
};

/* Prototypes */
void set_PSF_component_name(int ncomp, int id_coord, int icomp, char *comp_name);

void alloc_psfs_sorting_list(struct kemo_array_control *psf_a);
void dealloc_psfs_sorting_list(struct kemo_array_control *psf_a);


struct psf_menu_val *  init_psf_menu_val(void);
void dealloc_psf_menu_val(struct psf_menu_val *psf_m);

void alloc_draw_psf_flags(int id_color_mode, long nfield, long ncomptot,
                          struct psf_menu_val *psf_m);
void dealloc_draw_psf_flags(long nfield, long ncomptot,
                            struct psf_menu_val *psf_m);

void alloc_kemoview_array(struct kemo_array_control *psf_a);
void set_max_psf_loading(int ntot_psf_data, struct kemo_array_control *psf_a);
void init_kemoview_array(struct kemo_array_control *psf_a);
void dealloc_kemoview_array(struct kemo_array_control *psf_a);

void init_psf_parameters(struct psf_menu_val *psf_m);

void set_VIZ_field(int selected,
                   char *data_name, long *istack_comp,
                   struct psf_menu_val *psf_m);
void set_VIZ_component(int selected,
                       char *data_name, long *istack_comp,
                       struct psf_menu_val *psf_m);

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
