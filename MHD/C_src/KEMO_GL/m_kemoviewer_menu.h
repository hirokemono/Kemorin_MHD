
/* m_kemoviewer_menu.h */

#ifndef M_KEMOVIEWER_MENU_
#define M_KEMOVIEWER_MENU_

#include <stdio.h>
#include <stdlib.h>

#include "kemosrc_param_c.h"
#include "kemoviewer_param_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_color_table_c.h"
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

#define SHUTTER_OFF 0
#define SHUTTER_ON  1
#define ANAGLYPH_OFF 0
#define ANAGLYPH_ON  1

#define OFF 0
#define ON  1

struct mesh_menu_val{
	char mesh_file_name[LENGTHBUF];
	char pick_surface_command[LENGTHBUF];
    int iformat_surface_mesh;

	int iflag_streo_stutter;
	int iflag_streo_anaglyph;
	
	int iflag_draw_mesh;
	int iflag_draw_type;
	int iflag_view_type;
	
	int shading_mode;
	int polygon_mode;
	
	int mesh_color_mode;
	int num_of_color_loop;
	
	int iflag_draw_axis;
	
	int iflag_draw_coast;
	int iflag_draw_sph_grid;
	double radius_coast;
	
	int draw_surface_nod;
	int draw_surface_grid;
	int draw_surface_solid;
	
	int *draw_domains_nod;
	int *draw_domains_grid;
	int *draw_domains_solid;
	int *always_draw_domains;
	
	int *draw_nodgrp_nod;
	
	int *draw_elegrp_nod;
	int *draw_elegrp_grid;
	int *draw_elegrp_solid;
	
	int *draw_surfgrp_nod;
	int *draw_surfgrp_grid;
	int *draw_surfgrp_solid;
	
	int domain_surface_color;
	int ele_surface_color;
	int surf_surface_color;
	
	int domain_grid_color;
	int ele_grid_color;
	int surf_grid_color;
	
	int domain_node_color;
	int node_node_color;
	int ele_node_color;
	int surf_node_color;
	
	double node_diam;
	double dist_domains;
	
	double domain_opacity;
	double ele_grp_opacity;
	double surf_grp_opacity;
	
	GLfloat bg_color[4];
	GLfloat text_color[4];
	
	GLfloat domain_surface_color_code[4];
	GLfloat ele_surface_color_code[4];
	GLfloat surf_surface_color_code[4];
	
	GLfloat domain_grid_color_code[4];
	GLfloat ele_grid_color_code[4];
	GLfloat surf_grid_color_code[4];
	
	GLfloat domain_node_color_code[4];
	GLfloat node_node_color_code[4];
	GLfloat ele_node_color_code[4];
	GLfloat surf_node_color_code[4];
};

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
};

struct psf_menu_val{
	char psf_header[LENGTHBUF];
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
	char fline_header[LENGTHBUF];
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

void alloc_copy_string(struct kv_string *ucd_copied,
			struct kv_string *ucd_org);
void alloc_set_ucd_field_file_name(int iformat_ucd_file, int istep, const char *ucd_header,
			struct kv_string *ucd_m);
void alloc_set_grd_field_file_name(int iformat_ucd_file, const char *ucd_header, 
			struct kv_string *ucd_m);
void dealloc_ucd_m_file_name(struct kv_string *ucd_m);

void alloc_psfs_sorting_list(struct kemo_array_control *psf_a);
void dealloc_psfs_sorting_list(struct kemo_array_control *psf_a);


void alloc_draw_mesh_flags(struct viewer_mesh *mesh_s,
			struct mesh_menu_val *mesh_m);
void dealloc_draw_mesh_flags(struct mesh_menu_val *mesh_m);

void alloc_draw_psf_flags(struct psf_data *psf_s, struct psf_menu_val *psf_m);
void alloc_draw_fline_flags(struct psf_data *fline_s, struct fline_menu_val *fline_m);
void dealloc_draw_psf_flags(struct psf_data *psf_s, struct psf_menu_val *psf_m);
void dealloc_draw_fline_flags(struct psf_data *fline_s, struct fline_menu_val *fline_m);

void alloc_draw_psf_texture(struct psf_menu_val *psf_m);
void dealloc_draw_psf_texture(struct psf_menu_val *psf_m);

void alloc_kemoview_array(struct kemo_array_control *psf_a);
void init_kemoview_array(int ntot_psf_data, struct kemo_array_control *psf_a);
void dealloc_kemoview_array(struct kemo_array_control *psf_a);

void init_viewer_parameters(struct mesh_menu_val *mesh_m);
void init_psf_parameters(struct psf_menu_val *psf_m);
void init_fline_parameters(struct fline_menu_val *fline_m);

void set_linear_colormap(struct colormap_params *cmap_s,
			double val_min, double val_max);

void set_draw_flag_for_all(int iflag, int ngrp, int *iflag_draw);
void select_draw_flag_toggle(int selected, int ngrp, int *iflag_draw);

void select_domain_node_color(int selected, struct mesh_menu_val *mesh_m);
void select_domain_grid_color(int selected, struct mesh_menu_val *mesh_m);
void select_ele_grp_node_color(int selected, struct mesh_menu_val *mesh_m);
void select_ele_grp_grid_color(int selected, struct mesh_menu_val *mesh_m);
void select_surf_grp_node_color(int selected, struct mesh_menu_val *mesh_m);
void select_surf_grp_grid_color(int selected, struct mesh_menu_val *mesh_m);
void select_node_grp_node_color(int selected, struct mesh_menu_val *mesh_m);

void set_shading_mode(int iflag, struct mesh_menu_val *mesh_m);
void set_polygon_mode(int iflag, struct mesh_menu_val *mesh_m);
void set_axis_flag(int iflag, struct mesh_menu_val *mesh_m);
void set_coastline_flag(int iflag, struct mesh_menu_val *mesh_m);
void set_sphere_grid_flag(int iflag, struct mesh_menu_val *mesh_m);

int toggle_shading_mode(struct mesh_menu_val *mesh_m);
int toggle_polygon_mode(struct mesh_menu_val *mesh_m);
int toggle_draw_axis(struct mesh_menu_val *mesh_m);
int toggle_coastline_flag(struct mesh_menu_val *mesh_m);
int toggle_sphere_grid_flag(struct mesh_menu_val *mesh_m);

void set_PSF_field(int selected, struct psf_data *psf_s, struct psf_menu_val *psf_m);
void set_PSF_component(int selected, struct psf_data *psf_s, struct psf_menu_val *psf_m);

void set_fline_color_field(int selected, struct psf_data *fline_s, 
			struct fline_menu_val *fline_m);
void set_fline_color_component(int selected, struct psf_data *fline_s,
			struct fline_menu_val *fline_m);
#endif
