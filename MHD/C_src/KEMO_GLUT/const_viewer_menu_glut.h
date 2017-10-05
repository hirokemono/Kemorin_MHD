
/* const_viewer_menu_glut.h */

#ifndef CONST_VIEWER_MESH_GLUT_
#define CONST_VIEWER_MESH_GLUT_

#include <stdio.h>

#include "kemoviewer.h"
#include "view_modifier_glut.h"

#define QUIT_SELECTED  0
#define FILE_OPEN      1
#define MESH_ON        2
#define SAVE_SNAPSHOT   10
#define SAVE_EVOLUTION  11
#define SAVE_ROTATION   12

#define ROTATE_X  1
#define ROTATE_Y  2
#define ROTATE_Z  3

struct glut_menu_address{
	GLint submenu_id;
	GLint viewtype_id;
	
	GLint domain_id;
	GLint nod_grp_menu;
	GLint ele_grp_menu;
	GLint surf_grp_menu;
	
	GLint psf_root_menu;
	GLint map_root_menu;
	GLint fline_root_menu;
	
	GLint color_mode_menu;
	GLint polygon_id_menu;
	GLint save_rot_image_menu;
	GLint draw_rot_image_menu;
	
	GLint surface_color_menu;
	GLint grid_color_menu;
	GLint node_color_menu;
	
	GLint node_node_color_menu;
	
	GLint surf_4_ele_grp_menu;
	GLint grid_4_ele_grp_menu;
	GLint nod_4_ele_grp_menu;
	GLint ele_surf_color_menu;
	GLint ele_grid_color_menu;
	GLint ele_node_color_menu;
	
	GLint surf_4_surf_grp_menu;
	GLint grid_4_surf_grp_menu;
	GLint nod_4_surf_grp_menu;
	GLint surf_surf_color_menu;
	GLint surf_grid_color_menu;
	GLint surf_node_color_menu;
	
	GLint ichoose_current_psf_menu;
	GLint ichoose_field_menu;
	GLint ichoose_comp_menu;
	
	GLint ichoose_psf_patchcolor_menu;
	GLint ichoose_psf_linecolor_menu;
    GLint ichoose_psf_colormode_menu;
    
    GLint ichoose_psf_colormap_menu;
    GLint ichoose_psf_opacitymap_menu;

    GLint add_colormap_menu;
    GLint modify_colormap_menu;
    GLint delete_colormap_menu;
    GLint add_opacitymap_menu;
    GLint modify_opacitymap_menu;
    GLint delete_opacitymap_menu;

	GLint ichoose_fline_c_menu;
	GLint ichoose_fline_col_type_menu;
};

/* prototypes */

#ifdef __cplusplus
extern "C" {
#endif

	void glut_drawing_select() ;
	void glut_current_PSF_select();
	void glut_PSF_field_select();
	void glut_fline_color_select();
	void set_PSF_component_name(int ncomp, int icomp, int id_coord, char *comp_name);
	void glut_PSF_comps_select();
	void glut_PSF_patchcolor_select();
	void glut_PSF_linecolor_select();
    void glut_PSF_colormode_select();
	void glut_fline_color_comp_select();
	void glut_PSF_draw_menu();
	void glut_PSF_range_menu();
	void glut_nod_grp_menu_item();
	void glut_surf_grp_patch_menu();
	void glut_surf_grp_edge_menu();
	void glut_surf_grp_node_menu();
	void glut_ele_grp_patch_menu();
	void glut_ele_grp_edge_menu();
	void glut_ele_grp_node_menu();
	void glut_viewtype_menu();
	void glut_mesh_display_menu();
	void glut_draw_axis_menu_item(int iflag_draw_axis);
	void glut_draw_coast_menu_item(int iflag_draw_coast);
	void glut_draw_sph_grid_menu_item(int iflag_draw_sph);
	void glut_save_image_menu_item(char *file_head);
	void glut_color_menu_item();
	void glut_line_color_menu_item();
	void glut_grp_color_menu_item();
	void glut_surf_color_menu_item();
	void glut_color_mode_menu_item();
	void glut_fline_col_type_menu();

#ifdef __cplusplus
}
#endif

#endif
