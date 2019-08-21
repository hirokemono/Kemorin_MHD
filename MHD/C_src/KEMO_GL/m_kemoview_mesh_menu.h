
/* m_kemoview_mesh_menu.h */

#ifndef M_KEMOVIEWER_MESH_MENU_
#define M_KEMOVIEWER_MESH_MENU_

#include <stdio.h>
#include <stdlib.h>

#include "kemoviewer.h"
#include "kemosrc_param_c.h"
#include "kemoviewer_param_c.h"
#include "kemoviewer_base.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_color_table_c.h"
#include "m_colorbar_work.h"
#include "skip_comment_c.h"
#include "set_rgba_table_c.h"


#define SHUTTER_OFF 0
#define SHUTTER_ON  1
#define ANAGLYPH_OFF 0
#define ANAGLYPH_ON  1

struct mesh_menu_val{
	struct kv_string *mesh_file_name;
	struct kv_string *pick_surface_command;

	int iformat_surface_mesh;
	int iflag_draw_mesh;
	
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

/* Prototypes */

void alloc_draw_mesh_flags(struct viewer_mesh *mesh_s,
			struct mesh_menu_val *mesh_m);
void dealloc_draw_mesh_flags(struct mesh_menu_val *mesh_m);

void init_viewer_parameters(struct mesh_menu_val *mesh_m);

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
#endif
