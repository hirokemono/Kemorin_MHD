
/* m_kemoview_mesh_menu.h */

#ifndef M_KEMOVIEWER_MESH_MENU_
#define M_KEMOVIEWER_MESH_MENU_

#include <stdio.h>
#include <stdlib.h>

#include "kemoviewer.h"
#include "calypso_param_c.h"
#include "kemoviewer_param_c.h"
#include "kemoviewer_base.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_color_table_c.h"
#include "m_colorbar_work.h"
#include "rainbow_color_code_c.h"
#include "skip_comment_c.h"
#include "set_rgba_table_c.h"
#include "numbers_to_bin_c.h"


#define SHUTTER_OFF 0
#define SHUTTER_ON  1
#define ANAGLYPH_OFF 0
#define ANAGLYPH_ON  1

struct mesh_menu_val{
	struct kv_string *mesh_file_name;

	int iformat_surface_mesh;
	int iflag_draw_mesh;
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
	
	float bg_color[4];
	float text_color[4];
	
	float domain_surface_color_code[4];
	float ele_surface_color_code[4];
	float surf_surface_color_code[4];
	
	float domain_grid_color_code[4];
	float ele_grid_color_code[4];
	float surf_grid_color_code[4];
	
	float domain_node_color_code[4];
	float node_node_color_code[4];
	float ele_node_color_code[4];
	float surf_node_color_code[4];
};

/* Prototypes */

void alloc_draw_mesh_flags(struct viewer_mesh *mesh_s,
			struct mesh_menu_val *mesh_m);
struct mesh_menu_val * alloc_mesh_menu_val(void);

void dealloc_draw_mesh_flags(struct mesh_menu_val *mesh_m);

void init_viewer_parameters(struct mesh_menu_val *mesh_m);

void set_mesh_color_mode(int icolor, struct mesh_menu_val *mesh_m);
void set_num_of_color_loop(int icolor, struct mesh_menu_val *mesh_m);

void set_node_diamater(double factor, int i_digit, struct mesh_menu_val *mesh_m);
void get_node_diamater(struct mesh_menu_val *mesh_m, double *factor, int *i_digit);

void set_domain_distance(double dist, struct mesh_menu_val *mesh_m);


void set_polygon_mode(int iflag, struct mesh_menu_val *mesh_m);
void set_axis_flag(int iflag, struct mesh_menu_val *mesh_m);
void set_coastline_flag(int iflag, struct mesh_menu_val *mesh_m);
void set_sphere_grid_flag(int iflag, struct mesh_menu_val *mesh_m);

int toggle_polygon_mode(struct mesh_menu_val *mesh_m);
int toggle_draw_axis(struct mesh_menu_val *mesh_m);
int toggle_coastline_flag(struct mesh_menu_val *mesh_m);
int toggle_sphere_grid_flag(struct mesh_menu_val *mesh_m);

void set_mesh_draw_flag(int num_pe, int selected, int iflag, struct mesh_menu_val *mesh_m);
void mesh_draw_toggle(int num_pe, int selected, struct mesh_menu_val *mesh_m);

void set_draw_domain_flag(int selected, int igrp, int iflag, struct mesh_menu_val *mesh_m);
void set_draw_nodgrp_flag(int igrp, int iflag, struct mesh_menu_val *mesh_m);
void set_draw_elegrp_flag(int selected, int igrp, int iflag, struct mesh_menu_val *mesh_m);
void set_draw_surfgrp_flag(int selected, int igrp, int iflag, struct mesh_menu_val *mesh_m);

void toggle_draw_domain_flag(int selected, int igrp, int ngrp, struct mesh_menu_val *mesh_m);
void toggle_draw_nodgrp_flag(int igrp, int ngrp, struct mesh_menu_val *mesh_m);
void toggle_draw_elegrp_flag(int selected, int igrp, int ngrp, struct mesh_menu_val *mesh_m);
void toggle_draw_surfgrp_flag(int selected, int igrp, int ngrp, struct mesh_menu_val *mesh_m);

int get_draw_domain_flag(struct mesh_menu_val *mesh_m, int selected, int igrp);
int get_draw_nodgrp_flag(struct mesh_menu_val *mesh_m, int igrp);
int get_draw_elegrp_flag(struct mesh_menu_val *mesh_m, int selected, int igrp);
int get_draw_surfgrp_flag(struct mesh_menu_val *mesh_m, int selected, int igrp);


void set_domain_color_flag(int selected, int icolor, struct mesh_menu_val *mesh_m);
void set_node_grp_color_flag(int icolor, struct mesh_menu_val *mesh_m);
void set_ele_grp_color_flag(int selected, int icolor, struct mesh_menu_val *mesh_m);
void set_surf_grp_color_flag(int selected, int icolor, struct mesh_menu_val *mesh_m);

int get_domain_color_flag(int selected, struct mesh_menu_val *mesh_m);
int get_node_grp_color_flag(struct mesh_menu_val *mesh_m);
int get_ele_grp_color_flag(int selected, struct mesh_menu_val *mesh_m);
int get_surf_grp_color_flag(int selected, struct mesh_menu_val *mesh_m);

void set_domain_color_code(int selected, float color_code4[4],
			struct mesh_menu_val *mesh_m);
void set_node_grp_color_code(GLfloat color_code4[4], struct mesh_menu_val *mesh_m);
void set_ele_grp_color_code(int selected, GLfloat color_code4[4],
			struct mesh_menu_val *mesh_m);
void set_surf_grp_color_code(int selected, GLfloat color_code4[4], 
			struct mesh_menu_val *mesh_m);

void send_domain_color_code(struct mesh_menu_val *mesh_m, int selected,
			float color_code4[4]);
void send_node_grp_color_code(struct mesh_menu_val *mesh_m, GLfloat color_code4[4]);
void send_ele_grp_color_code(struct mesh_menu_val *mesh_m, int selected,
			float color_code4[4]);
void send_surf_grp_color_code(struct mesh_menu_val *mesh_m, int selected,
			float color_code4[4]);
#endif
