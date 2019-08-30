
/* m_kemoview_mesh_menu.c */


#include "m_kemoview_mesh_menu.h"


void alloc_draw_mesh_flags(struct viewer_mesh *mesh_s, 
			struct mesh_menu_val *mesh_m){
	int i;
	
	mesh_m->draw_domains_nod =    (int *)calloc(mesh_s->num_pe_sf,sizeof(int));
	mesh_m->draw_domains_grid =   (int *)calloc(mesh_s->num_pe_sf,sizeof(int));
	mesh_m->draw_domains_solid =  (int *)calloc(mesh_s->num_pe_sf,sizeof(int));
	mesh_m->always_draw_domains = (int *)calloc(mesh_s->num_pe_sf,sizeof(int));

	mesh_m->draw_nodgrp_nod =    (int *)calloc(mesh_s->ngrp_nod_sf,sizeof(int));
	
	mesh_m->draw_elegrp_nod =    (int *)calloc(mesh_s->ngrp_ele_sf,sizeof(int));
	mesh_m->draw_elegrp_grid =   (int *)calloc(mesh_s->ngrp_ele_sf,sizeof(int));
	mesh_m->draw_elegrp_solid =  (int *)calloc(mesh_s->ngrp_ele_sf,sizeof(int));
	
	mesh_m->draw_surfgrp_nod =   (int *)calloc(mesh_s->ngrp_surf_sf,sizeof(int));
	mesh_m->draw_surfgrp_grid =  (int *)calloc(mesh_s->ngrp_surf_sf,sizeof(int));
	mesh_m->draw_surfgrp_solid = (int *)calloc(mesh_s->ngrp_surf_sf,sizeof(int));

	for (i=0;i<mesh_s->num_pe_sf;i++){
		mesh_m->draw_domains_nod[i] =    INIT_DRAW_NOD;
		mesh_m->draw_domains_grid[i] =   INIT_DRAW_LINE;
		mesh_m->draw_domains_solid[i] =  INIT_DRAW_SOLID;
		mesh_m->always_draw_domains[i] = INIT_DRAW_SOLID;
	};
	for (i=0;i<mesh_s->ngrp_nod_sf;i++){
		mesh_m->draw_nodgrp_nod[i] =    INIT_DRAW_NOD;
	};
	for (i=0;i<mesh_s->ngrp_ele_sf;i++){
		mesh_m->draw_elegrp_nod[i] =    INIT_DRAW_NOD;
		mesh_m->draw_elegrp_grid[i] =   INIT_DRAW_LINE;
		mesh_m->draw_elegrp_solid[i] =  0;
	};
	for (i=0;i<mesh_s->ngrp_surf_sf;i++){
		mesh_m->draw_surfgrp_nod[i] =   INIT_DRAW_NOD;
		mesh_m->draw_surfgrp_grid[i] =  INIT_DRAW_LINE;
		mesh_m->draw_surfgrp_solid[i] = 0;
	};
	return;
}

void dealloc_draw_mesh_flags(struct mesh_menu_val *mesh_m){
	free(mesh_m->draw_surfgrp_solid);
	free(mesh_m->draw_surfgrp_grid);
	free(mesh_m->draw_surfgrp_nod);
	
	free(mesh_m->draw_elegrp_solid);
	free(mesh_m->draw_elegrp_grid);
	free(mesh_m->draw_elegrp_nod);
	
	free(mesh_m->draw_nodgrp_nod);

	free(mesh_m->always_draw_domains);
	free(mesh_m->draw_domains_solid);
	free(mesh_m->draw_domains_grid);
	free(mesh_m->draw_domains_nod);
	
	dealloc_kvstring(mesh_m->pick_surface_command);
	dealloc_kvstring(mesh_m->mesh_file_name);
	return;
}


void init_viewer_parameters(struct mesh_menu_val *mesh_m){
	mesh_m->draw_surface_solid = INIT_DRAW_SOLID;
	mesh_m->draw_surface_grid =  INIT_DRAW_LINE;
	mesh_m->draw_surface_nod =   INIT_DRAW_NOD;
	
	mesh_m->iflag_draw_axis =    INIT_DRAW_LINE;
	
	mesh_m->mesh_color_mode =    INIT_COLOR_MODE;
	mesh_m->num_of_color_loop =  INIT_NUM_COLOR_LOOP;
	
	mesh_m->polygon_mode =  INIT_POLYGON_MODE;
	mesh_m->node_diam =     INIT_NODE_SIZE;
	mesh_m->dist_domains =  INIT_DISTANCE;
	
	mesh_m->domain_node_color =    INIT_NODE_COLOR;
	mesh_m->node_node_color =      INIT_NODE_COLOR;
	mesh_m->ele_node_color =       INIT_NODE_COLOR;
	mesh_m->surf_node_color =      INIT_NODE_COLOR;
	
	mesh_m->domain_grid_color =    INIT_LINE_COLOR;
	mesh_m->ele_grid_color =       INIT_LINE_COLOR;
	mesh_m->surf_grid_color =      INIT_LINE_COLOR;
	
	mesh_m->domain_surface_color = INIT_SURFACE_COLOR;
	mesh_m->ele_surface_color =    INIT_SURFACE_COLOR;
	mesh_m->surf_surface_color =   INIT_SURFACE_COLOR;
	
	
	mesh_m->domain_opacity =    INIT_OPACITY;
	mesh_m->ele_grp_opacity =   INIT_OPACITY;
	mesh_m->surf_grp_opacity =  INIT_OPACITY;
	
	mesh_m->iflag_draw_coast =     OFF;
	mesh_m->iflag_draw_sph_grid =  OFF;
	mesh_m->radius_coast =         ONE;
	return;
};


void select_domain_node_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->domain_node_color = selected;
	return;
}
void select_domain_grid_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->domain_grid_color = selected;
	return;
}
void select_domain_patch_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->domain_surface_color = selected;
	return;
}

int get_domain_node_color_mode(struct mesh_menu_val *mesh_m){return mesh_m->domain_node_color;};
int get_domain_grid_color_mode(struct mesh_menu_val *mesh_m){return mesh_m->domain_grid_color;};
int get_domain_patch_color_mode(struct mesh_menu_val *mesh_m){return mesh_m->domain_surface_color;};

void select_ele_grp_node_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->ele_node_color = selected;
	return;
}
void select_ele_grp_grid_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->ele_grid_color = selected;
	return;
}
void select_ele_grp_patch_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->ele_surface_color = selected;
	return;
}

int get_ele_grp_node_color(struct mesh_menu_val *mesh_m){return mesh_m->ele_node_color;}
int get_ele_grp_grid_color(struct mesh_menu_val *mesh_m){return mesh_m->ele_grid_color;}
int get_ele_grp_patch_color(struct mesh_menu_val *mesh_m){return mesh_m->ele_surface_color;}

void select_surf_grp_node_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->surf_node_color = selected;
	return;
}
void select_surf_grp_grid_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->surf_grid_color = selected;
	return;
}
void select_surf_grp_patch_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->surf_surface_color = selected;
	return;
}

int get_surf_grp_node_color(struct mesh_menu_val *mesh_m){return mesh_m->surf_node_color;}
int get_surf_grp_grid_color(struct mesh_menu_val *mesh_m){return mesh_m->surf_grid_color;}
int get_surf_grp_patch_color(struct mesh_menu_val *mesh_m){return mesh_m->surf_surface_color;}


void select_node_grp_node_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->node_node_color = selected;
	return;
}

void set_domain_color_code(int selected, float color_code4[4],
			struct mesh_menu_val *mesh_m){
    if(selected == SURFSOLID_TOGGLE){
        copy_rgba_color_c(color_code4, mesh_m->domain_surface_color_code);
        kemoview_set_domain_opacity((double) color_code4[3]);
    } else if(selected == SURFGRID_TOGGLE){
        copy_rgba_color_c(color_code4, mesh_m->domain_grid_color_code);
    } else if(selected == SURFNOD_TOGGLE){
        copy_rgba_color_c(color_code4, mesh_m->domain_node_color_code);
    };
};

void set_node_grp_color_code(float color_code4[4], struct mesh_menu_val *mesh_m){
    copy_rgba_color_c(color_code4, mesh_m->node_node_color_code);
    return;
}

void set_ele_grp_color_code(int selected, float color_code4[4],
			struct mesh_menu_val *mesh_m){
    if(selected == SURFSOLID_TOGGLE){
        copy_rgba_color_c(color_code4, mesh_m->ele_surface_color_code);
        kemoview_set_ele_grp_opacity((double) color_code4[3]);
    } else if(selected == SURFGRID_TOGGLE){
        copy_rgba_color_c(color_code4, mesh_m->ele_grid_color_code);
    } else if(selected == SURFNOD_TOGGLE){
        copy_rgba_color_c(color_code4, mesh_m->ele_node_color_code);
    };
    return;
}

void set_surf_grp_color_code(int selected, float color_code4[4], 
			struct mesh_menu_val *mesh_m){
    if(selected == SURFSOLID_TOGGLE){
        copy_rgba_color_c(color_code4, mesh_m->surf_surface_color_code);
        kemoview_set_surf_grp_opacity((double) color_code4[3]);
    } else if(selected == SURFGRID_TOGGLE){
        copy_rgba_color_c(color_code4, mesh_m->surf_grid_color_code);
    } else if(selected == SURFNOD_TOGGLE){
        copy_rgba_color_c(color_code4, mesh_m->surf_node_color_code);
    };
    return;
}

void send_domain_color_code(struct mesh_menu_val *mesh_m, int selected,
			float color_code4[4]){
    if(selected == SURFSOLID_TOGGLE){
        copy_rgba_color_c(mesh_m->domain_surface_color_code, color_code4);
    } else if(selected == SURFGRID_TOGGLE){
        copy_rgba_color_c(mesh_m->domain_grid_color_code, color_code4);
    } else if(selected == SURFNOD_TOGGLE){
        copy_rgba_color_c(mesh_m->domain_node_color_code, color_code4);
    };
};

void send_node_grp_color_code(struct mesh_menu_val *mesh_m, float color_code4[4]){
    copy_rgba_color_c(mesh_m->node_node_color_code, color_code4);
    return;
}

void send_ele_grp_color_code(struct mesh_menu_val *mesh_m, int selected,
			float color_code4[4]){
    if(selected == SURFSOLID_TOGGLE){
        copy_rgba_color_c(mesh_m->ele_surface_color_code, color_code4);
    } else if(selected == SURFGRID_TOGGLE){
        copy_rgba_color_c(mesh_m->ele_grid_color_code, color_code4);
    } else if(selected == SURFNOD_TOGGLE){
        copy_rgba_color_c(mesh_m->ele_node_color_code, color_code4);
    };
    return;
}

void send_surf_grp_color_code(struct mesh_menu_val *mesh_m, int selected,
			float color_code4[4]){
    if(selected == SURFSOLID_TOGGLE){
        copy_rgba_color_c(mesh_m->surf_surface_color_code, color_code4);
    } else if(selected == SURFGRID_TOGGLE){
        copy_rgba_color_c(mesh_m->surf_grid_color_code, color_code4);
    } else if(selected == SURFNOD_TOGGLE){
        copy_rgba_color_c(mesh_m->surf_node_color_code, color_code4);
    };
    return;
}


void set_polygon_mode(int iflag, struct mesh_menu_val *mesh_m){mesh_m->polygon_mode = iflag;};
void set_axis_flag(int iflag, struct mesh_menu_val *mesh_m){mesh_m->iflag_draw_axis = iflag;};
void set_coastline_flag(int iflag, struct mesh_menu_val *mesh_m){mesh_m->iflag_draw_coast = iflag;};
void set_sphere_grid_flag(int iflag, struct mesh_menu_val *mesh_m){mesh_m->iflag_draw_sph_grid = iflag;};

int toggle_polygon_mode(struct mesh_menu_val *mesh_m){
	return mesh_m->polygon_mode = toggle_value_c(mesh_m->polygon_mode);
};
int toggle_draw_axis(struct mesh_menu_val *mesh_m){
	return mesh_m->iflag_draw_axis = toggle_value_c(mesh_m->iflag_draw_axis);
};
int toggle_coastline_flag(struct mesh_menu_val *mesh_m){
	return mesh_m->iflag_draw_coast = toggle_value_c(mesh_m->iflag_draw_coast);
};
int toggle_sphere_grid_flag(struct mesh_menu_val *mesh_m){
	return mesh_m->iflag_draw_sph_grid = toggle_value_c(mesh_m->iflag_draw_sph_grid);
};
