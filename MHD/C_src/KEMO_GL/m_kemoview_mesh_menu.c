
/* m_kemoview_mesh_menu.c */


#include "m_kemoview_mesh_menu.h"


static void set_draw_flag_for_all(int iflag, int ngrp, int *iflag_draw){
	int i;
	for (i=0; i<ngrp; i++){iflag_draw[i] = iflag;};
	return;
}

static void select_draw_flag_toggle(int igrp, int ngrp, int *iflag_draw){
	if(igrp == ngrp+1){set_draw_flag_for_all(IZERO, ngrp, iflag_draw);}
	else if(igrp == ngrp){set_draw_flag_for_all(IONE, ngrp, iflag_draw);}
	else {iflag_draw[igrp] = toggle_value_c(iflag_draw[igrp]);};
	return;
}


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

struct mesh_menu_val * alloc_mesh_menu_val(void){
    struct mesh_menu_val *mesh_m = (struct mesh_menu_val *) malloc(sizeof(struct mesh_menu_val));
    if(mesh_m == NULL){
        printf("malloc error for mesh_menu_val\n");
        exit(0);
    }
    
    return mesh_m;
};

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
	
	dealloc_kvstring(mesh_m->mesh_file_name);
	return;
}

static void init_single_color(float init_color[4]){
	init_color[0] = 0.3;
	init_color[1] = 0.8;
	init_color[2] = 0.8;
	init_color[3] = 1.0;
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
	
	mesh_m->iflag_draw_tangent_cyl = OFF;
    mesh_m->iflag_draw_coast =       OFF;
	mesh_m->iflag_draw_sph_grid =    OFF;
	mesh_m->radius_coast =           ONE;
    mesh_m->r_ICB =                  0.35;

	init_single_color(mesh_m->domain_surface_color_code);
	init_single_color(mesh_m->domain_grid_color_code);
	init_single_color(mesh_m->surf_surface_color_code);

	init_single_color(mesh_m->node_node_color_code);

	init_single_color(mesh_m->ele_surface_color_code);
	init_single_color(mesh_m->ele_grid_color_code);
	init_single_color(mesh_m->surf_grid_color_code);

	init_single_color(mesh_m->domain_surface_color_code);
	init_single_color(mesh_m->domain_surface_color_code);
	init_single_color(mesh_m->surf_node_color_code);
	return;
};


void set_mesh_color_mode(int icolor, struct mesh_menu_val *mesh_m)  {mesh_m->mesh_color_mode = icolor;};
void set_num_of_color_loop(int icolor, struct mesh_menu_val *mesh_m){mesh_m->num_of_color_loop = icolor;};

void set_node_diamater(double factor, int i_digit, struct mesh_menu_val *mesh_m){
	mesh_m->node_diam = const_from_digit_order(factor, i_digit);
};
void get_node_diamater(struct mesh_menu_val *mesh_m, double *factor, int *i_digit){
	find_order_digit(mesh_m->node_diam, factor, i_digit);
}

void set_domain_distance(double dist, struct mesh_menu_val *mesh_m){mesh_m->dist_domains = dist;};




void set_polygon_mode(int iflag, struct mesh_menu_val *mesh_m){mesh_m->polygon_mode = iflag;};
void set_axis_flag(int iflag, struct mesh_menu_val *mesh_m){mesh_m->iflag_draw_axis = iflag;};
void set_axis_position(int iflag, struct mesh_menu_val *mesh_m){mesh_m->iflag_axis_position = iflag;};


void set_coastline_flag(int iflag, struct mesh_menu_val *mesh_m){mesh_m->iflag_draw_coast = iflag;};
void set_sphere_grid_flag(int iflag, struct mesh_menu_val *mesh_m){mesh_m->iflag_draw_sph_grid = iflag;};
void set_tangent_cylinder_flag(int iflag, struct mesh_menu_val *mesh_m){mesh_m->iflag_draw_tangent_cyl = iflag;};

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



void set_mesh_draw_flag(int num_pe, int selected, int iflag, struct mesh_menu_val *mesh_m){
	if     (selected == SURFSOLID_TOGGLE){
		mesh_m->draw_surface_solid = iflag;
		set_draw_flag_for_all(mesh_m->draw_surface_solid, num_pe, mesh_m->draw_domains_solid);
	}else if(selected == SURFGRID_TOGGLE){
		mesh_m->draw_surface_grid = iflag;
		set_draw_flag_for_all(mesh_m->draw_surface_grid, num_pe, mesh_m->draw_domains_grid);
	}else if(selected == SURFNOD_TOGGLE){
		mesh_m->draw_surface_nod = iflag;
		set_draw_flag_for_all(mesh_m->draw_surface_nod, num_pe, mesh_m->draw_domains_nod);
	};
	return;
};

void mesh_draw_toggle(int num_pe, int selected, struct mesh_menu_val *mesh_m){
	if     (selected == SURFSOLID_TOGGLE){
		mesh_m->draw_surface_solid = toggle_value_c(mesh_m->draw_surface_solid);
		set_draw_flag_for_all(mesh_m->draw_surface_solid, num_pe, mesh_m->draw_domains_solid);
	}else if(selected == SURFGRID_TOGGLE){
		mesh_m->draw_surface_grid = toggle_value_c(mesh_m->draw_surface_grid);
		set_draw_flag_for_all(mesh_m->draw_surface_grid, num_pe, mesh_m->draw_domains_grid);
	}else if(selected == SURFNOD_TOGGLE){
		mesh_m->draw_surface_nod = toggle_value_c(mesh_m->draw_surface_nod);
		set_draw_flag_for_all(mesh_m->draw_surface_nod, num_pe, mesh_m->draw_domains_nod);
	};
	return;
};


void set_draw_domain_flag(int selected, int igrp, int iflag, struct mesh_menu_val *mesh_m){
	if(selected == SURFSOLID_TOGGLE){
		mesh_m->draw_surface_solid = IONE;
		mesh_m->draw_domains_solid[igrp] = iflag;
	} else if(selected == SURFGRID_TOGGLE){
		mesh_m->draw_surface_grid = IONE;
		mesh_m->draw_domains_grid[igrp] = iflag;
	} else if(selected == SURFNOD_TOGGLE){
		mesh_m->draw_surface_nod = IONE;
		mesh_m->draw_domains_nod[igrp] = iflag;
	};
	return;
};

void set_draw_nodgrp_flag(int igrp, int iflag, struct mesh_menu_val *mesh_m){
	mesh_m->draw_nodgrp_nod[igrp] = iflag;
	return;
};

void set_draw_elegrp_flag(int selected, int igrp, int iflag, struct mesh_menu_val *mesh_m){
	if(selected == SURFSOLID_TOGGLE){
		mesh_m->draw_elegrp_solid[igrp] = iflag;
	} else if(selected == SURFGRID_TOGGLE){
		mesh_m->draw_elegrp_grid[igrp] = iflag;
	} else if(selected == SURFNOD_TOGGLE){
		mesh_m->draw_elegrp_nod[igrp] = iflag;
	};
	return;
};

void set_draw_surfgrp_flag(int selected, int igrp, int iflag, struct mesh_menu_val *mesh_m){
	if(selected == SURFSOLID_TOGGLE){
		mesh_m->draw_surfgrp_solid[igrp] = iflag;
	} else if(selected == SURFGRID_TOGGLE){
		mesh_m->draw_surfgrp_grid[igrp] = iflag;
	} else if(selected == SURFNOD_TOGGLE){
		mesh_m->draw_surfgrp_nod[igrp] = iflag;
	};
	return;
};

void toggle_draw_domain_flag(int selected, int igrp, int ngrp, struct mesh_menu_val *mesh_m){
	if(selected == SURFSOLID_TOGGLE){
		select_draw_flag_toggle(igrp, ngrp, mesh_m->draw_domains_solid);
	} else if(selected == SURFGRID_TOGGLE){
		select_draw_flag_toggle(igrp, ngrp, mesh_m->draw_domains_grid);
	} else if(selected == SURFNOD_TOGGLE){
		select_draw_flag_toggle(igrp, ngrp, mesh_m->draw_domains_nod);
	};
	return;
};

void toggle_draw_nodgrp_flag(int igrp, int ngrp, struct mesh_menu_val *mesh_m){
	select_draw_flag_toggle(igrp, ngrp, mesh_m->draw_nodgrp_nod);
	return;
};

void toggle_draw_elegrp_flag(int selected, int igrp, int ngrp, struct mesh_menu_val *mesh_m){
	if(selected == SURFSOLID_TOGGLE){
		select_draw_flag_toggle(igrp, ngrp, mesh_m->draw_elegrp_solid);
	} else if(selected == SURFGRID_TOGGLE){
		select_draw_flag_toggle(igrp, ngrp, mesh_m->draw_elegrp_grid);
	} else if(selected == SURFNOD_TOGGLE){
		select_draw_flag_toggle(igrp, ngrp, mesh_m->draw_elegrp_nod);
	};
	return;
};

void toggle_draw_surfgrp_flag(int selected, int igrp, int ngrp, struct mesh_menu_val *mesh_m){
	if(selected == SURFSOLID_TOGGLE){
		select_draw_flag_toggle(igrp, ngrp, mesh_m->draw_surfgrp_solid);
	} else if(selected == SURFGRID_TOGGLE){
		select_draw_flag_toggle(igrp, ngrp, mesh_m->draw_surfgrp_grid);
	} else if(selected == SURFNOD_TOGGLE){
		select_draw_flag_toggle(igrp, ngrp, mesh_m->draw_surfgrp_nod);
	};
	return;
}

int get_draw_domain_flag(struct mesh_menu_val *mesh_m, int selected, int igrp){
	int iflag = 0;
	if(selected == SURFSOLID_TOGGLE){
		iflag = mesh_m->draw_domains_solid[igrp];
	} else if(selected == SURFGRID_TOGGLE){
		iflag = mesh_m->draw_domains_grid[igrp];
	} else if(selected == SURFNOD_TOGGLE){
		iflag = mesh_m->draw_domains_nod[igrp];
	};
	return iflag;
};

int get_draw_nodgrp_flag(struct mesh_menu_val *mesh_m, int igrp){
	return mesh_m->draw_nodgrp_nod[igrp];
};

int get_draw_elegrp_flag(struct mesh_menu_val *mesh_m, int selected, int igrp){
	int iflag = 0;
	if(selected == SURFSOLID_TOGGLE){
		iflag = mesh_m->draw_elegrp_solid[igrp];
	} else if(selected == SURFGRID_TOGGLE){
		iflag = mesh_m->draw_elegrp_grid[igrp];
	} else if(selected == SURFNOD_TOGGLE){
		iflag = mesh_m->draw_elegrp_nod[igrp];
	};
	return iflag;
};

int get_draw_surfgrp_flag(struct mesh_menu_val *mesh_m, int selected, int igrp){
	int iflag = 0;
	if(selected == SURFSOLID_TOGGLE){
		iflag = mesh_m->draw_surfgrp_solid[igrp];
	} else if(selected == SURFGRID_TOGGLE){
		iflag = mesh_m->draw_surfgrp_grid[igrp];
	} else if(selected == SURFNOD_TOGGLE){
		iflag = mesh_m->draw_surfgrp_nod[igrp];
	};
	return iflag;
};


void set_domain_color_flag(int selected, int icolor,
                           struct mesh_menu_val *mesh_m){
	if(selected == SURFSOLID_TOGGLE){
        mesh_m->domain_surface_color = icolor;
	}else if(selected == SURFGRID_TOGGLE){
		mesh_m->domain_grid_color = icolor;
	}else if(selected == SURFNOD_TOGGLE){
        mesh_m->domain_node_color = icolor;
	};
	return;
}
void set_node_grp_color_flag(int icolor, struct mesh_menu_val *mesh_m){
	mesh_m->node_node_color = icolor;
	return;
};
void set_ele_grp_color_flag(int selected, int icolor, struct mesh_menu_val *mesh_m){
	if(selected == SURFSOLID_TOGGLE){
		mesh_m->ele_surface_color = icolor;
	}else if(selected == SURFGRID_TOGGLE){
		mesh_m->ele_grid_color = icolor;
	}else if(selected == SURFNOD_TOGGLE){
		mesh_m->ele_node_color = icolor;
	};
    return;
}
void set_surf_grp_color_flag(int selected, int icolor, struct mesh_menu_val *mesh_m){
	if(selected == SURFSOLID_TOGGLE){
		mesh_m->surf_surface_color = icolor;
	}else if(selected == SURFGRID_TOGGLE){
		mesh_m->surf_grid_color = icolor;
	}else if(selected == SURFNOD_TOGGLE){
		mesh_m->surf_node_color = icolor;
	};
    return;
}

int get_domain_color_flag(int selected, struct mesh_menu_val *mesh_m){
	int icolor = 0;
	if(selected == SURFSOLID_TOGGLE){
		icolor = mesh_m->domain_node_color;
	}else if(selected == SURFGRID_TOGGLE){
		icolor = mesh_m->domain_grid_color;
	}else if(selected == SURFNOD_TOGGLE){
		icolor =  mesh_m->domain_surface_color;
	};
	return icolor;
}
int get_node_grp_color_flag(struct mesh_menu_val *mesh_m){
	return mesh_m->node_node_color;
};
int get_ele_grp_color_flag(int selected, struct mesh_menu_val *mesh_m){
	int icolor = 0;
	if(selected == SURFSOLID_TOGGLE){
		icolor = mesh_m->ele_surface_color;
	}else if(selected == SURFGRID_TOGGLE){
		icolor = mesh_m->ele_grid_color;
	}else if(selected == SURFNOD_TOGGLE){
		icolor = mesh_m->ele_node_color;
	};
    return icolor;
}
int get_surf_grp_color_flag(int selected, struct mesh_menu_val *mesh_m){
	int icolor = 0;
	if(selected == SURFSOLID_TOGGLE){
		icolor = mesh_m->surf_surface_color;
	}else if(selected == SURFGRID_TOGGLE){
		icolor = mesh_m->surf_grid_color;
	}else if(selected == SURFNOD_TOGGLE){
		icolor = mesh_m->surf_node_color;
	};
    return icolor;
}

void set_domain_color_code(int selected, float color_code4[4],
			struct mesh_menu_val *mesh_m){
    if(selected == SURFSOLID_TOGGLE){
        copy_rgba_color_c(color_code4, mesh_m->domain_surface_color_code);
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

