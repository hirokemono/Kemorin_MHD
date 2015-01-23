
/* m_kemoviewer_menu.c */


#include "m_kemoviewer_menu.h"

void alloc_psfs_sorting_list(struct kemo_array_control *psf_a){
    psf_a->z_ele_viz =    (double *)calloc(psf_a->ntot_psf_patch,sizeof(double));
    psf_a->ipsf_viz_far = (int *)calloc(psf_a->ntot_psf_patch,sizeof(int));
    psf_a->iele_viz_far = (int *)calloc(psf_a->ntot_psf_patch,sizeof(int));
    return;
}

void dealloc_psfs_sorting_list(struct kemo_array_control *psf_a){
    free(psf_a->z_ele_viz);
    free(psf_a->ipsf_viz_far);
    free(psf_a->iele_viz_far);

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

void alloc_draw_psf_flags(struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int i;
	psf_m->cmap_psf_comp =  (struct colormap_params **) malloc(psf_s->ncomptot*sizeof(struct colormap_params *));
	if( psf_m->cmap_psf_comp == NULL ) {
		printf( "cmap_psf_comp cannot alloc!\n" );
		exit( 1 );
	}
	for (i=0;i<psf_s->ncomptot;i++){
		psf_m->cmap_psf_comp[i] = (struct colormap_params *) malloc( sizeof(struct colormap_params));
		if(psf_m->cmap_psf_comp[i] == NULL) {
			printf( "psf_m->cmap_psf_comp[i] cannot alloc!\n" );
			exit( 1 );
		}
        alloc_single_color_code(psf_m->cmap_psf_comp[i]);
		alloc_color_index_list_s(psf_m->cmap_psf_comp[i], RAINBOW_MODE, ITWO);
		alloc_opacity_index_list_s(psf_m->cmap_psf_comp[i], ITWO);
	};
	
	psf_m->cmap_psf_fld =  (struct colormap_params **) malloc(psf_s->nfield*sizeof(struct colormap_params *));
	if( psf_m->cmap_psf_fld == NULL ) {
		printf( "cmap_psf_fld cannot alloc!\n" );
		exit( 1 );
	}
	for (i=0;i<psf_s->nfield;i++) {
		psf_m->cmap_psf_fld[i] = (struct colormap_params *) malloc( sizeof(struct colormap_params));
		if(psf_m->cmap_psf_fld[i] == NULL) {
			printf( "psf_m->cmap_psf_fld[i] cannot alloc!\n" );
			exit( 1 );
		}
        alloc_single_color_code(psf_m->cmap_psf_fld[i]);
		alloc_color_index_list_s(psf_m->cmap_psf_fld[i], RAINBOW_MODE, ITWO);
		alloc_opacity_index_list_s(psf_m->cmap_psf_fld[i], ITWO);
	}
	
	psf_m->cmap_psf = psf_m->cmap_psf_comp[0];
	return;
}

void alloc_draw_fline_flags(struct psf_data *fline_s, struct fline_menu_val *fline_m){
	int i;
	fline_m->cmap_fline_comp =  (struct colormap_params **) malloc(fline_s->ncomptot*sizeof(struct colormap_params *));
	if( fline_m->cmap_fline_comp == NULL ) {
		printf( "cmap_fline_comp cannot alloc!\n" );
		exit( 1 );
	}
	for (i=0;i<fline_s->ncomptot;i++){
		fline_m->cmap_fline_comp[i] = (struct colormap_params *) malloc( sizeof(struct colormap_params));
		if(fline_m->cmap_fline_comp[i] == NULL) {
			printf( "fline_m->cmap_fline_comp[i] cannot alloc!\n" );
			exit( 1 );
		}
		alloc_color_index_list_s(fline_m->cmap_fline_comp[i], RAINBOW_MODE, ITWO);
		alloc_opacity_index_list_s(fline_m->cmap_fline_comp[i], ITWO);
	};
	
	fline_m->cmap_fline_fld =  (struct colormap_params **) malloc(fline_s->nfield*sizeof(struct colormap_params *));
	if( fline_m->cmap_fline_fld == NULL ) {
		printf( "cmap_fline_fld cannot alloc!\n" );
		exit( 1 );
	}
	for (i=0;i<fline_s->nfield;i++) {
		fline_m->cmap_fline_fld[i] = (struct colormap_params *) malloc( sizeof(struct colormap_params));
		if(fline_m->cmap_fline_fld[i] == NULL) {
			printf( "fline_m->cmap_fline_fld[i] cannot alloc!\n" );
			exit( 1 );
		}
		alloc_color_index_list_s(fline_m->cmap_fline_fld[i], RAINBOW_MODE, ITWO);
		alloc_opacity_index_list_s(fline_m->cmap_fline_fld[i], ITWO);
	}
	
	fline_m->cmap_fline = fline_m->cmap_fline_comp[0];
	return;
}

void alloc_draw_psf_texture(struct psf_menu_val *psf_m){
	
	psf_m->texture_npix = psf_m->texture_width * psf_m->texture_height;
	if ((psf_m->texture_rgba = (GLubyte *) malloc( (4*psf_m->texture_npix) * sizeof(GLubyte))) == NULL) {
		exit(2);
	}
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
	return;
}

void dealloc_draw_psf_flags(struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int i;
	
	for (i=0;i<psf_s->nfield;i++){
		dealloc_opacity_index_list_s(psf_m->cmap_psf_fld[i]);
		dealloc_color_index_list_s(psf_m->cmap_psf_fld[i]);
		free(psf_m->cmap_psf_fld[i]);
	};
	free(psf_m->cmap_psf_fld);
	
	for (i=0;i<psf_s->ncomptot;i++){
		dealloc_opacity_index_list_s(psf_m->cmap_psf_comp[i]);
		dealloc_color_index_list_s(psf_m->cmap_psf_comp[i]);
		free(psf_m->cmap_psf_comp[i]);
	};
	free(psf_m->cmap_psf_comp);
	return;
}

void dealloc_draw_fline_flags(struct psf_data *fline_s, struct fline_menu_val *fline_m){
	int i;
	
	for (i=0;i<fline_s->nfield;i++){
		dealloc_opacity_index_list_s(fline_m->cmap_fline_fld[i]);
		dealloc_color_index_list_s(fline_m->cmap_fline_fld[i]);
		free(fline_m->cmap_fline_fld[i]);
	};
	free(fline_m->cmap_fline_fld);
	
	for (i=0;i<fline_s->ncomptot;i++){
		dealloc_opacity_index_list_s(fline_m->cmap_fline_comp[i]);
		dealloc_color_index_list_s(fline_m->cmap_fline_comp[i]);
		free(fline_m->cmap_fline_comp[i]);
	};
	free(fline_m->cmap_fline_comp);
	return;
}

void dealloc_draw_psf_texture(struct psf_menu_val *psf_m){
	free(psf_m->texture_rgba);
	return;
}

void alloc_kemoview_array(struct kemo_array_control *psf_a){
	psf_a->iflag_loaded = (int *) calloc(psf_a->nlimit_loaded,sizeof(int));
    
    psf_a->ntot_psf_patch = 0;
    psf_a->istack_solid_psf_txtur = 0;
    psf_a->istack_solid_psf_patch = 0;
    psf_a->istack_trans_psf_txtur = 0;
    psf_a->istack_trans_psf_patch = 0;
    alloc_psfs_sorting_list(psf_a);
	return;
};
void init_kemoview_array(int ntot_psf_data, struct kemo_array_control *psf_a){
	psf_a->nlimit_loaded = ntot_psf_data;
	psf_a->num_loaded =  0;
	psf_a->nmax_loaded = 0;
	psf_a->id_current =  0;
	alloc_kemoview_array(psf_a);
	return;
};
void dealloc_kemoview_array(struct kemo_array_control *psf_a){
    dealloc_psfs_sorting_list(psf_a);
    
	free(psf_a->iflag_loaded);
	return;
};


void init_viewer_parameters(struct mesh_menu_val *mesh_m){
	mesh_m->iflag_streo_stutter =  SHUTTER_OFF;
	mesh_m->iflag_streo_anaglyph = ANAGLYPH_ON;
	
	mesh_m->draw_surface_solid = INIT_DRAW_SOLID;
	mesh_m->draw_surface_grid =  INIT_DRAW_LINE;
	mesh_m->draw_surface_nod =   INIT_DRAW_NOD;
	
	mesh_m->iflag_draw_axis =    INIT_DRAW_LINE;
	
	mesh_m->mesh_color_mode =    INIT_COLOR_MODE;
	mesh_m->num_of_color_loop =  INIT_NUM_COLOR_LOOP;
	
	mesh_m->shading_mode =  INIT_SHADING_MODE;
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

void init_psf_parameters(struct psf_menu_val *psf_m){
	psf_m->if_draw_psf = INIT_IF_DRAW_PSF;
	psf_m->ic_draw_psf = INIT_IC_DRAW_PSF;
	psf_m->icomp_draw_psf = INIT_IC_DRAW_PSF;
	

	psf_m->polygon_mode_psf = INIT_POLYGON_MODE;
	psf_m->ivect_tangential = INIT_TANGENTIAL_VECT;

	psf_m->draw_psf_solid = IONE;
	psf_m->draw_psf_grid = IZERO;
	psf_m->draw_psf_zero = IZERO;
	psf_m->draw_psf_cbar = IZERO;
	psf_m->draw_psf_vect = IZERO;
	
	psf_m->psf_patch_color = RAINBOW_SURFACE;
	psf_m->isoline_color =   INIT_ISOLINE_COLOR;
	psf_m->n_isoline =       INIT_N_ISOLINE;
	
	psf_m->scale_vect =         ONE;
	psf_m->increment_vect =     IONE;
	psf_m->vector_patch_color = RAINBOW_SURFACE;
	return;
};

void init_fline_parameters(struct fline_menu_val *fline_m){
	fline_m ->iflag_draw_fline =  IZERO;
	
	fline_m->if_draw_fline = INIT_IF_DRAW_FLINE;
	fline_m->ic_draw_fline = INIT_IC_DRAW_FLINE;
	fline_m->icomp_draw_fline = INIT_IC_DRAW_FLINE;
	
	fline_m->fieldline_color = INIT_FLDLINE_COLOR;
	fline_m->fieldline_type = INIT_FLDLINE_TYPE;
	fline_m->fieldline_thick = INIT_FLDLINE_THICK;
	return;
}


void set_draw_flag_for_all(int iflag, int ngrp, int *iflag_draw){
	int i;
	for (i=0; i<ngrp; i++){iflag_draw[i] = iflag;};
	return;
}

void select_draw_flag_toggle(int selected, int ngrp, int *iflag_draw){
	if(selected == ngrp+1){set_draw_flag_for_all(IZERO, ngrp, iflag_draw);}
	else if(selected == ngrp){set_draw_flag_for_all(IONE, ngrp, iflag_draw);}
	else {iflag_draw[selected] = toggle_value_c(iflag_draw[selected]);};
	return;
}


void select_domain_node_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->domain_node_color = selected;
	return;
}
void select_domain_grid_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->domain_grid_color = selected;
	return;
}

void select_ele_grp_node_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->ele_node_color = selected;
	return;
}
void select_ele_grp_grid_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->ele_grid_color = selected;
	return;
}

void select_surf_grp_node_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->surf_node_color = selected;
	return;
}
void select_surf_grp_grid_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->surf_grid_color = selected;
	return;
}

void select_node_grp_node_color(int selected, struct mesh_menu_val *mesh_m){
	mesh_m->node_node_color = selected;
	return;
}

void set_shading_mode(int iflag, struct mesh_menu_val *mesh_m){mesh_m->shading_mode = iflag;};
void set_polygon_mode(int iflag, struct mesh_menu_val *mesh_m){mesh_m->polygon_mode = iflag;};
void set_axis_flag(int iflag, struct mesh_menu_val *mesh_m){mesh_m->iflag_draw_axis = iflag;};
void set_coastline_flag(int iflag, struct mesh_menu_val *mesh_m){mesh_m->iflag_draw_coast = iflag;};
void set_sphere_grid_flag(int iflag, struct mesh_menu_val *mesh_m){mesh_m->iflag_draw_sph_grid = iflag;};

int toggle_shading_mode(struct mesh_menu_val *mesh_m){
	return mesh_m->iflag_draw_sph_grid = toggle_value_c(mesh_m->iflag_draw_sph_grid);
};
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


void set_PSF_field(int selected, struct psf_data *psf_s, struct psf_menu_val *psf_m){
	psf_m->if_draw_psf = selected;
	psf_m->ic_draw_psf = IZERO;
	if(psf_s->ncomp[selected] != 3) psf_m->draw_psf_vect = IZERO;
	psf_m->icomp_draw_psf = psf_s->istack_comp[psf_m->if_draw_psf];
    psf_m->cmap_psf = psf_m->cmap_psf_comp[psf_m->icomp_draw_psf];
	printf("selected 1st component of %s, %d \n", 
			psf_s->data_name[psf_m->if_draw_psf], psf_m->if_draw_psf);
	return;
}

void set_PSF_component(int selected, struct psf_data *psf_s, struct psf_menu_val *psf_m){
	psf_m->ic_draw_psf = selected;
	psf_m->icomp_draw_psf = psf_s->istack_comp[psf_m->if_draw_psf] + psf_m->ic_draw_psf;
    psf_m->cmap_psf = psf_m->cmap_psf_comp[psf_m->icomp_draw_psf];
	printf("component %d  of %s, %d \n", (psf_m->ic_draw_psf+1),
			psf_s->data_name[psf_m->if_draw_psf], psf_m->icomp_draw_psf);
	return;
}


void set_fline_color_field(int selected, struct psf_data *fline_s,
			struct fline_menu_val *fline_m){
	fline_m->if_draw_fline = selected;
	fline_m->ic_draw_fline = IZERO;
	fline_m->icomp_draw_fline = fline_s->istack_comp[fline_m->if_draw_fline];
	fline_m->cmap_fline = fline_m->cmap_fline_comp[fline_m->icomp_draw_fline];
	printf("selected 1st component of %s, %d \n", 
			fline_s->data_name[fline_m->if_draw_fline], fline_m->if_draw_fline);
	return;
}

void set_fline_color_component(int selected, struct psf_data *fline_s,
			struct fline_menu_val *fline_m){
	fline_m->ic_draw_fline = selected;
	fline_m->icomp_draw_fline = fline_s->istack_comp[fline_m->if_draw_fline] + fline_m->ic_draw_fline;
	fline_m->cmap_fline = fline_m->cmap_fline_comp[fline_m->icomp_draw_fline];
	printf("selected %d  of %s, %d \n", (selected+1), 
			fline_s->data_name[fline_m->if_draw_fline], fline_m->icomp_draw_fline);
	return;
}

