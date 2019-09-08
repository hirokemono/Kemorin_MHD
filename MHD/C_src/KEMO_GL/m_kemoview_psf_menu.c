
/* m_kemoview_psf_menu.c */


#include "m_kemoview_psf_menu.h"

void set_PSF_component_name(int ncomp, int icomp, char *comp_name) {
    int id_coord =    kemoview_get_PSF_coordinate_id();
	
	if(id_coord == 1){
		if(ncomp == 3){
			if(icomp == 0) sprintf(comp_name, "R");
			if(icomp == 1) sprintf(comp_name, "Theta");
			if(icomp == 2) sprintf(comp_name, "Phi");
		} else if(ncomp == 6){
			if(icomp == 0) sprintf(comp_name, "RR");
			if(icomp == 1) sprintf(comp_name, "R-Theta");
			if(icomp == 2) sprintf(comp_name, "R-Phi");
			if(icomp == 3) sprintf(comp_name, "Theta-Theta");
			if(icomp == 4) sprintf(comp_name, "Theta-Phi");
			if(icomp == 5) sprintf(comp_name, "Phi-Phi");
		};
	} else if(id_coord == 2){
		if(ncomp == 3){
			if(icomp == 0) sprintf(comp_name, "S");
			if(icomp == 1) sprintf(comp_name, "Phi");
			if(icomp == 2) sprintf(comp_name, "Z");
		} else if(ncomp == 6){
			if(icomp == 0) sprintf(comp_name, "SS");
			if(icomp == 1) sprintf(comp_name, "S-Phi");
			if(icomp == 2) sprintf(comp_name, "SZ");
			if(icomp == 3) sprintf(comp_name, "Phi-Phi");
			if(icomp == 4) sprintf(comp_name, "Phi-Z");
			if(icomp == 5) sprintf(comp_name, "ZZ");
		};
	} else {
		if(ncomp == 3){
			if(icomp == 0) sprintf(comp_name, "X");
			if(icomp == 1) sprintf(comp_name, "Y");
			if(icomp == 2) sprintf(comp_name, "Z");
		} else if(ncomp == 6){
			if(icomp == 0) sprintf(comp_name, "XX");
			if(icomp == 1) sprintf(comp_name, "XY");
			if(icomp == 2) sprintf(comp_name, "XZ");
			if(icomp == 3) sprintf(comp_name, "YY");
			if(icomp == 4) sprintf(comp_name, "YZ");
			if(icomp == 5) sprintf(comp_name, "ZZ");
		};
	};
	
	if(icomp == ncomp) sprintf(comp_name,"magnitude");
	
	return;
};


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

		set_color_mode_by_id(psf_m->cmap_psf_comp[i], RAINBOW_MODE);
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

		set_color_mode_by_id(psf_m->cmap_psf_fld[i], RAINBOW_MODE);
	}
	
	psf_m->icomp_draw_psf = 0;
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
	dealloc_kvstring(psf_m->psf_header);
	return;
}

void alloc_draw_psf_texture(struct psf_menu_val *psf_m){
	
	psf_m->texture_npix = psf_m->texture_width * psf_m->texture_height;
	if ((psf_m->texture_rgba = (GLubyte *) malloc( (4*psf_m->texture_npix) * sizeof(GLubyte))) == NULL) {
		exit(2);
	}
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
	
	psf_a->cbar_wk = (struct cbar_work *) malloc(sizeof(struct cbar_work));
	if( psf_a->cbar_wk == NULL ) {
		printf( "cbar_work cannot alloc!\n" );
		exit( 1 );
	}
	
	return;
};
void set_max_psf_loading(int ntot_psf_data, struct kemo_array_control *psf_a){
	psf_a->nlimit_loaded = ntot_psf_data;
	return;
};
void init_kemoview_array(struct kemo_array_control *psf_a){
	psf_a->num_loaded =  0;
	psf_a->nmax_loaded = 0;
	psf_a->id_current =  0;
	alloc_kemoview_array(psf_a);
	return;
};
void dealloc_kemoview_array(struct kemo_array_control *psf_a){
    dealloc_psfs_sorting_list(psf_a);
    
	free(psf_a->iflag_loaded);
	free(psf_a->cbar_wk);
	return;
};

void init_psf_parameters(struct psf_menu_val *psf_m){
	psf_m->if_draw_psf = INIT_IF_DRAW_PSF;
	psf_m->ic_draw_psf = INIT_IC_DRAW_PSF;
	psf_m->icomp_draw_psf = INIT_IC_DRAW_PSF;
	

	psf_m->polygon_mode_psf = INIT_POLYGON_MODE;
	psf_m->ivect_tangential = INIT_TANGENTIAL_VECT;
    psf_m->vector_thick = INIT_VECTOR_WIDTH;

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

void set_PSF_field(int selected, struct psf_data *psf_s, struct psf_menu_val *psf_m){
	psf_m->if_draw_psf = selected;
	psf_m->ic_draw_psf = IZERO;
	if(psf_s->ncomp[selected] != 3) psf_m->draw_psf_vect = IZERO;
	psf_m->icomp_draw_psf = psf_s->istack_comp[psf_m->if_draw_psf];
	printf("selected 1st component of %s, %d \n", 
			psf_s->data_name[psf_m->if_draw_psf], psf_m->if_draw_psf);
	return;
}

void set_PSF_component(int selected, struct psf_data *psf_s, struct psf_menu_val *psf_m){
	psf_m->ic_draw_psf = selected;
	psf_m->icomp_draw_psf = psf_s->istack_comp[psf_m->if_draw_psf] + psf_m->ic_draw_psf;
	printf("component %d  of %s, %d \n", (psf_m->ic_draw_psf+1),
			psf_s->data_name[psf_m->if_draw_psf], psf_m->icomp_draw_psf);
	return;
}

int get_PSF_maximum_load(struct kemo_array_control *psf_a){
	return psf_a->nlimit_loaded;
};

void psf_viewer_evolution(int istep, struct kemo_array_control *psf_a){
    psf_a->istep_sync = istep;
};

void set_PSF_num_loaded(int num, struct kemo_array_control *psf_a){
	psf_a->num_loaded = num;
};
void set_PSF_max_loaded(int num, struct kemo_array_control *psf_a){
	psf_a->nmax_loaded = num;
};
void set_loaded_PSF_flag(int id_psf, int iflag, struct kemo_array_control *psf_a){
	psf_a->iflag_loaded[id_psf] = iflag;
};
void set_current_PSF_to_menu(int id_psf, struct kemo_array_control *psf_a){
	psf_a->id_current = id_psf;
};

int get_PSF_num_loaded(struct kemo_array_control *psf_a){return psf_a->num_loaded;};
int get_PSF_max_loaded(struct kemo_array_control *psf_a){return psf_a->nmax_loaded;};
int get_PSF_loaded_flag(int id_psf, struct kemo_array_control *psf_a){
	return psf_a->iflag_loaded[id_psf];
};
int get_curent_PSF_ID(struct kemo_array_control *psf_a){return psf_a->id_current;};
int get_curent_PSF_filename(struct kemo_array_control *psf_a){return psf_a->id_current;};

int get_PSF_draw_switch(struct kemo_array_control *psf_a){
	return psf_a->iflag_loaded[psf_a->id_current];
};

