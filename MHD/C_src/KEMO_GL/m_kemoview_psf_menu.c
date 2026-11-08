
/* m_kemoview_psf_menu.c */


#include "m_kemoview_psf_menu.h"


void set_PSF_component_name(int ncomp, int id_coord, int icomp, char *comp_name) {
    if(id_coord == 1){
        if(ncomp == 1){
            if(icomp == 0) sprintf(comp_name, "%s", hd_scalar);
        }else if(ncomp == 3){
            if(icomp == 0) sprintf(comp_name, "%s", hd_r);
            if(icomp == 1) sprintf(comp_name, "%s", hd_t);
            if(icomp == 2) sprintf(comp_name, "%s", hd_p);
        }else if(ncomp == 6){
            if(icomp == 0) sprintf(comp_name, "%s", hd_rr);
            if(icomp == 1) sprintf(comp_name, "%s", hd_rt);
            if(icomp == 2) sprintf(comp_name, "%s", hd_rp);
            if(icomp == 3) sprintf(comp_name, "%s", hd_tt);
            if(icomp == 4) sprintf(comp_name, "%s", hd_tp);
            if(icomp == 5) sprintf(comp_name, "%s", hd_pp);
        };
    } else if(id_coord == 2){
        if(ncomp == 1){
            if(icomp == 0) sprintf(comp_name, "%s", hd_scalar);
        }else if(ncomp == 3){
            if(icomp == 0) sprintf(comp_name, "%s", hd_s);
            if(icomp == 1) sprintf(comp_name, "%s", hd_p);
            if(icomp == 2) sprintf(comp_name, "%s", hd_z);
        }else if(ncomp == 6){
            if(icomp == 0) sprintf(comp_name, "%s", hd_ss);
            if(icomp == 1) sprintf(comp_name, "%s", hd_sp);
            if(icomp == 2) sprintf(comp_name, "%s", hd_sz);
            if(icomp == 3) sprintf(comp_name, "%s", hd_pp);
            if(icomp == 4) sprintf(comp_name, "%s", hd_pz);
            if(icomp == 5) sprintf(comp_name, "%s", hd_zz);
        };
    } else {
        if(ncomp == 1){
            if(icomp == 0) sprintf(comp_name, "%s", hd_scalar);
        }else if(ncomp == 3){
            if(icomp == 0) sprintf(comp_name, "%s", hd_x);
            if(icomp == 1) sprintf(comp_name, "%s", hd_y);
            if(icomp == 2) sprintf(comp_name, "%s", hd_z);
        }else if(ncomp == 6){
            if(icomp == 0) sprintf(comp_name, "%s", hd_xx);
            if(icomp == 1) sprintf(comp_name, "%s", hd_xy);
            if(icomp == 2) sprintf(comp_name, "%s", hd_xz);
            if(icomp == 3) sprintf(comp_name, "%s", hd_yy);
            if(icomp == 4) sprintf(comp_name, "%s", hd_yz);
            if(icomp == 5) sprintf(comp_name, "%s", hd_zz);
        };
    };
    
    if(icomp == ncomp) sprintf(comp_name, "%s", hd_magitude);
    
    return;
};

void set_PSF_full_component_name(int ncomp, int id_coord, int icomp, char *comp_name) {
    if(id_coord == 1){
        if(ncomp == 1){
            if(icomp == 0) sprintf(comp_name, "%s", hd_scalar);
        }else if(ncomp == 3){
            if(icomp ==  0) sprintf(comp_name, "%s", hd_r);
            if(icomp ==  1) sprintf(comp_name, "%s", hd_t);
            if(icomp ==  2) sprintf(comp_name, "%s", hd_p);
            if(icomp ==  3) sprintf(comp_name, "%s", hd_s);
            
            if(icomp ==  4) sprintf(comp_name, "%s", hd_x);
            if(icomp ==  5) sprintf(comp_name, "%s", hd_y);
            if(icomp ==  6) sprintf(comp_name, "%s", hd_z);
        }else if(ncomp == 6){
            if(icomp ==  0) sprintf(comp_name, "%s", hd_rr);
            if(icomp ==  1) sprintf(comp_name, "%s", hd_rt);
            if(icomp ==  2) sprintf(comp_name, "%s", hd_rp);
            if(icomp ==  3) sprintf(comp_name, "%s", hd_tt);
            if(icomp ==  4) sprintf(comp_name, "%s", hd_tp);
            if(icomp ==  5) sprintf(comp_name, "%s", hd_pp);
            
            if(icomp ==  6) sprintf(comp_name, "%s", hd_ss);
            if(icomp ==  7) sprintf(comp_name, "%s", hd_sp);
            if(icomp ==  8) sprintf(comp_name, "%s", hd_sz);
            if(icomp ==  9) sprintf(comp_name, "%s", hd_pz);
            if(icomp == 10) sprintf(comp_name, "%s", hd_zz);
            
            if(icomp == 11) sprintf(comp_name, "%s", hd_xx);
            if(icomp == 12) sprintf(comp_name, "%s", hd_xy);
            if(icomp == 13) sprintf(comp_name, "%s", hd_xz);
            if(icomp == 14) sprintf(comp_name, "%s", hd_yy);
            if(icomp == 15) sprintf(comp_name, "%s", hd_yz);
        };
    } else if(id_coord == 2){
        if(ncomp == 1){
            if(icomp == 0) sprintf(comp_name, "%s", hd_scalar);
        }else if(ncomp == 3){
            if(icomp ==  0) sprintf(comp_name, "%s", hd_s);
            if(icomp ==  1) sprintf(comp_name, "%s", hd_p);
            if(icomp ==  2) sprintf(comp_name, "%s", hd_z);
            if(icomp ==  3) sprintf(comp_name, "%s", hd_r);
            if(icomp ==  4) sprintf(comp_name, "%s", hd_t);
            
            if(icomp ==  5) sprintf(comp_name, "%s", hd_x);
            if(icomp ==  6) sprintf(comp_name, "%s", hd_y);
        }else if(ncomp == 6){
            if(icomp ==  0) sprintf(comp_name, "%s", hd_ss);
            if(icomp ==  1) sprintf(comp_name, "%s", hd_sp);
            if(icomp ==  2) sprintf(comp_name, "%s", hd_sz);
            if(icomp ==  3) sprintf(comp_name, "%s", hd_pp);
            if(icomp ==  4) sprintf(comp_name, "%s", hd_pz);
            if(icomp ==  5) sprintf(comp_name, "%s", hd_zz);
            
            if(icomp ==  6) sprintf(comp_name, "%s", hd_rr);
            if(icomp ==  7) sprintf(comp_name, "%s", hd_rt);
            if(icomp ==  8) sprintf(comp_name, "%s", hd_rp);
            if(icomp ==  9) sprintf(comp_name, "%s", hd_tt);
            if(icomp == 10) sprintf(comp_name, "%s", hd_tp);
            
            if(icomp == 11) sprintf(comp_name, "%s", hd_xx);
            if(icomp == 12) sprintf(comp_name, "%s", hd_xy);
            if(icomp == 13) sprintf(comp_name, "%s", hd_xz);
            if(icomp == 14) sprintf(comp_name, "%s", hd_yy);
            if(icomp == 15) sprintf(comp_name, "%s", hd_yz);
        };
    } else {
        if(ncomp == 1){
            if(icomp == 0) sprintf(comp_name, "%s", hd_scalar);
        }else if(ncomp == 3){
            if(icomp ==  0) sprintf(comp_name, "%s", hd_x);
            if(icomp ==  1) sprintf(comp_name, "%s", hd_y);
            if(icomp ==  2) sprintf(comp_name, "%s", hd_z);
            
            if(icomp ==  3) sprintf(comp_name, "%s", hd_r);
            if(icomp ==  4) sprintf(comp_name, "%s", hd_t);
            if(icomp ==  5) sprintf(comp_name, "%s", hd_p);
            if(icomp ==  6) sprintf(comp_name, "%s", hd_s);
        }else if(ncomp == 6){
            if(icomp ==  0) sprintf(comp_name, "%s", hd_xx);
            if(icomp ==  1) sprintf(comp_name, "%s", hd_xy);
            if(icomp ==  2) sprintf(comp_name, "%s", hd_xz);
            if(icomp ==  3) sprintf(comp_name, "%s", hd_yy);
            if(icomp ==  4) sprintf(comp_name, "%s", hd_yz);
            if(icomp ==  5) sprintf(comp_name, "%s", hd_zz);
            
            if(icomp ==  6) sprintf(comp_name, "%s", hd_rr);
            if(icomp ==  7) sprintf(comp_name, "%s", hd_rt);
            if(icomp ==  8) sprintf(comp_name, "%s", hd_rp);
            if(icomp ==  9) sprintf(comp_name, "%s", hd_tt);
            if(icomp == 10) sprintf(comp_name, "%s", hd_tp);
            if(icomp == 11) sprintf(comp_name, "%s", hd_pp);
            
            if(icomp == 12) sprintf(comp_name, "%s", hd_ss);
            if(icomp == 13) sprintf(comp_name, "%s", hd_sp);
            if(icomp == 14) sprintf(comp_name, "%s", hd_sz);
            if(icomp == 15) sprintf(comp_name, "%s", hd_pz);
        };
    };
    
    if(icomp == ncomp) sprintf(comp_name, "%s", hd_magitude);
    
    return;
};



void alloc_psfs_sorting_list(struct kemo_array_control *psf_a){
    psf_a->ipsf_viz_far = (int *)calloc(psf_a->ntot_psf_patch,sizeof(int));
    psf_a->iele_viz_far = (int *)calloc(psf_a->ntot_psf_patch,sizeof(int));
    return;
}

void dealloc_psfs_sorting_list(struct kemo_array_control *psf_a){
    free(psf_a->ipsf_viz_far);
    free(psf_a->iele_viz_far);
    return;
}

struct psf_menu_val *  init_psf_menu_val(void){
    struct psf_menu_val *psf_m = (struct psf_menu_val *) malloc(sizeof(struct psf_menu_val));
    if(psf_m  == NULL) {
        printf("malloc error for psf_menu_val\n");
        exit(1);
    }
    
    psf_m->ncorner_viz_line = ISIX;
    return psf_m;
}

void dealloc_psf_menu_val(struct psf_menu_val *psf_m){
    free(psf_m);
}

void alloc_draw_psf_flags(int id_color_mode, long ncomptot, long npicktot,
                          struct psf_menu_val *psf_m){
    int i;
        
    psf_m->cmap_viz_pick =  (struct colormap_params **) malloc(npicktot*sizeof(struct colormap_params *));
    if(psf_m->cmap_viz_pick == NULL ) {
        printf( "cmap_viz_pick cannot alloc!\n" );
        exit( 1 );
    }
    for(i=0;i<npicktot;i++){
        psf_m->cmap_viz_pick[i] = (struct colormap_params *) malloc( sizeof(struct colormap_params));
        if(psf_m->cmap_viz_pick[i] == NULL) {
            printf( "psf_m->cmap_viz_pick[i] cannot alloc!\n" );
            exit( 1 );
        }
        alloc_color_index_list_s(psf_m->cmap_viz_pick[i], id_color_mode);
    };
        
    psf_m->ipick_draw_viz = 0;
    return;
}

void dealloc_draw_psf_flags(long ncomptot, long npicktot,
                            struct psf_menu_val *psf_m){
    int i;
    for(i=0;i<npicktot;i++){dealloc_color_index_list_s(psf_m->cmap_viz_pick[i]);};
    free(psf_m->cmap_viz_pick);
    dealloc_kvstring(psf_m->viz_prefix_c);
    return;
}

void alloc_kemoview_array(struct kemo_array_control *psf_a){
    psf_a->iflag_loaded = (int *) calloc(psf_a->nlimit_loaded,sizeof(int));
    if( psf_a->iflag_loaded == NULL ) {
        printf( "Failed allocation for psf_a->iflag_loaded\n" );
        exit( 1 );
    }

    psf_a->istack_all_psf_node = (long *) calloc(psf_a->nlimit_loaded+1,sizeof(long));
    if( psf_a->istack_all_psf_node == NULL ) {
        printf( "Failed allocation for psf_a->istack_all_psf_node\n" );
        exit( 1 );
    }
    
    psf_a->ntot_psf_patch = 0;
    psf_a->istack_solid_psf_txtur = 0;
    psf_a->istack_solid_psf_patch = 0;
    psf_a->istack_trans_psf_txtur = 0;
    psf_a->istack_trans_psf_patch = 0;
    alloc_psfs_sorting_list(psf_a);
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
    psf_a->ipsf_texured = -1;
    alloc_kemoview_array(psf_a);
    psf_a->psf_texure = alloc_kemoview_gl_texure();
    return;
};
void dealloc_kemoview_array(struct kemo_array_control *psf_a){
    dealloc_psfs_sorting_list(psf_a);
    dealloc_kemoview_gl_texure(psf_a->psf_texure);
    free(psf_a->iflag_loaded);
    free(psf_a->istack_all_psf_node);
    return;
};


void init_psf_parameters(struct psf_menu_val *psf_m){
    psf_m->if_draw_viz =    INIT_IF_DRAW_PSF;
    psf_m->ic_draw_viz =    INIT_IC_DRAW_PSF;

	psf_m->ncomp_draw_viz = IONE;
	psf_m->ist_draw_viz =   INIT_IF_DRAW_PSF;
	psf_m->ipick_draw_viz = INIT_IC_DRAW_PSF;

    psf_m->polygon_mode_psf = INIT_POLYGON_MODE;
    psf_m->ivect_tangential = INIT_TANGENTIAL_VECT;
    psf_m->vector_thick = INIT_VECTOR_WIDTH;

    psf_m->iflag_draw_viz =  IONE;
    psf_m->draw_psf_grid =   IZERO;
    psf_m->draw_psf_zero =   IZERO;
    psf_m->iflag_draw_cbar = IZERO;
    psf_m->draw_psf_vect =   IZERO;
    
    psf_m->viz_color_mode =  COLORED_BY_DATA;
    psf_m->isoline_color =   INIT_ISOLINE_COLOR;
    psf_m->n_isoline =       INIT_N_ISOLINE;
    psf_m->viz_line_width =  INIT_ISOLINE_WIDTH;
    
    psf_m->scale_vect =         ONE;
    psf_m->increment_vect =     IONE;
    psf_m->vector_patch_color = COLORED_BY_DATA;
    return;
};

void set_VIZ_field(int selected, char *data_name,
                   long *istack_comp, long *istack_pick,
                   struct psf_menu_val *psf_m){
    psf_m->if_draw_viz =  selected;
    psf_m->ist_draw_viz =   (int) istack_comp[psf_m->if_draw_viz];
    psf_m->ncomp_draw_viz = (int) istack_comp[psf_m->if_draw_viz+1] - psf_m->ist_draw_viz;
    if(psf_m->ncomp_draw_viz != 3) psf_m->draw_psf_vect = IZERO;
 
    psf_m->ic_draw_viz = IZERO;
    psf_m->ipick_draw_viz = psf_m->ic_draw_viz + istack_pick[psf_m->if_draw_viz];
    
    printf("selected 1st component of %s, %d \n",
           data_name, psf_m->if_draw_viz);
    return;
}

void set_VIZ_component(int selected, char *data_name, 
                       long *istack_pick,
                       struct psf_menu_val *psf_m){
    psf_m->ic_draw_viz = selected;
    psf_m->ipick_draw_viz = psf_m->ic_draw_viz + istack_pick[psf_m->if_draw_viz];

    printf("component %d  of %s, %ld \n", (psf_m->ic_draw_viz+1),
           data_name, psf_m->ipick_draw_viz);
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

void set_iflag_draw_time(double time, struct psf_menu_val *psf_m){
    if(   psf_m->iformat_viz_file == IFLAG_PSF_BIN
       || psf_m->iformat_viz_file == IFLAG_PSF_BIN_GZ
       || psf_m->iformat_viz_file == IFLAG_SURF_SDT
       || psf_m->iformat_viz_file == IFLAG_SURF_SDT_GZ){
        psf_m->iflag_draw_time = 1;
        psf_m->time = time;
    }else{
        psf_m->iflag_draw_time = 0;
    }
    return;
}
