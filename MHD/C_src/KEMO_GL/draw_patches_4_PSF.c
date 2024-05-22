
/* draw_patches_4_PSF.c */

#include "draw_patches_4_PSF.h"

#define ARCPI 0.318309886

void const_PSF_node_stack(struct psf_data **psf_s,
                          struct kemo_array_control *psf_a){
    psf_a->istack_all_psf_node[0] = 0;
    for(int i_psf=0;i_psf<psf_a->nmax_loaded;i_psf++){
        if(psf_a->iflag_loaded[i_psf] == 0){
            psf_a->istack_all_psf_node[i_psf+1] = psf_a->istack_all_psf_node[i_psf];
        }else{
            psf_a->istack_all_psf_node[i_psf+1] = psf_a->istack_all_psf_node[i_psf]
                                                 + psf_s[i_psf]->nnod_viz;
        }
    }
    return;
}

void const_PSF_node_buffer(const int nthreads,
                           struct psf_data **psf_s,
                           struct kemo_array_control *psf_a,
                           struct gl_strided_buffer *psf_buf){
    set_buffer_address_4_patch(psf_a->istack_all_psf_node[psf_a->nmax_loaded], psf_buf);
	if(psf_buf->num_nod_buf <= 0) return;
	
	resize_strided_buffer(psf_buf);
    
    long num_patch = 0;
    for(int i_psf=0;i_psf<psf_a->nmax_loaded;i_psf++){
        if(psf_a->iflag_loaded[i_psf] == 0) continue;
        num_patch = sel_psf_nodes_to_buf_pthread(psf_a->istack_all_psf_node[i_psf], nthreads,
                                                 IZERO, psf_s[i_psf]->nnod_viz,
                                                 i_psf, psf_s, psf_buf);
    }
	return;
}

void const_PSF_patch_index_buffer(const int nthreads, long ist_psf, long ied_psf,
                                  struct psf_data **psf_s,  struct kemo_array_control *psf_a,
                                  struct gl_index_buffer *index_buf){
    long num_patch = count_psf_nodes_to_buf(ist_psf, ied_psf);
    resize_gl_index_buffer(num_patch, ITHREE, index_buf);
    if(index_buf->ntot_vertex <= 0) return;
    
    num_patch = set_psf_patch_indices_to_buf(0, ist_psf, ied_psf, psf_s, psf_a, index_buf);
    return;
}

static void const_PSF_patch_buffer(const int nthreads, long ist_psf, long ied_psf,
                                   struct psf_data **psf_s,
                                   struct kemo_array_control *psf_a,
                                   struct gl_strided_buffer *psf_buf){
    long num_patch = count_psf_nodes_to_buf(ist_psf, ied_psf);
    set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
    if(psf_buf->num_nod_buf <= 0) return;
    
    resize_strided_buffer(psf_buf);
    num_patch = sel_psf_patches_to_buf_pthread(0, nthreads, ist_psf, ied_psf,
                                               psf_s, psf_a, psf_buf);
    return;
}

static void const_PSF_texture_buffer(int shading_mode, const int nthreads,
                                     long ist_psf, long ied_psf,
                                     struct psf_data **psf_s, struct psf_menu_val **psf_m,
                                     struct kemo_array_control *psf_a,
                                     struct gl_strided_buffer *psf_buf){
    const_PSF_patch_buffer(nthreads, ist_psf, ied_psf,
                           psf_s, psf_a, psf_buf);
    if(psf_buf->num_nod_buf > 0){
        sel_psf_textures_to_buf_pthread(nthreads, ist_psf, ied_psf,
                                        psf_s, psf_a, psf_buf);
    };
    return;
}

static void const_PSF_arrow_buffer(const int nthreads, struct view_element *view_s,
                                   struct psf_data **psf_s, struct psf_menu_val **psf_m,
                                   struct kemo_array_control *psf_a,
                                   struct gl_strided_buffer *psf_buf){
    int i;
    long **istack_smp_arrow = (long **) malloc(psf_a->nmax_loaded * sizeof(long *));
    if(istack_smp_arrow == NULL) {
        printf("malloc error for istack_smp_arrow\n");
        exit(0);
    }
    for(i=0; i<psf_a->nmax_loaded; i++){
        int ntot = (psf_m[i]->n_isoline + 1) * nthreads;
        istack_smp_arrow[i] = (long *) calloc(ntot+1, sizeof(long));
        if(istack_smp_arrow[i] == NULL) {
            printf("malloc error for istack_smp_arrow[i] for %d\n", i);
            exit(0);
        }
    };
    
    long num_cone = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_a->iflag_loaded[i]*psf_m[i]->draw_psf_vect != 0){
            num_cone = sel_add_num_psf_arrows_pthread(num_cone, nthreads, istack_smp_arrow[i],
                                                      view_s->ncorner_tube, psf_s[i], psf_m[i]);
        };
    };
    long num_vertex = ITHREE * view_s->ncorner_tube * num_cone;
    set_buffer_address_4_patch(num_vertex, psf_buf);
    if(psf_buf->num_nod_buf <= 0) return;
    
    resize_strided_buffer(psf_buf);
    
    double ref_width = 10.0;
    double radius_arrow;

    long inum_cone = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_m[i]->vector_thick <= 0.0){
            radius_arrow = ref_width * set_tube_radius_by_axis(view_s);
        }else{
            radius_arrow = psf_m[i]->vector_thick;
        };

        if(psf_a->iflag_loaded[i]*psf_m[i]->draw_psf_vect != 0){
            inum_cone = sel_psf_arrows_to_buf_pthread(inum_cone, nthreads, istack_smp_arrow[i],
                                                      view_s->ncorner_tube, radius_arrow,
                                                      psf_s[i], psf_m[i], psf_buf);
        };
    };

    for(i=0; i<psf_a->nmax_loaded; i++){free(istack_smp_arrow[i]);};
    free(istack_smp_arrow);
    return;
}


static void const_PSF_isotube_buffer(const int nthreads,
                                     struct view_element *view_s, struct psf_data **psf_s,
                                     struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                                     struct gl_strided_buffer *psf_buf){
    int i, iflag;
    long **istack_smp_psf_iso = (long **) malloc(psf_a->nmax_loaded * sizeof(long *));
    if(istack_smp_psf_iso == NULL) {
        printf("malloc error for istack_smp_psf_iso\n");
        exit(0);
    }
    for(i=0; i<psf_a->nmax_loaded; i++){
        int ntot = (psf_m[i]->n_isoline + 1) * nthreads;
        istack_smp_psf_iso[i] = (long *) calloc(ntot+1, sizeof(long));
        if(istack_smp_psf_iso[i] == NULL) {
            printf("malloc error for istack_smp_psf_iso[i] for %d\n", i);
            exit(0);
        }
    };


    double ref_width = 1.0;
    double isoline_width = 1.0;;
	long num_patch = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid + psf_m[i]->draw_psf_zero);
        if(iflag != 0){
			num_patch = add_PSF_all_isolines_num(num_patch, nthreads,
                                                 psf_s[i], psf_m[i],
                                                 istack_smp_psf_iso[i]);
		};
	};
    num_patch = ITHREE * (TWO*view_s->ncorner_tube) * num_patch;
    set_buffer_address_4_patch(num_patch, psf_buf);

    if(psf_buf->num_nod_buf <= 0) return;
	resize_strided_buffer(psf_buf);
	
	long inum_patch = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid + psf_m[i]->draw_psf_zero);
        if(iflag != 0){
			if(psf_m[i]->isoline_width <= 0.0){
                isoline_width = ref_width * set_tube_radius_by_axis(view_s);
            }else{
                isoline_width = psf_m[i]->isoline_width;
            };
			inum_patch = set_PSF_all_isotubes_to_buf(inum_patch,
                                                     nthreads, istack_smp_psf_iso[i],
                                                     view_s->ncorner_tube, isoline_width,
                                                     psf_s[i], psf_m[i], psf_buf);
		};
	};
    
    for(i=0; i<psf_a->nmax_loaded; i++){free(istack_smp_psf_iso[i]);};
    free(istack_smp_psf_iso);
	return;
}

static void const_PSF_isoline_buffer(const int nthreads,
                                     struct view_element *view_s, struct psf_data **psf_s,
                                     struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                                     struct gl_strided_buffer *psf_buf){
    int i, iflag;
    long **istack_smp_psf_iso = (long **) malloc(psf_a->nmax_loaded * sizeof(long *));
    if(istack_smp_psf_iso == NULL) {
        printf("malloc error for istack_smp_psf_iso\n");
        exit(0);
    }
    for(i=0; i<psf_a->nmax_loaded; i++){
        int ntot = (psf_m[i]->n_isoline + 1) * nthreads;
        istack_smp_psf_iso[i] = (long *) calloc(ntot+1, sizeof(long));
        if(istack_smp_psf_iso[i] == NULL) {
            printf("malloc error for istack_smp_psf_iso[i] for %d\n", i);
            exit(0);
        }
    };


    double ref_width = 1.5;
    long num_patch = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid + psf_m[i]->draw_psf_zero);
        if(iflag != 0){
            num_patch = add_PSF_all_isolines_num(num_patch, nthreads,
                                                 psf_s[i], psf_m[i],
                                                 istack_smp_psf_iso[i]);
        };
    };
    num_patch = ITWO * num_patch;
    set_buffer_address_4_patch(num_patch, psf_buf);

    if(psf_buf->num_nod_buf <= 0) return;
    resize_strided_buffer(psf_buf);
    
    long inum_patch = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid + psf_m[i]->draw_psf_zero);
        if(iflag != 0){
            inum_patch = set_PSF_all_isolines_to_buf(inum_patch,
                                                     nthreads, istack_smp_psf_iso[i],
                                                     psf_s[i], psf_m[i], psf_buf);
        };
    };
    
    for(i=0; i<psf_a->nmax_loaded; i++){free(istack_smp_psf_iso[i]);};
    free(istack_smp_psf_iso);
    return;
}

int check_draw_psf(struct kemo_array_control *psf_a){
	int i;
    int iflag_psf = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
		iflag_psf = iflag_psf + psf_a->iflag_loaded[i];
	};
	return iflag_psf;
};

void const_PSF_solid_objects_buffer(const int nthreads,
                                    struct view_element *view_s, struct psf_data **psf_s,
                                    struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                                    struct gl_strided_buffer *PSF_solid_buf,
                                    struct gl_strided_buffer *PSF_stxur_buf,
                                    struct gl_index_buffer *PSF_solid_index_buf,
                                    struct gl_index_buffer *PSF_stxur_index_buf){

    
    if(view_s->shading_mode == FLAT_SHADE){
        const_PSF_texture_buffer(view_s->shading_mode, nthreads,
                                 IZERO, psf_a->istack_solid_psf_txtur,
                                 psf_s, psf_m, psf_a, PSF_stxur_buf);
        const_PSF_patch_buffer(nthreads,
                               psf_a->istack_solid_psf_txtur,
                               psf_a->istack_solid_psf_patch,
                               psf_s, psf_a, PSF_solid_buf);
        PSF_stxur_index_buf->ntot_vertex = 0;
        PSF_solid_index_buf->ntot_vertex = 0;
    }else{
        const_PSF_patch_index_buffer(nthreads,
                                     IZERO, psf_a->istack_solid_psf_txtur,
                                     psf_s, psf_a, PSF_stxur_index_buf);
        const_PSF_patch_index_buffer(nthreads,
                                     psf_a->istack_solid_psf_txtur,
                                     psf_a->istack_solid_psf_patch,
                                     psf_s, psf_a,PSF_solid_index_buf);
        PSF_stxur_buf->num_nod_buf = 0;
        PSF_solid_buf->num_nod_buf = 0;
    }
    return;
}

long const_PSF_isolines_buffer(const int nthreads,
                               struct view_element *view_s, struct psf_data **psf_s,
                               struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                               struct gl_strided_buffer *PSF_isoline_buf,
                               struct gl_strided_buffer *PSF_isotube_buf,
                               struct gl_strided_buffer *PSF_arrow_buf){
    const_PSF_isoline_buffer(nthreads, view_s, psf_s, psf_m, psf_a, PSF_isoline_buf);

    const_PSF_isotube_buffer(nthreads, view_s, psf_s, psf_m, psf_a, PSF_isotube_buf);

    const_PSF_arrow_buffer(nthreads, view_s, psf_s, psf_m, psf_a,
                           PSF_arrow_buf);
    return PSF_isoline_buf->num_nod_buf;
}

void const_PSF_trans_objects_buffer(const int nthreads, 
                                    struct view_element *view_s, struct psf_data **psf_s,
                                    struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                                    struct gl_strided_buffer *PSF_trns_buf,
                                    struct gl_strided_buffer *PSF_ttxur_buf,
                                    struct gl_index_buffer *PSF_trns_index_buf,
                                    struct gl_index_buffer *PSF_ttxur_index_buf){

    if(view_s->shading_mode == FLAT_SHADE){
        const_PSF_texture_buffer(view_s->shading_mode, nthreads,
                                 psf_a->istack_solid_psf_patch, psf_a->istack_trans_psf_txtur,
                                 psf_s, psf_m, psf_a, PSF_ttxur_buf);
        const_PSF_patch_buffer(nthreads,
                               psf_a->istack_trans_psf_txtur,
                               psf_a->ntot_psf_patch,
                               psf_s, psf_a, PSF_trns_buf);
        PSF_ttxur_index_buf->ntot_vertex = 0;
        PSF_trns_index_buf->ntot_vertex =  0;
    }else{
        const_PSF_patch_index_buffer(nthreads,
                                     psf_a->istack_solid_psf_patch,
                                     psf_a->istack_trans_psf_txtur,
                                     psf_s, psf_a, PSF_ttxur_index_buf);
        const_PSF_patch_index_buffer(nthreads,
                                     psf_a->istack_trans_psf_txtur,
                                     psf_a->istack_trans_psf_patch,
                                     psf_s, psf_a, PSF_trns_index_buf);
        PSF_ttxur_buf->num_nod_buf = 0;
        PSF_trns_buf->num_nod_buf =  0;
    }
        
	return;
};

