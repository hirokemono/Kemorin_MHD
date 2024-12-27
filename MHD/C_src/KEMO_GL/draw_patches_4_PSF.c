
/* draw_patches_4_PSF.c */

#include "draw_patches_4_PSF.h"

#define ARCPI 0.318309886

void const_PSF_node_stack(struct psf_data **psf_s,
                          struct kemo_array_control *psf_a){
    psf_a->istack_all_psf_node[0] = 0;
    int i_psf;
    for(i_psf=0;i_psf<psf_a->nmax_loaded;i_psf++){
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
                           struct psf_normals **psf_n,
                           struct kemo_array_control *psf_a,
                           struct gl_strided_buffer *psf_buf){
    set_buffer_address_4_patch(psf_a->istack_all_psf_node[psf_a->nmax_loaded], psf_buf);
	if(psf_buf->num_nod_buf <= 0) return;
	
	resize_strided_buffer(psf_buf);
    
    long num_patch = 0;
    int i_psf;
    for(i_psf=0;i_psf<psf_a->nmax_loaded;i_psf++){
        if(psf_a->iflag_loaded[i_psf] == 0) continue;
        num_patch = sel_psf_nodes_to_buf_pthread(psf_a->istack_all_psf_node[i_psf], nthreads,
                                                 IZERO, psf_s[i_psf]->nnod_viz,
                                                 i_psf, psf_s, psf_n, psf_buf);
    }
	return;
}

void const_PSF_patch_index_buffer(const int nthreads, long ist_psf, long ied_psf,
                                  struct psf_data **psf_s,
                                  struct kemo_array_control *psf_a,
                                  struct gl_index_buffer *index_buf){
    long num_patch = count_psf_nodes_to_buf(ist_psf, ied_psf);
    resize_gl_index_buffer(num_patch, ITHREE, index_buf);
    if(index_buf->ntot_vertex <= 0) return;
    
    num_patch = set_psf_patch_indices_to_buf(0, ist_psf, ied_psf, psf_s, psf_a, index_buf);
    return;
}

void const_PSF_patch_buffer(const int nthreads, long ist_psf, long ied_psf,
                            struct psf_data **psf_s,
                            struct psf_normals **psf_n,
                            struct kemo_array_control *psf_a,
                            struct gl_strided_buffer *psf_buf){
    long num_patch = count_psf_nodes_to_buf(ist_psf, ied_psf);
    set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
    if(psf_buf->num_nod_buf <= 0) return;
    
    resize_strided_buffer(psf_buf);
    num_patch = sel_psf_patches_to_buf_pthread(0, nthreads, ist_psf, ied_psf,
                                               psf_s, psf_n, psf_a, psf_buf);
    return;
}

void const_PSF_texture_buffer(int shading_mode, const int nthreads,
                              long ist_psf, long ied_psf,
                              struct psf_data **psf_s,
                              struct psf_normals **psf_n,
                              struct psf_menu_val **psf_m,
                              struct kemo_array_control *psf_a,
                              struct gl_strided_buffer *psf_buf){
    const_PSF_patch_buffer(nthreads, ist_psf, ied_psf,
                           psf_s, psf_n, psf_a, psf_buf);
    if(psf_buf->num_nod_buf > 0){
        sel_psf_textures_to_buf_pthread(nthreads, ist_psf, ied_psf,
                                        psf_s, psf_a, psf_buf);
    };
    return;
}

void const_PSF_arrow_buffer(const int nthreads, struct view_element *view_s,
                            struct psf_data **psf_s,
                            struct psf_normals **psf_n,
                            struct psf_menu_val **psf_m,
                            struct kemo_array_control *psf_a,
                            struct gl_strided_buffer *psf_buf,
                            struct gl_index_buffer *psf_index_buf){
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
                                                      view_s->ncorner_tube,
                                                      psf_s[i], psf_n[i], psf_m[i]);
        };
    };
    long num_vertex = (2 + view_s->ncorner_tube) * num_cone;
    set_buffer_address_4_patch(num_vertex, psf_buf);
    psf_index_buf->ntot_vertex = (2*psf_m[i]->ncorner_viz_line) * num_cone;
    
    if(psf_buf->num_nod_buf <= 0) return;
    
    resize_strided_buffer(psf_buf);
    
    resize_gl_index_buffer(((2*psf_m[i]->ncorner_viz_line) * num_cone),
                           ITHREE,
                           psf_index_buf);

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
                                                      psf_s[i], psf_n[i], psf_m[i],
                                                      psf_buf, psf_index_buf);
        };
    };

    for(i=0; i<psf_a->nmax_loaded; i++){free(istack_smp_arrow[i]);};
    free(istack_smp_arrow);
    return;
}


void const_PSF_isotube_buffer(const int nthreads,
                              struct view_element *view_s,
                              struct psf_data **psf_s,
                              struct psf_normals **psf_n,
                              struct psf_menu_val **psf_m,
                              struct kemo_array_control *psf_a,
                              struct gl_strided_buffer *psf_buf,
                              struct gl_index_buffer *index_buf){
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
    double viz_line_width = 1.0;;
	long num_tube = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid + psf_m[i]->draw_psf_zero);
        if(iflag != 0){
            num_tube = add_PSF_all_isolines_num(num_tube, nthreads,
                                                psf_s[i], psf_m[i],
                                                istack_smp_psf_iso[i]);
		};
	};
    long num_node =  ITWO *  (IONE + view_s->ncorner_tube) * num_tube;
    long num_patch = IFOUR * (view_s->ncorner_tube) * num_tube;
    set_buffer_address_4_patch(num_node, psf_buf);
    index_buf->ntot_vertex = num_patch;

    if(psf_buf->num_nod_buf <= 0) return;
	resize_strided_buffer(psf_buf);
    resize_gl_index_buffer(num_patch, ITHREE, index_buf);

	long inum_patch = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid + psf_m[i]->draw_psf_zero);
        if(iflag != 0){
			if(psf_m[i]->viz_line_width <= 0.0){
                viz_line_width = ref_width * set_tube_radius_by_axis(view_s);
            }else{
                viz_line_width = psf_m[i]->viz_line_width;
            };
			inum_patch = set_PSF_all_isotubes_to_buf(inum_patch,
                                                     nthreads, istack_smp_psf_iso[i],
                                                     view_s->ncorner_tube, viz_line_width,
                                                     psf_s[i], psf_n[i], psf_m[i],
                                                     psf_buf, index_buf);
		};
	};
    
    for(i=0; i<psf_a->nmax_loaded; i++){free(istack_smp_psf_iso[i]);};
    free(istack_smp_psf_iso);
	return;
}

void const_PSF_isoline_buffer(const int nthreads, struct view_element *view_s,
                              struct psf_data **psf_s, struct psf_normals **psf_n,
                              struct psf_menu_val **psf_m,
                              struct kemo_array_control *psf_a,
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
                                                     psf_s[i], psf_n[i], psf_m[i],
                                                     psf_buf);
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
