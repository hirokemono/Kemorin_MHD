
/* draw_map_4_PSF.c */

#include "draw_map_4_PSF.h"

int check_draw_map(struct kemo_array_control *psf_a){
    int i;
    int iflag_map = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        iflag_map = iflag_map + psf_a->iflag_loaded[i];
    };
    return iflag_map;
};



void set_map_node_buffer(const int nthreads, struct psf_data **psf_s,
                         struct kemo_array_control *psf_a,
                         struct gl_strided_buffer *map_buf){
    set_buffer_address_4_patch(psf_a->istack_all_psf_node[psf_a->nmax_loaded], map_buf);
    if(map_buf->num_nod_buf <= 0) return;
    
    resize_strided_buffer(map_buf);
    
    long num_patch = 0;
    for(int i_psf=0;i_psf<psf_a->nmax_loaded;i_psf++){
        if(psf_a->iflag_loaded[i_psf] == 0) continue;
        num_patch = sel_map_nodes_to_buf_pthread(num_patch, nthreads,
                                                 0, psf_s[i_psf]->nnod_viz,
                                                 i_psf, psf_s, map_buf);
    }
    return;
}

void set_map_patch_buffer(const int nthreads, long ist_psf, long ied_psf,
                          struct psf_data **psf_s, struct psf_menu_val **psf_m,
                          struct kemo_array_control *psf_a,
                          struct gl_strided_buffer *map_buf){
    long num_patch =  count_psf_nodes_to_buf(ist_psf, ied_psf);
    set_buffer_address_4_patch((ITHREE * num_patch), map_buf);
    if(map_buf->num_nod_buf > 0){
        resize_strided_buffer(map_buf);
        long num_patch = sel_map_patch_to_buf_pthread(0, nthreads, ist_psf, ied_psf,
                                                      psf_s, psf_a, map_buf);
    }
	return;
}

void set_map_PSF_isolines_buffer(const int nthreads,
                                 struct view_element *view_s,
                                 struct psf_data **psf_s,
                                 struct psf_normals **psf_n,
                                 struct psf_menu_val **psf_m,
                                 struct kemo_array_control *psf_a,
                                 struct gl_strided_buffer *mline_buf){
    int i, iflag;
    
    long **istack_smp_map_iso = (long **) malloc(psf_a->nmax_loaded * sizeof(long *));
    if(istack_smp_map_iso == NULL) {
        printf("malloc error for istack_smp_map_iso\n");
        exit(0);
    }
    for(i=0; i<psf_a->nmax_loaded; i++){
        int ntot = (psf_m[i]->n_isoline + 1) * nthreads;
        istack_smp_map_iso[i] = (long *) calloc(ntot+1, sizeof(long));
        if(istack_smp_map_iso[i] == NULL) {
            printf("malloc error for istack_smp_map_iso[i] for %d\n", i);
            exit(0);
        }
    };

	long num_patch = 0;
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid + psf_m[i]->draw_psf_zero);
		if(iflag > 0){
			num_patch = add_map_PSF_isoline(num_patch, nthreads,
                                            psf_s[i], psf_m[i],
                                            istack_smp_map_iso[i]);
		};
	};
    num_patch = ITHREE * (ITWO * view_s->ncorner_tube) * num_patch;
    set_buffer_address_4_patch(num_patch, mline_buf);

    if(mline_buf->num_nod_buf <= 0) return;
    resize_strided_buffer(mline_buf);
    
    long inum_patch = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid + psf_m[i]->draw_psf_zero);
        if(iflag > 0){
            inum_patch = set_map_PSF_isoline_to_buf(inum_patch,
                                                    nthreads, istack_smp_map_iso[i],
                                                    view_s->ncorner_tube, view_s->width_tube,
                                                    psf_s[i], psf_n[i], psf_m[i], mline_buf);
        };
    };
    
    for(i=0; i<psf_a->nmax_loaded; i++){free(istack_smp_map_iso[i]);};
    free(istack_smp_map_iso);
    return;
}
