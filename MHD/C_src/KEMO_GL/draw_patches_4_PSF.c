
/* draw_patches_4_PSF.c */

#include "draw_patches_4_PSF.h"

#define ARCPI 0.318309886

static void const_PSF_patch_buffer(int shading_mode, const int nthreads, 
                                   long ist_psf, long ied_psf,
                                   struct psf_data **psf_s, struct psf_menu_val **psf_m,
                                   struct kemo_array_control *psf_a,
                                   struct gl_strided_buffer *psf_buf){
	long num_patch = count_psf_nodes_to_buf(ist_psf, ied_psf);
    set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	if(psf_buf->num_nod_buf <= 0) return;
	
	resize_strided_buffer(psf_buf);
    num_patch = sel_psf_nodes_to_buf_pthread(0, nthreads, ist_psf, ied_psf, shading_mode,
                                             psf_s, psf_m, psf_a, psf_buf);
	return;
}

static void const_PSF_texture_buffer(int shading_mode, const int nthreads, 
                                     long ist_psf, long ied_psf,
                                     struct psf_data **psf_s, struct psf_menu_val **psf_m,
                                     struct kemo_array_control *psf_a,
                                     struct gl_strided_buffer *psf_buf){
    const_PSF_patch_buffer(shading_mode, nthreads, ist_psf, ied_psf,
                           psf_s, psf_m, psf_a, psf_buf);
    if(psf_buf->num_nod_buf > 0){
        sel_psf_textures_to_buf_pthread(nthreads, ist_psf, ied_psf,
                                        psf_s, psf_a, psf_buf);
    };
    return;
}

static void const_PSF_arrow_buffer(const int nthreads,
                                   struct psf_data **psf_s, struct psf_menu_val **psf_m,
                                   struct kemo_array_control *psf_a,
                                   struct gl_strided_buffer *psf_buf){
    int ncorner = 20;
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
    
    long num_patch = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_a->iflag_loaded[i]*psf_m[i]->draw_psf_vect != 0){
            num_patch = sel_add_num_psf_arrows_pthread(num_patch,
                                                   nthreads, istack_smp_arrow[i],
                                                   ncorner, psf_s[i], psf_m[i]);
        };
    };
    
    set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
    if(psf_buf->num_nod_buf <= 0) return;
    
    resize_strided_buffer(psf_buf);
    
    long inum_buf = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_a->iflag_loaded[i]*psf_m[i]->draw_psf_vect != 0){
            inum_buf = sel_psf_arrows_to_buf_pthread(inum_buf, nthreads,
                                                     istack_smp_arrow[i], ncorner,
                                                     psf_s[i], psf_m[i], psf_buf);
        };
    };

    for(i=0; i<psf_a->nmax_loaded; i++){free(istack_smp_arrow[i]);};
    free(istack_smp_arrow);
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
    num_patch = 12 * num_patch;
    set_buffer_address_4_patch((ITHREE*num_patch), psf_buf);

    if(psf_buf->num_nod_buf <= 0) return;
	resize_strided_buffer(psf_buf);
	
	long inum_patch = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid + psf_m[i]->draw_psf_zero);
        if(iflag != 0){
			if(psf_m[i]->isoline_width <= 0.0){
				psf_m[i]->isoline_width = set_tube_radius_by_view(view_s, ref_width);
			};
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
                                    struct gl_strided_buffer *PSF_isoline_buf,
                                    struct gl_strided_buffer *PSF_arrow_buf){
    const_PSF_texture_buffer(view_s->shading_mode, nthreads, 
                             IZERO, psf_a->istack_solid_psf_txtur,
                             psf_s, psf_m, psf_a, PSF_stxur_buf);
    const_PSF_patch_buffer(view_s->shading_mode, nthreads, 
                           psf_a->istack_solid_psf_txtur, psf_a->istack_solid_psf_patch,
                           psf_s, psf_m, psf_a, PSF_solid_buf);
    const_PSF_isoline_buffer(nthreads, view_s, psf_s, psf_m, psf_a, PSF_isoline_buf);
    const_PSF_arrow_buffer(nthreads, psf_s, psf_m, psf_a, PSF_arrow_buf);
    return;
}

void const_PSF_trans_objects_buffer(const int nthreads, 
                                    struct view_element *view_s, struct psf_data **psf_s,
                                    struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                                    struct gl_strided_buffer *PSF_trns_buf,
                                    struct gl_strided_buffer *PSF_ttxur_buf){
    const_PSF_texture_buffer(view_s->shading_mode, nthreads, 
                             psf_a->istack_solid_psf_patch, psf_a->istack_trans_psf_txtur,
                             psf_s, psf_m, psf_a, PSF_ttxur_buf);
    const_PSF_patch_buffer(view_s->shading_mode, nthreads, 
                           psf_a->istack_trans_psf_txtur, psf_a->ntot_psf_patch, 
                           psf_s, psf_m, psf_a, PSF_trns_buf);
	return;
};

