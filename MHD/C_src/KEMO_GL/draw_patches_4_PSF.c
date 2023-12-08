
/* draw_patches_4_PSF.c */

#include "draw_patches_4_PSF.h"

#define ARCPI 0.318309886

void const_PSF_patch_buffer(int shading_mode, long ist_psf, long ied_psf,
                            struct psf_data **psf_s, struct psf_menu_val **psf_m,
                            struct kemo_array_control *psf_a,
                            struct gl_strided_buffer *psf_buf){
	int num_vetex = ITHREE * (int) count_psf_nodes_to_buf(ist_psf, ied_psf);
    set_buffer_address_4_patch(num_vetex, psf_buf);
	if(psf_buf->num_nod_buf <= 0) return;
	
	resize_strided_buffer(psf_buf);
	
	set_psf_nodes_to_buf(ist_psf, ied_psf, shading_mode, 
                         psf_s, psf_m, psf_a, psf_buf);
	return;
}

void const_PSF_texture_buffer(int shading_mode, long ist_psf, long ied_psf,
                              struct psf_data **psf_s, struct psf_menu_val **psf_m,
                              struct kemo_array_control *psf_a,
                              struct gl_strided_buffer *psf_buf){
    const_PSF_patch_buffer(shading_mode, ist_psf, ied_psf, psf_s, psf_m, psf_a, psf_buf);
    if(psf_buf->num_nod_buf > 0) set_psf_textures_to_buf(ist_psf, ied_psf, psf_s, psf_a, psf_buf);
    return;
}

void const_PSF_arrow_buffer(struct psf_data **psf_s, struct psf_menu_val **psf_m,
                            struct kemo_array_control *psf_a, struct gl_strided_buffer *psf_buf){
    int ncorner = 20;
    int i;
    int inum_buf;
    
    int num_patch = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_a->iflag_loaded[i]*psf_m[i]->draw_psf_vect != 0){
            num_patch = num_patch + count_psf_arrows_to_buf(ncorner, psf_s[i], psf_m[i]);
        };
    };
    
    set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
    if(psf_buf->num_nod_buf <= 0) return;
    
    resize_strided_buffer(psf_buf);
    inum_buf = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_a->iflag_loaded[i]*psf_m[i]->draw_psf_vect != 0){
            inum_buf = set_psf_arrows_to_buf(inum_buf, ncorner, psf_s[i], psf_m[i], psf_buf);
        };
    };
    return;
}


static void const_PSF_isoline_buffer(struct view_element *view_s, struct psf_data **psf_s,
                                     struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                                     struct gl_strided_buffer *psf_buf){
	double ref_width = 1.5;
	int i, iflag;
	int inum_patch;
	
	int num_patch = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid+psf_m[i]->draw_psf_zero);
        if(iflag != 0){
			num_patch = num_patch + count_PSF_all_isolines_to_buf(psf_s[i], psf_m[i]);
		};
	};
    set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);

    if(psf_buf->num_nod_buf <= 0) return;
	resize_strided_buffer(psf_buf);
	
	inum_patch = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid+psf_m[i]->draw_psf_zero);
        if(iflag != 0){
			if(psf_m[i]->isoline_width <= 0.0){
				psf_m[i]->isoline_width = set_tube_radius_by_view(view_s, ref_width);
			};
			inum_patch = set_PSF_all_isolines_to_buf(inum_patch, psf_s[i], psf_m[i], psf_buf);
		};
	};
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

void const_PSF_solid_objects_buffer(struct view_element *view_s, struct psf_data **psf_s,
                                    struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                                    struct gl_strided_buffer *PSF_solid_buf,
                                    struct gl_strided_buffer *PSF_stxur_buf,
                                    struct gl_strided_buffer *PSF_isoline_buf,
                                    struct gl_strided_buffer *PSF_arrow_buf){
    const_PSF_texture_buffer(view_s->shading_mode,
                             IZERO, psf_a->istack_solid_psf_txtur,
                             psf_s, psf_m, psf_a, PSF_stxur_buf);
    const_PSF_patch_buffer(view_s->shading_mode, 
                           psf_a->istack_solid_psf_txtur, psf_a->istack_solid_psf_patch,
                           psf_s, psf_m, psf_a, PSF_solid_buf);
    const_PSF_isoline_buffer(view_s, psf_s, psf_m, psf_a, PSF_isoline_buf);
    const_PSF_arrow_buffer(psf_s, psf_m, psf_a, PSF_arrow_buf);
    return;
}

void const_PSF_trans_objects_buffer(struct view_element *view_s, struct psf_data **psf_s,
                                    struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                                    struct gl_strided_buffer *PSF_trns_buf,
                                    struct gl_strided_buffer *PSF_ttxur_buf){
    const_PSF_texture_buffer(view_s->shading_mode,
                             psf_a->istack_solid_psf_patch, psf_a->istack_trans_psf_txtur,
                             psf_s, psf_m, psf_a, PSF_ttxur_buf);
    const_PSF_patch_buffer(view_s->shading_mode,
                           psf_a->istack_trans_psf_txtur, psf_a->ntot_psf_patch, 
                           psf_s, psf_m, psf_a, PSF_trns_buf);
	return;
};

