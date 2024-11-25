/*
//  m_kemoview_PSF_buffers.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include "m_kemoview_PSF_buffers.h"

struct PSF_solid_buffers * init_PSF_solid_buffers(void)
{
    struct PSF_solid_buffers *PSF_solids = (struct PSF_solid_buffers *) malloc(sizeof(struct PSF_solid_buffers));
    if(PSF_solids == NULL){
        printf("malloc error in PSF_solid_buffers \n");
        exit(0);
    };
    
    long n_point = 1024;
    PSF_solids->PSF_solid_index_buf = init_gl_index_buffer(12, 3);
    PSF_solids->PSF_stxur_index_buf = init_gl_index_buffer(12, 3);

    PSF_solids->PSF_solid_buf =       init_strided_buffer(n_point);
    PSF_solids->PSF_stxur_buf =       init_strided_buffer(n_point);
    return PSF_solids;
}

struct PSF_trans_buffers * init_PSF_trans_buffers(void)
{
    struct PSF_trans_buffers *PSF_transes = (struct PSF_trans_buffers *) malloc(sizeof(struct PSF_trans_buffers));
    if(PSF_transes == NULL){
        printf("malloc error in PSF_trans_buffers \n");
        exit(0);
    };
    
    long n_point = 1024;
    PSF_transes->PSF_trns_index_buf =  init_gl_index_buffer(12, 3);
    PSF_transes->PSF_ttxur_index_buf = init_gl_index_buffer(12, 3);
    
    PSF_transes->PSF_trns_buf =    init_strided_buffer(n_point);
    PSF_transes->PSF_ttxur_buf =   init_strided_buffer(n_point);

    return PSF_transes;
}


void dealloc_PSF_solid_buffers(struct PSF_solid_buffers *PSF_solids)
{
    dealloc_strided_buffer(PSF_solids->PSF_stxur_buf);
    dealloc_strided_buffer(PSF_solids->PSF_solid_buf);

    dealloc_gl_index_buffer(PSF_solids->PSF_solid_index_buf);
    dealloc_gl_index_buffer(PSF_solids->PSF_stxur_index_buf);
    free(PSF_solids);
};

void dealloc_PSF_trans_buffers(struct PSF_trans_buffers *PSF_transes)
{
    dealloc_gl_index_buffer(PSF_transes->PSF_trns_index_buf);
    dealloc_gl_index_buffer(PSF_transes->PSF_ttxur_index_buf);
    
    dealloc_strided_buffer(PSF_transes->PSF_trns_buf);
    dealloc_strided_buffer(PSF_transes->PSF_ttxur_buf);
    free(PSF_transes);
};

void const_PSF_solid_objects_buffer(const int nthreads,
                                    struct view_element *view_s,
                                    struct psf_data **psf_s,
                                    struct psf_normals **psf_n,
                                    struct psf_menu_val **psf_m,
                                    struct kemo_array_control *psf_a,
                                    struct PSF_solid_buffers *PSF_solids){
    if(view_s->shading_mode == FLAT_SHADE){
        const_PSF_texture_buffer(view_s->shading_mode, nthreads,
                                 IZERO, psf_a->istack_solid_psf_txtur,
                                 psf_s, psf_n, psf_m, psf_a,
                                 PSF_solids->PSF_stxur_buf);
        const_PSF_patch_buffer(nthreads,
                               psf_a->istack_solid_psf_txtur,
                               psf_a->istack_solid_psf_patch,
                               psf_s, psf_n, psf_a,
                               PSF_solids->PSF_solid_buf);
        PSF_solids->PSF_stxur_index_buf->ntot_vertex = 0;
        PSF_solids->PSF_solid_index_buf->ntot_vertex = 0;
    }else{
        const_PSF_patch_index_buffer(nthreads,
                                     IZERO, psf_a->istack_solid_psf_txtur,
                                     psf_s, psf_a, 
                                     PSF_solids->PSF_stxur_index_buf);
        const_PSF_patch_index_buffer(nthreads,
                                     psf_a->istack_solid_psf_txtur,
                                     psf_a->istack_solid_psf_patch,
                                     psf_s, psf_a,
                                     PSF_solids->PSF_solid_index_buf);
        PSF_solids->PSF_stxur_buf->num_nod_buf = 0;
        PSF_solids->PSF_solid_buf->num_nod_buf = 0;
    }
    return;
}

void const_PSF_trans_objects_buffer(const int nthreads, 
                                    struct view_element *view_s,
                                    struct psf_data **psf_s,
                                    struct psf_normals **psf_n,
                                    struct psf_menu_val **psf_m,
                                    struct kemo_array_control *psf_a,
                                    struct PSF_trans_buffers *PSF_transes){

    if(view_s->shading_mode == FLAT_SHADE){
        const_PSF_texture_buffer(view_s->shading_mode, nthreads,
                                 psf_a->istack_solid_psf_patch,
                                 psf_a->istack_trans_psf_txtur,
                                 psf_s, psf_n, psf_m, psf_a, 
                                 PSF_transes->PSF_ttxur_buf);
        const_PSF_patch_buffer(nthreads,
                               psf_a->istack_trans_psf_txtur,
                               psf_a->ntot_psf_patch,
                               psf_s, psf_n, psf_a,
                               PSF_transes->PSF_trns_buf);
        PSF_transes->PSF_ttxur_index_buf->ntot_vertex = 0;
        PSF_transes->PSF_trns_index_buf->ntot_vertex =  0;
    }else{
        const_PSF_patch_index_buffer(nthreads,
                                     psf_a->istack_solid_psf_patch,
                                     psf_a->istack_trans_psf_txtur,
                                     psf_s, psf_a, 
                                     PSF_transes->PSF_ttxur_index_buf);
        const_PSF_patch_index_buffer(nthreads,
                                     psf_a->istack_trans_psf_txtur,
                                     psf_a->istack_trans_psf_patch,
                                     psf_s, psf_a, 
                                     PSF_transes->PSF_trns_index_buf);
        PSF_transes->PSF_ttxur_buf->num_nod_buf = 0;
        PSF_transes->PSF_trns_buf->num_nod_buf =  0;
    }
        
	return;
};

