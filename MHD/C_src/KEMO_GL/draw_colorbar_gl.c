/*
// draw_colorbar_gl.c
*/

#include "draw_colorbar_gl.h"

static int count_colorbar_box_VAO(struct cbar_work *cbar_wk){
	int num_patch = 4*cbar_wk->num_quad + 2*(cbar_wk->iflag_zero + IFOUR);
	return (ITHREE * num_patch);
};

static int count_colorbar_text_VAO(struct cbar_work *cbar_wk){
    int num_patch = ITHREE*2*(cbar_wk->iflag_zero + ITWO);
	return num_patch;
};

static void set_colorbar_text_VAO(struct cbar_work *cbar_wk, struct VAO_ids *text_VAO,
								  struct gl_strided_buffer *cbar_buf){
	glBindVertexArray(text_VAO->id_VAO);
	Const_VAO_4_Texture(text_VAO, cbar_buf);
	cbar_wk->id_texture = set_texture_to_buffer(cbar_wk->npix_x, 3*cbar_wk->npix_y,
                                                cbar_wk->numBMP);
	glBindVertexArray(0);
	return;
};

static void set_time_text_VAO(struct tlabel_work *tlabel_wk, struct VAO_ids *text_VAO,
                              struct gl_strided_buffer *cbar_buf){
    glBindVertexArray(text_VAO->id_VAO);
	Const_VAO_4_Texture(text_VAO, cbar_buf);
	tlabel_wk->id_texture = set_texture_to_buffer(tlabel_wk->npix_x, tlabel_wk->npix_y,
												  tlabel_wk->numBMP);
	glBindVertexArray(0);
    return;
};


void const_colorbar_buffer(int iflag_retina, int nx_win, int ny_win,
                           float text_color[4], float bg_color[4],
                           struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                           struct gl_strided_buffer *cbar_buf){
    int i;
    int icomp;
    set_buffer_address_4_patch(16, cbar_buf);
    alloc_strided_buffer(cbar_buf);

    cbar_buf->num_nod_buf = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_a->iflag_loaded[i] != 0 && psf_m[i]->draw_psf_cbar > 0) {
            icomp = psf_m[i]->icomp_draw_psf;
            set_colorbar_position(iflag_retina, (int) nx_win, (int) ny_win,
                        psf_m[i]->cmap_psf_comp[icomp], psf_a->cbar_wk);
    
            set_buffer_address_4_patch(count_colorbar_box_VAO(psf_a->cbar_wk), cbar_buf);
            resize_strided_buffer(cbar_buf);
            
            int inum_quad = 0;
            inum_quad = solid_colorbar_box_to_buf(inum_quad, psf_m[i]->cmap_psf_comp[icomp],
                                                  psf_a->cbar_wk, cbar_buf);
            inum_quad = fade_colorbar_box_to_buf(inum_quad, psf_m[i]->cmap_psf_comp[icomp],
                                                 bg_color, psf_a->cbar_wk, cbar_buf);
            inum_quad = colorbar_frame_to_buf(inum_quad, iflag_retina, text_color,
                                              psf_a->cbar_wk, cbar_buf);
            break;
        };
    };
    return;
};

void const_cbar_text_buffer(int iflag_retina,  float text_color[4],
                            struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                            struct gl_strided_buffer *cbar_buf){
    int i;
    set_buffer_address_4_patch(16, cbar_buf);
    alloc_strided_buffer(cbar_buf);

    clear_colorbar_text_image(psf_a->cbar_wk);
    cbar_buf->num_nod_buf = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_a->iflag_loaded[i] != 0 && psf_m[i]->draw_psf_cbar > 0){
            set_colorbar_text_image(text_color, psf_a->cbar_wk);
            set_buffer_address_4_patch(count_colorbar_text_VAO(psf_a->cbar_wk), cbar_buf);
            resize_strided_buffer(cbar_buf);
            colorbar_mbox_to_buf(iflag_retina, text_color, psf_a->cbar_wk, cbar_buf);
            break;
        };
    };
    return;
};

void set_colorbar_VAO(int iflag_retina, int nx_win, int ny_win,
                      GLfloat text_color[4], GLfloat bg_color[4],
                      struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                      struct VAO_ids **cbar_VAO){
	struct gl_strided_buffer *cbar_buf
		= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    const_colorbar_buffer(iflag_retina, nx_win, ny_win, text_color, bg_color,
                          psf_m, psf_a, cbar_buf);
    if(cbar_buf->num_nod_buf > 0){
        cbar_VAO[0]->npoint_draw = cbar_buf->num_nod_buf;
        Const_VAO_4_Simple(cbar_VAO[0], cbar_buf);
    }else{
        cbar_VAO[0]->npoint_draw = 0;
    };
    free(cbar_buf->v_buf);
    free(cbar_buf);
    
    struct gl_strided_buffer *ctxt_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    clear_colorbar_text_image(psf_a->cbar_wk);
    const_cbar_text_buffer(iflag_retina, text_color, psf_m, psf_a,cbar_buf);
    
    cbar_VAO[1]->npoint_draw = ctxt_buf->num_nod_buf;
    if(cbar_VAO[1]->npoint_draw > 0){
        set_colorbar_text_VAO(psf_a->cbar_wk, cbar_VAO[1], ctxt_buf);
    };
    free(ctxt_buf->v_buf);
    free(ctxt_buf);
	return;
};

void const_timelabel_buffer(int iflag_retina, int nx_win, int ny_win,
                            float text_color[4], float bg_color[4],
                            struct kemo_array_control *psf_a,
                            struct gl_strided_buffer *cbar_buf){
    set_buffer_address_4_patch((ITHREE*2), cbar_buf);
    alloc_strided_buffer(cbar_buf);
    if((psf_a->iflag_draw_time + psf_a->iflag_draw_file_step) > 0){
        psf_a->tlabel_wk->xwin = (float) nx_win;
        psf_a->tlabel_wk->ywin = (float) ny_win;
        
        clear_time_text_image(psf_a->tlabel_wk);
        if(psf_a->iflag_draw_time > 0){
            sprintf(psf_a->tlabel_wk->minlabel,"    t = %5.4E", (float) psf_a->time_disp);
        }else if(psf_a->iflag_draw_file_step > 0){
            sprintf(psf_a->tlabel_wk->minlabel,"File index: %6d", psf_a->file_step_disp);
        };
        set_time_text_image(text_color, psf_a->tlabel_wk);
        time_mbox_to_buf(iflag_retina, text_color, psf_a->tlabel_wk, cbar_buf);
   }else{
        cbar_buf->num_nod_buf = 0;
    };
    return;
};

void set_timelabel_VAO(int iflag_retina, int nx_win, int ny_win,
                       float text_color[4], float bg_color[4],
                       struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                       struct VAO_ids *time_VAO){
	struct gl_strided_buffer *cbar_buf 
		= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    const_timelabel_buffer(iflag_retina, nx_win, ny_win, text_color, bg_color,
                           psf_a, cbar_buf);
    
    time_VAO->npoint_draw = cbar_buf->num_nod_buf;
    if(time_VAO->npoint_draw > 0){
        set_time_text_VAO(psf_a->tlabel_wk, time_VAO, cbar_buf);
	};
	free(cbar_buf->v_buf);
	free(cbar_buf);
	return;
};

void draw_colorbar_VAO(struct cbar_work *cbar_wk,
			struct VAO_ids **cbar_VAO, struct kemoview_shaders *kemo_shaders){
	if(cbar_VAO[1]->npoint_draw <= 0) return;
	
    double *orthogonal = orthogonal_projection_mat_c(0.0, cbar_wk->xwin,
                                                     0.0, cbar_wk->ywin,
                                                     -1.0, 1.0);
    struct transfer_matrices *matrices = plane_transfer_matrices(orthogonal);
	draw_2D_box_patch_VAO(matrices, cbar_VAO[0], kemo_shaders);
	draw_textured_2D_box_VAO(cbar_wk->id_texture, matrices,
							 cbar_VAO[1], kemo_shaders);
    free(matrices);
    free(orthogonal);
	return;
}

void draw_timelabel_VAO(struct tlabel_work *tlabel_wk,
			struct VAO_ids *time_VAO, struct kemoview_shaders *kemo_shaders){
	if(time_VAO->npoint_draw <= 0) return;
    double *orthogonal = orthogonal_projection_mat_c(0.0, tlabel_wk->xwin,
                                                     0.0, tlabel_wk->ywin,
                                                     -1.0, 1.0);
    struct transfer_matrices *matrices = plane_transfer_matrices(orthogonal);
	draw_textured_2D_box_VAO(tlabel_wk->id_texture, matrices,
							 time_VAO, kemo_shaders);
    free(matrices);
    free(orthogonal);
	return;
}

