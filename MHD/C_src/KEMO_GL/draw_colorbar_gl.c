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

static void set_colorbar_text_VAO(struct cbar_work *cbar_wk, struct VAO_ids **cbar_VAO,
                                  struct gl_strided_buffer *min_buf, struct gl_strided_buffer *max_buf, 
                                  struct gl_strided_buffer *zero_buf){
	glBindVertexArray(cbar_VAO[1]->id_VAO);
	Const_VAO_4_Texture(cbar_VAO[1], min_buf);
	cbar_wk->id_texture[0] = set_texture_to_buffer(cbar_wk->cbar_min_image->npix_img[0],
                                                   cbar_wk->cbar_min_image->npix_img[1],
                                                   cbar_wk->cbar_min_image->imgBMP);
    glBindVertexArray(0);
    
	glBindVertexArray(cbar_VAO[2]->id_VAO);
	Const_VAO_4_Texture(cbar_VAO[2], max_buf);
	cbar_wk->id_texture[1] = set_texture_to_buffer(cbar_wk->cbar_max_image->npix_img[0],
                                                   cbar_wk->cbar_max_image->npix_img[1],
                                                   cbar_wk->cbar_max_image->imgBMP);
    glBindVertexArray(0);
    
	glBindVertexArray(cbar_VAO[3]->id_VAO);
	Const_VAO_4_Texture(cbar_VAO[3], zero_buf);
	cbar_wk->id_texture[2] = set_texture_to_buffer(cbar_wk->cbar_zero_image->npix_img[0],
                                                   cbar_wk->cbar_zero_image->npix_img[1],
                                                   cbar_wk->cbar_zero_image->imgBMP);
	glBindVertexArray(0);
	return;
};

static void set_time_text_VAO(struct tlabel_work *tlabel_wk, struct VAO_ids *text_VAO,
                              struct gl_strided_buffer *cbar_buf){
    glBindVertexArray(text_VAO->id_VAO);
	Const_VAO_4_Texture(text_VAO, cbar_buf);
	tlabel_wk->id_texture = set_texture_to_buffer(tlabel_wk->tlabel_image->npix_img[0],
                                                  tlabel_wk->tlabel_image->npix_img[1],
												  tlabel_wk->tlabel_image->imgBMP);
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
                            struct gl_strided_buffer *min_buf, struct gl_strided_buffer *max_buf, 
                            struct gl_strided_buffer *zero_buf){
    int i;
    set_buffer_address_4_patch((ITHREE*2), min_buf);
    set_buffer_address_4_patch((ITHREE*2), max_buf);
    set_buffer_address_4_patch((ITHREE*2), zero_buf);
    alloc_strided_buffer(min_buf);
    alloc_strided_buffer(max_buf);
    alloc_strided_buffer(zero_buf);

    min_buf->num_nod_buf =  0;
    max_buf->num_nod_buf =  0;
    zero_buf->num_nod_buf = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_a->iflag_loaded[i] != 0 && psf_m[i]->draw_psf_cbar > 0){
            clear_colorbar_text_image(psf_a->cbar_wk);
            set_colorbar_text_image(text_color, psf_a->cbar_wk);

            min_buf->num_nod_buf =  (ITHREE*2);
            max_buf->num_nod_buf =  (ITHREE*2);
            if(psf_a->cbar_wk->iflag_zero == 1) zero_buf->num_nod_buf = (ITHREE*2);
            
            colorbar_mbox_to_buf(iflag_retina, text_color, psf_a->cbar_wk, 
                                 min_buf, max_buf, zero_buf);
            break;
        };
    };
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
            sprintf(psf_a->tlabel_wk->tlabel_image->texts,"    t = %5.4E", (float) psf_a->time_disp);
        }else if(psf_a->iflag_draw_file_step > 0){
            sprintf(psf_a->tlabel_wk->tlabel_image->texts,"File index: %6d", psf_a->file_step_disp);
        };
        set_time_text_image(text_color, psf_a->tlabel_wk);
        time_mbox_to_buf(iflag_retina, text_color, psf_a->tlabel_wk, cbar_buf);
   }else{
        cbar_buf->num_nod_buf = 0;
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
    
    struct gl_strided_buffer *min_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    struct gl_strided_buffer *max_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    struct gl_strided_buffer *zero_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    clear_colorbar_text_image(psf_a->cbar_wk);
    const_cbar_text_buffer(iflag_retina, text_color, psf_m, psf_a, 
                           min_buf, max_buf, zero_buf);
    
    cbar_VAO[1]->npoint_draw = min_buf->num_nod_buf;
    cbar_VAO[2]->npoint_draw = max_buf->num_nod_buf;
    cbar_VAO[3]->npoint_draw = zero_buf->num_nod_buf;
    if(cbar_VAO[1]->npoint_draw > 0){
        set_colorbar_text_VAO(psf_a->cbar_wk, cbar_VAO, min_buf, max_buf, zero_buf);
    };
    free(min_buf->v_buf);
    free(max_buf->v_buf);
    free(zero_buf->v_buf);
    free(min_buf);
    free(max_buf);
    free(zero_buf);
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

void draw_colorbar_VAO(struct cbar_work *cbar_wk, struct VAO_ids **cbar_VAO,
                       struct transfer_matrices *matrices, struct kemoview_shaders *kemo_shaders){
	if(cbar_VAO[0]->npoint_draw <= 0) return;
	
	draw_2D_box_patch_VAO(matrices, cbar_VAO[0], kemo_shaders);
	draw_textured_2D_box_VAO(cbar_wk->id_texture[0], matrices,
							 cbar_VAO[1], kemo_shaders);
    draw_textured_2D_box_VAO(cbar_wk->id_texture[1], matrices,
                             cbar_VAO[2], kemo_shaders);
    draw_textured_2D_box_VAO(cbar_wk->id_texture[2], matrices,
                             cbar_VAO[3], kemo_shaders);
	return;
}

void draw_timelabel_VAO(struct tlabel_work *tlabel_wk, struct VAO_ids *time_VAO, 
                        struct transfer_matrices *matrices, struct kemoview_shaders *kemo_shaders){
	if(time_VAO->npoint_draw <= 0) return;
	draw_textured_2D_box_VAO(tlabel_wk->id_texture, matrices,
							 time_VAO, kemo_shaders);
	return;
}

