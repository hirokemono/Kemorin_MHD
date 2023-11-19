/*
// draw_colorbar_gl.c
*/

#include "draw_colorbar_gl.h"

int count_colorbar_box_VAO(int iflag_zero, int num_quad){
	int num_patch = 4*num_quad + 2*(iflag_zero + IFOUR);
	return (ITHREE * num_patch);
};

static int count_colorbar_text_VAO(struct cbar_work *cbar_wk){
    int num_patch = ITHREE*2*(cbar_wk->iflag_zero + ITWO);
	return num_patch;
};

static void set_colorbar_text_VAO(struct cbar_work *cbar_wk, struct VAO_ids **cbar_VAO,
                                  struct gl_strided_buffer *min_buf, struct gl_strided_buffer *max_buf, 
                                  struct gl_strided_buffer *zero_buf){
    const_texture_VBO(cbar_wk->cbar_min_image->npix_img[0],
                      cbar_wk->cbar_min_image->npix_img[1],
                      cbar_wk->cbar_min_image->imgBMP,
                      cbar_VAO[1], min_buf);
    const_texture_VBO(cbar_wk->cbar_max_image->npix_img[0],
                      cbar_wk->cbar_max_image->npix_img[1],
                      cbar_wk->cbar_max_image->imgBMP,
                      cbar_VAO[2], max_buf);
    const_texture_VBO(cbar_wk->cbar_zero_image->npix_img[0],
                      cbar_wk->cbar_zero_image->npix_img[1],
                      cbar_wk->cbar_zero_image->imgBMP,
                      cbar_VAO[3], zero_buf);
	return;
};

static void set_time_text_VAO(struct tlabel_work *tlabel_wk, struct VAO_ids *text_VAO,
                              struct gl_strided_buffer *time_buf){
	const_texture_VBO(tlabel_wk->tlabel_image->npix_img[0],
                      tlabel_wk->tlabel_image->npix_img[1],
                      tlabel_wk->tlabel_image->imgBMP,
                      text_VAO, time_buf);
    return;
};


void const_colorbar_buffer(int iflag_retina, int nx_win, int ny_win,
                           float text_color[4], float bg_color[4],
                           struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                           struct gl_strided_buffer *cbar_buf){
    int i;
    int icomp;
    cbar_buf->num_nod_buf = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_a->iflag_loaded[i] != 0 && psf_m[i]->draw_psf_cbar > 0) {
            icomp = psf_m[i]->icomp_draw_psf;
            set_colorbar_position(iflag_retina, (int) nx_win, (int) ny_win,
                        psf_m[i]->cmap_psf_comp[icomp], psf_a->cbar_wk);
    
            cbar_buf->num_nod_buf = count_colorbar_box_VAO(psf_a->cbar_wk->iflag_zero,
                                                           psf_a->cbar_wk->num_quad);
            
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
    min_buf->num_nod_buf =  0;
    max_buf->num_nod_buf =  0;
    zero_buf->num_nod_buf = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_a->iflag_loaded[i] != 0 && psf_m[i]->draw_psf_cbar > 0){
            set_colorbar_text_image(text_color, psf_a->cbar_wk->psf_min, psf_a->cbar_wk->cbar_min_image);
            set_colorbar_text_image(text_color, psf_a->cbar_wk->psf_max, psf_a->cbar_wk->cbar_max_image);
            if(psf_a->cbar_wk->iflag_zero == 1){
                set_colorbar_text_image(text_color, ZERO, psf_a->cbar_wk->cbar_zero_image);
            }

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
                            struct gl_strided_buffer *time_buf){
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
        time_mbox_to_buf(iflag_retina, text_color, psf_a->tlabel_wk, time_buf);
        time_buf->num_nod_buf = TWO * THREE;
   }else{
        time_buf->num_nod_buf = 0;
    };
    return;
};

void set_colorbar_VAO(int iflag_retina, int nx_win, int ny_win,
                      GLfloat text_color[4], GLfloat bg_color[4],
                      struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                      struct gl_strided_buffer *cbar_buf, struct gl_strided_buffer *min_buf,
                      struct gl_strided_buffer *max_buf, struct gl_strided_buffer *zero_buf,
                      struct VAO_ids **cbar_VAO){
    const_colorbar_buffer(iflag_retina, nx_win, ny_win, text_color, bg_color,
                          psf_m, psf_a, cbar_buf);
    Const_VAO_4_Simple(cbar_VAO[0], cbar_buf);
    
    const_cbar_text_buffer(iflag_retina, text_color, psf_m, psf_a,
                           min_buf, max_buf, zero_buf);
    
    cbar_VAO[1]->npoint_draw = min_buf->num_nod_buf;
    cbar_VAO[2]->npoint_draw = max_buf->num_nod_buf;
    cbar_VAO[3]->npoint_draw = zero_buf->num_nod_buf;
    if(cbar_VAO[1]->npoint_draw > 0){
        set_colorbar_text_VAO(psf_a->cbar_wk, cbar_VAO, min_buf, max_buf, zero_buf);
    };
	return;
};

void set_timelabel_VAO(int iflag_retina, int nx_win, int ny_win,
                       float text_color[4], float bg_color[4],
                       struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                       struct gl_strided_buffer *time_buf, struct VAO_ids *time_VAO){
    const_timelabel_buffer(iflag_retina, nx_win, ny_win, text_color, bg_color,
                           psf_a, time_buf);
    
    time_VAO->npoint_draw = time_buf->num_nod_buf;
    if(time_VAO->npoint_draw > 0){
        set_time_text_VAO(psf_a->tlabel_wk, time_VAO, time_buf);
	};
	return;
};
