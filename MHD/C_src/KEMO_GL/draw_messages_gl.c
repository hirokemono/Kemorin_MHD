/*
// draw_messages_gl.c
*/

#include "draw_messages_gl.h"

static void set_message_text_VAO(struct VAO_ids *text_VAO, struct msg_work *msg_wk,
                                 const struct gl_strided_buffer *cbar_buf){
	
	glBindVertexArray(text_VAO->id_VAO);
	Const_VAO_4_Texture(text_VAO, cbar_buf);
    msg_wk->id_texture = set_texture_to_buffer(msg_wk->message_image->npix_img[0], msg_wk->message_image->npix_img[1],
                                               msg_wk->message_image->imgBMP);
	glBindVertexArray(0);
	return;
};

void const_message_buffer(int iflag_retina, int nx_win, int ny_win,
                          struct msg_work *msg_wk,
                          struct gl_strided_buffer *cbar_buf){
    clear_message_text_image(msg_wk);
    set_message_position(iflag_retina, (int) nx_win, (int) ny_win, msg_wk);
    set_windowsize_image((int) nx_win, (int) ny_win, msg_wk);
    
    set_buffer_address_4_patch((ITHREE*2), cbar_buf);
    alloc_strided_buffer(cbar_buf);
    message_mbox_to_buf(iflag_retina, msg_wk, cbar_buf);
    return;
}

void set_message_VAO(int iflag_retina, int nx_win, int ny_win,
					 struct msg_work *msg_wk, struct VAO_ids *msg_VAO,
                     struct gl_strided_buffer *cbar_buf){
    const_message_buffer(iflag_retina, nx_win, ny_win, msg_wk, cbar_buf);
    
    msg_VAO->npoint_draw = cbar_buf->num_nod_buf;
    if(msg_VAO->npoint_draw > 0){
        set_message_text_VAO(msg_VAO, msg_wk, cbar_buf);
    }
	return;
};

void draw_message_VAO(struct msg_work *msg_wk, struct VAO_ids *msg_VAO, 
                      struct transfer_matrices *matrices, 
                      struct kemoview_shaders *kemo_shaders){
	if(msg_VAO->npoint_draw <= 0) return;
	
	draw_textured_2D_box_VAO(msg_wk->id_texture, matrices, msg_VAO, kemo_shaders);
	return;
}

