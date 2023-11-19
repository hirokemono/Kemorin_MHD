/*
// draw_messages_gl.c
*/

#include "draw_messages_gl.h"

void const_message_buffer(int iflag_retina, int nx_win, int ny_win,
                          struct msg_work *msg_wk,
                          struct gl_strided_buffer *cbar_buf){
    if(msg_wk->message_opacity > 0.0){
        clear_line_text_image(msg_wk->message_image);
        set_message_position(iflag_retina, (int) nx_win, (int) ny_win, msg_wk);
        set_windowsize_image((int) nx_win, (int) ny_win, msg_wk);
        
        cbar_buf->num_nod_buf = ITWO * ITHREE;
        message_mbox_to_buf(iflag_retina, msg_wk, cbar_buf);
    }else{
        cbar_buf->num_nod_buf = 0;
    }
    return;
}

void set_message_VAO(struct msg_work *msg_wk, struct gl_strided_buffer *cbar_buf,
                     struct VAO_ids *msg_VAO){    
    const_texture_VBO(msg_wk->message_image->npix_img[0],
                      msg_wk->message_image->npix_img[1],
                      msg_wk->message_image->imgBMP,
                      msg_VAO, cbar_buf);
	return;
};
