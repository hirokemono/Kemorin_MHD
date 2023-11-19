/*
// draw_messages_gl.c
*/

#include "draw_messages_gl.h"

void const_message_buffer(int iflag_retina, int nx_win, int ny_win,
                          struct msg_work *msg_wk,
                          struct gl_strided_buffer *cbar_buf){
    clear_line_text_image(msg_wk->message_image);
    set_message_position(iflag_retina, (int) nx_win, (int) ny_win, msg_wk);
    set_windowsize_image((int) nx_win, (int) ny_win, msg_wk);
    
    message_mbox_to_buf(iflag_retina, msg_wk, cbar_buf);
    return;
}

void set_message_VAO(struct msg_work *msg_wk, struct VAO_ids *msg_VAO,
                     struct gl_strided_buffer *cbar_buf){    
    const_texture_VBO(msg_wk->message_image->npix_img[0],
                      msg_wk->message_image->npix_img[1],
                      msg_wk->message_image->imgBMP,
                      msg_VAO, cbar_buf);
	return;
};
