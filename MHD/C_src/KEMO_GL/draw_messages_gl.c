/*
// draw_messages_gl.c
*/

#include "draw_messages_gl.h"

void const_message_buffer(int iflag_retina, int nx_win, int ny_win,
                          struct gl_strided_buffer *cbar_buf,
                          struct line_text_image *message_image){
    float xbar_max, ybar_min;
    if(message_image->text_opacity > 0.0){
        clear_line_text_image(message_image);
        xbar_max = message_xmax(nx_win);
        ybar_min = message_ymin(ny_win);
        set_windowsize_image(nx_win, ny_win, message_image);
        
        cbar_buf->num_nod_buf = ITWO * ITHREE;
        message_mbox_to_buf(iflag_retina, message_image->text_opacity,
                            xbar_max, ybar_min, cbar_buf);
    }else{
        cbar_buf->num_nod_buf = 0;
    }
    return;
}

void set_message_VAO(struct line_text_image *message_image,
                     struct gl_strided_buffer *cbar_buf,
                     struct VAO_ids *msg_VAO){    
    const_texture_VBO(message_image->npix_img[0],
                      message_image->npix_img[1],
                      message_image->imgBMP,
                      msg_VAO, cbar_buf);
	return;
};
