
/* write_gl_window_to_file.c */

#include "write_gl_window_to_file.h"

#define I_BUFFERSIZE 10000000


int set_image_format_id_by_ext(char *image_fmt){
	int id_img;
	
	if      ( (image_fmt[0]=='0' && image_fmt[1]=='\0')
			||(image_fmt[0]=='N' && image_fmt[1]=='O' && image_fmt[2]=='\0')
			||(image_fmt[0]=='n' && image_fmt[1]=='o' && image_fmt[2]=='\0')){
		id_img = NO_SAVE_FILE;
	} else if ( (image_fmt[0]=='1' && image_fmt[1]=='\0')
			||(image_fmt[0]=='P' && image_fmt[1]=='N' && image_fmt[2]=='G')
			||(image_fmt[0]=='p' && image_fmt[1]=='n' && image_fmt[2]=='g')){
		id_img = SAVE_PNG;
	} else if(  (image_fmt[0]=='2' && image_fmt[1]=='\0')
			||(image_fmt[0]=='B' && image_fmt[1]=='M' && image_fmt[2]=='P')
			||(image_fmt[0]=='b' && image_fmt[1]=='m' && image_fmt[2]=='p')){
		id_img = SAVE_BMP;
	} else if(  (image_fmt[0]=='2' && image_fmt[1]=='0' && image_fmt[2]=='\0')
			||(image_fmt[0]=='P' && image_fmt[1]=='D' && image_fmt[2]=='F')
			||(image_fmt[0]=='p' && image_fmt[1]=='d' && image_fmt[2]=='f')){
		id_img = SAVE_PDF;
	} else {
		id_img = SAVE_UNDEFINED;
	};
	
	return id_img;
}

void write_gl_window_to_file(int iflag_img, const char *fhead, int nwin_x, int nwin_y){
	
/*	if (iflag_img == SAVE_EPS) { outputEPS(gl_drawID, I_BUFFERSIZE, IONE, colorbar_text, fhead); }*/
	if (iflag_img == SAVE_PNG)  { gl_buffer_2_png(fhead, nwin_x, nwin_y); }
    if (iflag_img == SAVE_BMP)  { gl_buffer_to_bmp(fhead, nwin_x, nwin_y); }
	else if(iflag_img == SAVE_PPM_B) { gl_buffer_to_ppm_p6(fhead, nwin_x, nwin_y); }
	else if(iflag_img == SAVE_PPM_A) { gl_buffer_to_ppm_p3(fhead, nwin_x, nwin_y); }
	
	return;
}

void write_gl_window_step_file(int iflag_img, int istep, const char *fhead,
			int nwin_x, int nwin_y){
	char fhead_step[LENGTHBUF];
	
	sprintf(fhead_step, "%s.%d%c",fhead, istep,'\0');
	/* printf("file header: %s \n",fhead_step); */
	write_gl_window_to_file(iflag_img, fhead_step, nwin_x, nwin_y);
	return;
}
