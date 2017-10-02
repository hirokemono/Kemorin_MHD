
/* gl_buffer_2_gl2ps.c */

#include "gl_buffer_2_gl2ps.h"

#define I_BUFFERSIZE 10000000

void gl_buffer_2_ps_gl2ps(int size, int doSort, const char *filehead){
	FILE *fp;
	char fname[LENGTHBUF];
	int state = GL2PS_OVERFLOW, buffsize = 0;
	
	sprintf(fname, "%s.ps",filehead);
	printf("EPS file name: %s \n",fname);
	
	fp = fopen(fname, "wb");
	printf("Writing %s... ",fname);
	
	while(state == GL2PS_OVERFLOW){
        buffsize += size;
        draw_kemoviewer_to_ps();
        gl2psBeginPage("Kemoviewer", "Kemoviewer", NULL, GL2PS_PS, GL2PS_SIMPLE_SORT,
				 GL2PS_USE_CURRENT_VIEWPORT |GL2PS_NO_PIXMAP|GL2PS_OCCLUSION_CULL,
				GL_RGBA, 0, NULL, 0, 0, 0, size, fp, fname);
		rotate_kemoview();
		state = gl2psEndPage();
	}
	fclose(fp);
	printf("Done!\n");
    draw_kemoviewer_c();
	
	return;
};

void gl_buffer_2_eps_gl2ps(int size, int doSort, const char *filehead){
	FILE *fp;
	char fname[LENGTHBUF];
	int state = GL2PS_OVERFLOW, buffsize = 0;
	
	sprintf(fname, "%s.eps",filehead);
	printf("EPS file name: %s \n",fname);
	
	fp = fopen(fname, "wb");
	printf("Writing %s... ",fname);
	
	while(state == GL2PS_OVERFLOW){
        buffsize += size;
        draw_kemoviewer_to_ps();
        gl2psBeginPage("Kemoviewer", "Kemoviewer", NULL, GL2PS_EPS, GL2PS_SIMPLE_SORT,
				 GL2PS_USE_CURRENT_VIEWPORT |GL2PS_NO_PIXMAP|GL2PS_OCCLUSION_CULL,
				GL_RGBA, 0, NULL, 0, 0, 0, size, fp, fname);
		rotate_kemoview();
		state = gl2psEndPage();
	}
	fclose(fp);
	printf("Done!\n");
    draw_kemoviewer_c();
	
	return;
};


void gl_buffer_2_pdf_gl2ps(int size, int doSort, const char *filehead){
	FILE *fp;
	char fname[LENGTHBUF];
	int state = GL2PS_OVERFLOW, buffsize = 0;
	
	sprintf(fname, "%s.pdf",filehead);
	printf("EPS file name: %s \n",fname);
	
	fp = fopen(fname, "wb");
	printf("Writing %s... ",fname);
	
	while(state == GL2PS_OVERFLOW){
        buffsize += size;
        draw_kemoviewer_to_ps();
        gl2psBeginPage("Kemoviewer", "Kemoviewer", NULL, GL2PS_PDF, GL2PS_SIMPLE_SORT,
				 GL2PS_USE_CURRENT_VIEWPORT |GL2PS_NO_PIXMAP|GL2PS_OCCLUSION_CULL,
				GL_RGBA, 0, NULL, 0, 0, 0, size, fp, fname);
		rotate_kemoview();
		state = gl2psEndPage();
	}
	fclose(fp);
	printf("Done!\n");
    draw_kemoviewer_c();
	
	return;
};

void sel_gl_buffer_2_vector_img(int iflag_img, const char *filehead){
	
	if (iflag_img == SAVE_EPS) { gl_buffer_2_eps_gl2ps(I_BUFFERSIZE, IONE, filehead); }
	else if (iflag_img == SAVE_PDF) { gl_buffer_2_pdf_gl2ps(I_BUFFERSIZE, IONE, filehead); }
	else if (iflag_img == SAVE_PS) { gl_buffer_2_ps_gl2ps(I_BUFFERSIZE, IONE, filehead); };
	
	return;
}

