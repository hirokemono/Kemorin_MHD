
/* set_texture_4_psf.c */

#include "set_texture_4_psf.h"

void flip_gl_bitmap(int num_x, int num_y,
                    unsigned char *glimage, unsigned char *fliped_img){
    int i, j, k, l;
    
    for (i = 0; i < num_x; i++) {
        for (j = 0; j < num_y; j++) {
            k = (num_y-j-1)*num_x + i;
            l = j*num_x +i;
            fliped_img[3*l  ] = glimage[3*k];
            fliped_img[3*l+1] = glimage[3*k+1];
            fliped_img[3*l+2] = glimage[3*k+2];
        }
    }
    return;
}

void flip_gl_bitmap_to_img2d(int num_x, int num_y,
                             unsigned char *glimage, unsigned char **img_2d){
    int i, j, k;
    
    for (i = 0; i < num_x; i++) {
        for (j = 0; j < num_y; j++) {
            k = (num_y-j-1)*num_x + i;
            img_2d[j][3*i  ] = glimage[3*k];
            img_2d[j][3*i+1] = glimage[3*k+1];
            img_2d[j][3*i+2] = glimage[3*k+2];
        }
    }
    return;
}

static void vart_flip_rgba_c(int ihpixf, int jvpixf, const unsigned char *fliped_img,
                      unsigned char *image){
	int i, j, k, l;
    
    for(j=0;j<jvpixf;j++){
        for (i=0; i < ihpixf; i++) {
            k = i + j*ihpixf;
            l = i + (jvpixf-j-1)*ihpixf;
		/* reading in BGR order, not RGB. */
            image[4*k  ] = fliped_img[4*l  ];
            image[4*k+1] = fliped_img[4*l+1];
            image[4*k+2] = fliped_img[4*l+2];
            image[4*k+3] = fliped_img[4*l+3];
        };
	};
}

void set_texture_4_psf(int width, int height, const unsigned char *bgra_in, 
			struct psf_menu_val *psf_m) {
	
	psf_m->texture_width =  width;
	psf_m->texture_height = height;
	
	alloc_draw_psf_texture(psf_m);
	vart_flip_rgba_c(psf_m->texture_width, psf_m->texture_height, bgra_in, 
			psf_m->texture_rgba);
	return;
}

void release_texture_4_psf(struct psf_menu_val *psf_m) {
	dealloc_draw_psf_texture(psf_m);
	return;
}
