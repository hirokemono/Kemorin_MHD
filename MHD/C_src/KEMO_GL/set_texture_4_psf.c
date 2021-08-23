
/* set_texture_4_psf.c */

#include "set_texture_4_psf.h"

void flip_gl_bitmap(int num_x, int num_y,
                    unsigned char *glimage, unsigned char *fliped_img){
    int i, j, k, l;
        
    for (j = 0; j < num_y; j++) {
        for (i = 0; i < num_x; i++) {
            k = i + (num_y-j-1)*num_x;
            l = i + j*num_x;
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
    
    for (j = 0; j < num_y; j++) {
        for (i = 0; i < num_x; i++) {
            k = i + (num_y-j-1)*num_x;
            img_2d[j][3*i  ] = glimage[3*k];
            img_2d[j][3*i+1] = glimage[3*k+1];
            img_2d[j][3*i+2] = glimage[3*k+2];
        }
    }
    return;
}

void flip_gl_quilt_bitmap(int n_quilt_column, int n_quilt_raw, int istep_quilt,
                          int npix_each_x, int npix_each_y, unsigned char *glimage, unsigned char *fliped_quilt){
    int i, j, k, l;
    int i_column = istep_quilt % n_quilt_column;
    int j_raw =   (istep_quilt - i_column) / n_quilt_column;
    for (j = 0; j < npix_each_y; j++) {
        for (i = 0; i < npix_each_x; i++) {
            k = i + (npix_each_y-j-1)*npix_each_x;
            l = i + i_column * npix_each_x + (j + (n_quilt_raw-j_raw-1)*npix_each_y) * (n_quilt_column*npix_each_x);
            fliped_quilt[3*l  ] = glimage[3*k];
            fliped_quilt[3*l+1] = glimage[3*k+1];
            fliped_quilt[3*l+2] = glimage[3*k+2];
        }
    }
    return;
};




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
