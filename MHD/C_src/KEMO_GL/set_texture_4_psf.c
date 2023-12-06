
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

void quilt_bitmap_by_bgra(int n_quilt_column, int n_quilt_raw, int istep_quilt,
                          int npix_each_x, int npix_each_y, unsigned char *bgra,
                          unsigned char *fliped_quilt){
    int i, j, k, l;
    int i_column = istep_quilt % n_quilt_column;
    int j_raw =   (istep_quilt - i_column) / n_quilt_column;
    for (j = 0; j < npix_each_y; j++) {
        for (i = 0; i < npix_each_x; i++) {
//            k = i + (npix_each_y-j-1)*npix_each_x;
            k = i + j*npix_each_x;
			l = i + i_column * npix_each_x
					+ (j + (n_quilt_raw-j_raw-1)*npix_each_y) * (n_quilt_column*npix_each_x);
            fliped_quilt[3*l  ] = bgra[4*k+2];
            fliped_quilt[3*l+1] = bgra[4*k+1];
            fliped_quilt[3*l+2] = bgra[4*k  ];
        }
    }
    return;
};

void set_gl_quilt_bitmap(int n_quilt_column, int n_quilt_raw, int istep_quilt,
						 int npix_each_x, int npix_each_y, unsigned char *glimage, unsigned char *image_quilt){
    int i, j, k, l;
    int i_column = istep_quilt % n_quilt_column;
    int j_raw =   (istep_quilt - i_column) / n_quilt_column;
    for (j = 0; j < npix_each_y; j++) {
        for (i = 0; i < npix_each_x; i++) {
            k = i + j*npix_each_x;
			l = i + i_column * npix_each_x
					+ (j + j_raw*npix_each_y) * (n_quilt_column*npix_each_x);
            image_quilt[3*l  ] = glimage[3*k];
            image_quilt[3*l+1] = glimage[3*k+1];
            image_quilt[3*l+2] = glimage[3*k+2];
        }
    }
    return;
};


void half_anaglyph_rgba_by_rgbs(const int num_x, const int num_y,
                                const unsigned char *left_rgb,
                                const unsigned char *right_rgb,
                                unsigned char *anaglyph_rgb){
    int i;
    for(i=0;i<(num_x*num_y);i++){
        anaglyph_rgb[4*i  ] =  0.299 * left_rgb[3*i]
                             + 0.587 * left_rgb[3*i+1]
                             + 0.114 * left_rgb[3*i+2];
        anaglyph_rgb[4*i+1] = right_rgb[3*i+1];
        anaglyph_rgb[4*i+2] = right_rgb[3*i+2];
        anaglyph_rgb[4*i+3] = 255;
    }
    return;
}

void full_anaglyph_rgba_by_rgbs(const int num_x, const int num_y,
                                const unsigned char *left_rgb,
                                const unsigned char *right_rgb,
                                unsigned char *anaglyph_rgb){
    int i;
    for(i=0;i<(num_x*num_y);i++){
        anaglyph_rgb[4*i  ] = left_rgb[3*i  ];
        anaglyph_rgb[4*i+1] = right_rgb[3*i+1];
        anaglyph_rgb[4*i+2] = right_rgb[3*i+2];
        anaglyph_rgb[4*i+3] = 255;
    }
    return;
}




static void vertical_flip_rgba_c(int ihpixf, int jvpixf, const unsigned char *fliped_img,
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
                       struct kemo_PSF_texure *psf_texure){
	
    psf_texure->texure_width =  width;
    psf_texure->texure_height = height;
	
	alloc_draw_psf_texture(psf_texure);
    vertical_flip_rgba_c(psf_texure->texure_width, psf_texure->texure_height,
                         bgra_in, psf_texure->texure_rgba);
	return;
}

void release_texture_4_psf(struct kemo_array_control *psf_a) {
	dealloc_draw_psf_texture(psf_a->psf_texure);
	return;
}
