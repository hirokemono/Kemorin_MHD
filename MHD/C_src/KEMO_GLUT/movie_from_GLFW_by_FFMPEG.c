/*
//  movie_from_GLFW_by_FFMPEG.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include "movie_from_GLFW_by_FFMPEG.h"

#ifdef FFMPEG
static void write_rotate_movie(GLFWwindow *glfw_win,
                               struct kemoviewer_gl_type *kemo_gl,
                               int ied_deg, int inc_deg,
                               struct kv_string *image_prefix, int i_fps, 
                               int npix_x, int npix_y, unsigned char *image){
    struct FFMPEG_encoder *kemo_encode;
    kemo_encode = init_FFMPEG_encoder(1, npix_x, npix_y, i_fps,
                                      image_prefix->string);
    for (int i = 0; i< ied_deg; i++) {
		kemoview_set_view_integer(ISET_ROTATE_INCREMENT, (i*inc_deg),
                                  kemo_gl->kemoview_data);
        kemoview_modify_anaglyph(kemo_gl);
        glfwSwapBuffers(glfw_win);
		kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
        encode_by_FFMPEG(npix_x, npix_y, image, kemo_encode);
	};
    finalize_FFMPEG_encoder(kemo_encode);
	return;
}

static void write_rotate_quilt_movie(GLFWwindow *glfw_win,
                                     struct kemoviewer_gl_type *kemo_gl,
                                     int ied_deg, int inc_deg,
                                     struct kv_string *image_prefix, int i_fps, 
                                     int npix_x, int npix_y, unsigned char *image) {
	int nimg_column = kemoview_get_quilt_nums(kemo_gl->kemoview_data,
                                              ISET_QUILT_COLUMN);
	int nimg_raw =    kemoview_get_quilt_nums(kemo_gl->kemoview_data,
                                              ISET_QUILT_RAW);
	unsigned char *quilt_image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
	
    struct FFMPEG_encoder *kemo_encode;
    kemo_encode = init_FFMPEG_encoder(1, npix_x, npix_y, i_fps,
                                      image_prefix->string);
    
    int i_quilt;
	for (int i = 0; i< ied_deg; i++) {
		glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
		kemoview_set_view_integer(ISET_ROTATE_INCREMENT, (i*inc_deg),
                                  kemo_gl->kemoview_data);
		
		for(i_quilt=0;i_quilt<(nimg_column * nimg_raw);i_quilt++){
            kemoview_gl_quilt_draw(i_quilt, kemo_gl);
            glfwSwapBuffers(glfw_win);
			kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
			kemoview_add_quilt_img(i_quilt, kemo_gl->kemoview_data,
                                   image, quilt_image);
		};
        encode_by_FFMPEG((nimg_column * npix_x),
                         (nimg_raw * npix_y),
                         quilt_image, kemo_encode);
	};
    finalize_FFMPEG_encoder(kemo_encode);
    free(quilt_image);
	return;
}

static void write_evolution_movie(GLFWwindow *glfw_win,
                                  struct kemoviewer_gl_type *kemo_gl,
                                  int ist_udt, int ied_udt, int inc_udt,
                                  struct kv_string *image_prefix, int i_fps, 
                                  int npix_x, int npix_y, unsigned char *image) {
    struct FFMPEG_encoder *kemo_encode;
    kemo_encode = init_FFMPEG_encoder(1, npix_x, npix_y, i_fps,
                                      image_prefix->string);
	for(int i=ist_udt; i<(ied_udt+1); i++) {
		if( ((i-ist_udt)%inc_udt) == 0) {
			kemoview_viewer_evolution(i, kemo_gl->kemoview_data);
			glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
            kemoview_gl_full_draw(kemo_gl);
			glfwSwapBuffers(glfw_win);
			
			kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
            encode_by_FFMPEG(npix_x, npix_y, image, kemo_encode);
		}
	}
    finalize_FFMPEG_encoder(kemo_encode);
	return;
};

static void write_evolution_quilt_movie(GLFWwindow *glfw_win,
                                        struct kemoviewer_gl_type *kemo_gl,
                                        int ist_udt, int ied_udt, int inc_udt,
                                        struct kv_string *image_prefix, int i_fps, 
                                        int npix_x, int npix_y, unsigned char *image) {
	int nimg_column = kemoview_get_quilt_nums(kemo_gl->kemoview_data,
                                              ISET_QUILT_COLUMN);
	int nimg_raw =    kemoview_get_quilt_nums(kemo_gl->kemoview_data,
                                              ISET_QUILT_RAW);
	unsigned char *quilt_image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
	int i, i_quilt;

    struct FFMPEG_encoder *kemo_encode;
    kemo_encode = init_FFMPEG_encoder(1, npix_x, npix_y, i_fps,
                                      image_prefix->string);
	for (i=ist_udt; i<(ied_udt+1); i++) {
		if( ((i-ist_udt)%inc_udt) == 0) {
			
			kemoview_viewer_evolution(i, kemo_gl->kemoview_data);
			
			for(i_quilt=0;i_quilt<(nimg_column*nimg_raw);i_quilt++){
                kemoview_gl_quilt_draw(i_quilt, kemo_gl);
                glfwSwapBuffers(glfw_win);
				kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
				kemoview_add_quilt_img(i_quilt, kemo_gl->kemoview_data,
                                       image, quilt_image);
			};
            encode_by_FFMPEG((nimg_column * npix_x),
                             (nimg_raw * npix_y),
                             quilt_image, kemo_encode);
		}
	}
    finalize_FFMPEG_encoder(kemo_encode);
    
    free(quilt_image);
	return;
};
#endif


void sel_lc_write_rotate_views(GLFWwindow *glfw_win,
                               struct kemoviewer_gl_type *kemo_gl,
                               int iflag_img, struct kv_string *image_prefix,
                               int i_fps, int i_axis, int inc_deg){
    if(inc_deg <= 0) inc_deg = 1;
    int ied_deg = 360/inc_deg;
    int npix_x = kemoview_get_view_integer(kemo_gl->kemoview_data,
                                           ISET_PIXEL_X);
    int npix_y = kemoview_get_view_integer(kemo_gl->kemoview_data,
                                           ISET_PIXEL_Y);
    unsigned char *image =      kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
    
    kemoview_set_view_integer(ISET_DRAW_MODE, MOVIE_DRAW, kemo_gl->kemoview_data);
    kemoview_set_view_integer(ISET_ROTATE_AXIS, i_axis, kemo_gl->kemoview_data);
    glfwFocusWindow(glfw_win);
    
#ifdef FFMPEG
    if(iflag_img == SAVE_QT_MOVIE){
        if(kemoview_get_quilt_nums(kemo_gl->kemoview_data, ISET_QUILT_MODE) > 0){
            write_rotate_quilt_movie(glfw_win, kemo_gl,
                                     ied_deg, inc_deg,
                                     image_prefix, i_fps, 
                                     npix_x, npix_y, image);
        }else{
            write_rotate_movie(glfw_win, kemo_gl,
                               ied_deg, inc_deg,
                               image_prefix, i_fps, 
                               npix_x, npix_y, image);
        };
        return;
    }
#endif
    
    if(kemoview_get_quilt_nums(kemo_gl->kemoview_data, ISET_QUILT_MODE) > 0){
        write_rotate_quilt_views(glfw_win, kemo_gl,
                                 ied_deg, inc_deg,
                                 iflag_img, image_prefix,
                                 npix_x, npix_y, image);
    }else{
        write_rotate_views(glfw_win, kemo_gl,
                           ied_deg, inc_deg,
                           iflag_img, image_prefix,
                           npix_x, npix_y, image);
	}
    
    kemoview_gl_full_draw(kemo_gl);
    glfwSwapBuffers(glfw_win);
	return;
}

void sel_lc_write_evolution_views(GLFWwindow *glfw_win,
                                  struct kemoviewer_gl_type *kemo_gl,
                                  int iflag_img, struct kv_string *image_prefix,
                                  int i_fps, int ist_udt, int ied_udt, int inc_udt){
    int npix_x = kemoview_get_view_integer(kemo_gl->kemoview_data,
                                           ISET_PIXEL_X);
    int npix_y = kemoview_get_view_integer(kemo_gl->kemoview_data,
                                           ISET_PIXEL_Y);
    unsigned char *image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
    
    glfwFocusWindow(glfw_win);
    glfwSwapBuffers(glfw_win);
    
#ifdef FFMPEG
    if(iflag_img == SAVE_QT_MOVIE){
        if(kemoview_get_quilt_nums(kemo_gl->kemoview_data, ISET_QUILT_MODE) != 0){
            write_evolution_quilt_movie(glfw_win, kemo_gl,
                                        ist_udt, ied_udt, inc_udt,
                                        image_prefix, i_fps,
                                        npix_x, npix_y, image);
        }else{
            write_evolution_movie(glfw_win, kemo_gl,
                                  ist_udt, ied_udt, inc_udt,
                                  image_prefix, i_fps,
                                  npix_x, npix_y, image);
        }
        return;
    }
#endif
    
    
	if(kemoview_get_quilt_nums(kemo_gl->kemoview_data, ISET_QUILT_MODE) != 0){
        write_evolution_quilt_views(glfw_win, kemo_gl,
                                    ist_udt, ied_udt, inc_udt,
                                    iflag_img, image_prefix,
                                    npix_x, npix_y, image);
	}else{
        write_evolution_views(glfw_win, kemo_gl,
                              ist_udt, ied_udt, inc_udt,
                              iflag_img, image_prefix,
                              npix_x, npix_y, image);
	};
    free(image);
    
    kemoview_gl_full_draw(kemo_gl);
    glfwSwapBuffers(glfw_win);
	return;
};
