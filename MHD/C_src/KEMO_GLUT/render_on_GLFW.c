/*
//  render_on_GLFW.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include "render_on_GLFW.h"

void draw_full_lc(GLFWwindow *glfw_win,
                  struct kemoviewer_type *kemo_sgl,
                  struct kemoviewer_gl_type * kemo_gl){
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_sgl);
    kemoview_set_view_integer(ISET_DRAW_MODE, FULL_DRAW, kemo_sgl);
    kemoview_modify_anaglyph(kemo_sgl, kemo_gl);
    glfwSwapBuffers(glfw_win);
    return;
};

void draw_fast_lc(GLFWwindow *glfw_win,
                  struct kemoviewer_type *kemo_sgl,
                  struct kemoviewer_gl_type * kemo_gl){
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_sgl);
    kemoview_set_view_integer(ISET_DRAW_MODE, SIMPLE_DRAW, kemo_sgl);
    kemoview_mono_viewmatrix(kemo_sgl);
    kemoview_fast_buffers(kemo_sgl);
    glDrawBuffer(GL_BACK);
    kemoview_modify_view(kemo_sgl, kemo_gl);
    glfwSwapBuffers(glfw_win);
    return;
};

void draw_quilt_lc(int istep_qult, GLFWwindow *glfw_win,
                   struct kemoviewer_type *kemo_sgl,
                   struct kemoviewer_gl_type * kemo_gl){
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_sgl);
    kemoview_set_view_integer(ISET_DRAW_MODE, QUILT_DRAW, kemo_sgl);
    kemoview_step_viewmatrix(istep_qult, kemo_sgl);
    glDrawBuffer(GL_BACK);
    kemoview_modify_view(kemo_sgl, kemo_gl);
    glfwSwapBuffers(glfw_win);
    return;
};

double draw_rotate_views_lc(GLFWwindow *glfw_win,
                            struct kemoviewer_type *kemo_sgl,
                            struct kemoviewer_gl_type *kemo_gl,
                            int i_axis, int inc_deg, int num_rotation){
    struct timeval startwtime;
    struct timeval endwtime;
    double seq_time;
    double AverageFPS = 0.0;
    
    if(inc_deg <= 0) inc_deg = 1;
    int ied_deg = 1 + num_rotation * 360/inc_deg;
    
    kemoview_set_view_integer(ISET_ROTATE_AXIS, i_axis, kemo_sgl);
    kemoview_set_view_integer(ISET_DRAW_MODE, MOVIE_DRAW, kemo_sgl);
    glfwFocusWindow(glfw_win);
    double accum_time = 0.0;
    for (int i = 0; i< ied_deg; i++) {
        kemoview_set_view_integer(ISET_ROTATE_INCREMENT, (i*inc_deg), kemo_sgl);
        
        gettimeofday( &startwtime, NULL );
        glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
        kemoview_modify_anaglyph(kemo_sgl, kemo_gl);
        glfwSwapBuffers(glfw_win);
        gettimeofday( &endwtime, NULL );
        seq_time = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                             + endwtime.tv_sec - startwtime.tv_sec );
        accum_time = accum_time + seq_time;
/*        *SnapshotFPS = 1.0 / seq_time; */
    };
    AverageFPS =  (double) ied_deg / accum_time;
    return AverageFPS;
}

void draw_evolution_views_lc(GLFWwindow *glfw_win,
                             struct kemoviewer_type *kemo_sgl,
                             struct kemoviewer_gl_type *kemo_gl,
                             int ist_udt, int ied_udt, int inc_udt){
	glfwFocusWindow(glfw_win);
	glfwSwapBuffers(glfw_win);
	for (int i=ist_udt; i<(ied_udt+1); i++) {
		if( ((i-ist_udt)%inc_udt) == 0) {
			kemoview_viewer_evolution(i, kemo_sgl);
			glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
            draw_full_lc(glfw_win, kemo_sgl, kemo_gl);
			glfwSwapBuffers(glfw_win);
		}
	}
	return;
};


void write_rotate_views(GLFWwindow *glfw_win,
                        struct kemoviewer_type *kemo_sgl,
                        struct kemoviewer_gl_type *kemo_gl,
                        int ied_deg, int inc_deg,
                        int iflag_img, struct kv_string *image_prefix,
                        int npix_x, int npix_y, unsigned char *image) {
	for (int i = 0; i< ied_deg; i++) {
		kemoview_set_view_integer(ISET_ROTATE_INCREMENT, (i*inc_deg), kemo_sgl);
        kemoview_modify_anaglyph(kemo_sgl, kemo_gl);
        glfwSwapBuffers(glfw_win);
		kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
        kemoview_write_window_to_file_w_step(iflag_img, i, image_prefix,
                                             npix_x, npix_y, image);
	};
	return;
}

void write_rotate_quilt_views(GLFWwindow *glfw_win,
                              struct kemoviewer_type *kemo_sgl,
                              struct kemoviewer_gl_type *kemo_gl,
                              int ied_deg, int inc_deg,
                              int iflag_img, struct kv_string *image_prefix,
                              int npix_x, int npix_y, unsigned char *image) {
	int nimg_column = kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_COLUMN);
	int nimg_raw =    kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_RAW);
	unsigned char *quilt_image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
	
    int i_quilt;
	for (int i = 0; i< ied_deg; i++) {
		glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
		kemoview_set_view_integer(ISET_ROTATE_INCREMENT, (i*inc_deg), kemo_sgl);
		
		for(i_quilt=0;i_quilt<(nimg_column * nimg_raw);i_quilt++){
            draw_quilt_lc(i_quilt, glfw_win, kemo_sgl, kemo_gl);
			kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
			kemoview_add_quilt_img(i_quilt, kemo_sgl, image, quilt_image);
		};
        kemoview_write_window_to_file_w_step(iflag_img, i, image_prefix,
                                             (nimg_column * npix_x),
                                             (nimg_raw * npix_y),
                                             quilt_image);
	};
    free(quilt_image);
	return;
}

void write_evolution_quilt_views(GLFWwindow *glfw_win,
                                 struct kemoviewer_type *kemo_sgl,
                                 struct kemoviewer_gl_type *kemo_gl,
                                 int ist_udt, int ied_udt, int inc_udt,
                                 int iflag_img, struct kv_string *image_prefix,
                                 int npix_x, int npix_y, unsigned char *image) {
	int nimg_column = kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_COLUMN);
	int nimg_raw =    kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_RAW);
	unsigned char *quilt_image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
	int i, i_quilt;

	for (i=ist_udt; i<(ied_udt+1); i++) {
		if( ((i-ist_udt)%inc_udt) == 0) {
			
			kemoview_viewer_evolution(i, kemo_sgl);
			
			for(i_quilt=0;i_quilt<(nimg_column*nimg_raw);i_quilt++){
                draw_quilt_lc(i_quilt, glfw_win, kemo_sgl, kemo_gl);
				kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
				kemoview_add_quilt_img(i_quilt, kemo_sgl, image, quilt_image);
			};
            
            kemoview_write_window_to_file_w_step(iflag_img, i, image_prefix,
                                                 (nimg_column * npix_x),
                                                 (nimg_raw * npix_y),
                                                 quilt_image);
		}
	}
    free(quilt_image);
	return;
};

void write_evolution_views(GLFWwindow *glfw_win,
                           struct kemoviewer_type *kemo_sgl,
                           struct kemoviewer_gl_type *kemo_gl,
                           int ist_udt, int ied_udt, int inc_udt,
                           int iflag_img, struct kv_string *image_prefix,
                           int npix_x, int npix_y, unsigned char *image) {
	for(int i=ist_udt; i<(ied_udt+1); i++) {
		if( ((i-ist_udt)%inc_udt) == 0) {
			kemoview_viewer_evolution(i, kemo_sgl);
			glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
            draw_full_lc(glfw_win, kemo_sgl, kemo_gl);
			glfwSwapBuffers(glfw_win);
			
			kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
            kemoview_write_window_to_file_w_step(iflag_img, i, image_prefix,
                                                 npix_x, npix_y, image);
		}
	}
	return;
};
