
/* modify_stereo_view_gl.c*/

#include "modify_stereo_view_gl.h"

static void modify_stereo_shutter(struct kemoviewer_type *kemoview){
	update_left_projection_struct(kemoview->view_s);
	modify_left_view_by_struct(kemoview->view_s);
	glDrawBuffer(GL_BACK_LEFT);
	update_draw_objects_gl3(kemoview);

	update_right_projection_struct(kemoview->view_s);
	modify_right_view_by_struct(kemoview->view_s);
	glDrawBuffer(GL_BACK_RIGHT);
	update_draw_objects_gl3(kemoview);
	return;
};

static void rotate_stereo_shutter(struct kemoviewer_type *kemoview){
	update_left_projection_struct(kemoview->view_s);
	rotate_left_view_by_struct(kemoview->view_s);
	glDrawBuffer(GL_BACK_LEFT);
	update_draw_objects_gl3(kemoview);
	
	update_right_projection_struct(kemoview->view_s);
	rotate_right_view_by_struct(kemoview->view_s);
	glDrawBuffer(GL_BACK_RIGHT);
	update_draw_objects_gl3(kemoview);
	return;
};

static void modify_stereo_anaglyph(struct kemoviewer_type *kemoview){
	update_left_projection_struct(kemoview->view_s);
	modify_left_view_by_struct(kemoview->view_s);
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	glColorMask(GL_TRUE,GL_FALSE,GL_FALSE,GL_FALSE);
	update_draw_objects_gl3(kemoview);
	
	update_right_projection_struct(kemoview->view_s);
	modify_right_view_by_struct(kemoview->view_s);
	glClear(GL_DEPTH_BUFFER_BIT);
	glColorMask(GL_FALSE,GL_TRUE,GL_TRUE,GL_FALSE);
	update_draw_objects_gl3(kemoview);
	
	glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
	return;
};

static void rotate_stereo_anaglyph(struct kemoviewer_type *kemoview){
	update_left_projection_struct(kemoview->view_s);
	rotate_left_view_by_struct(kemoview->view_s);
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	glColorMask(GL_TRUE,GL_FALSE,GL_FALSE,GL_FALSE);
	update_draw_objects_gl3(kemoview);
	glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
			
	update_right_projection_struct(kemoview->view_s);
	rotate_right_view_by_struct(kemoview->view_s);
	glClear(GL_DEPTH_BUFFER_BIT);
	glColorMask(GL_FALSE,GL_TRUE,GL_TRUE,GL_FALSE);
	update_draw_objects_gl3(kemoview);
	glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
	return;
};

static void modify_mono_kemoview(struct kemoviewer_type *kemoview){
	update_projection_struct(kemoview->view_s);
	modify_view_by_struct(kemoview->view_s);
	
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	update_draw_objects_gl3(kemoview);
	return;
};

static void rotate_mono_kemoview(struct kemoviewer_type *kemoview){
	update_projection_struct(kemoview->view_s);
	rotate_view_by_struct(kemoview->view_s);
	
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	update_draw_objects_gl3(kemoview);
	return;
};

void quick_mono_kemoview(struct kemoviewer_type *kemoview){
	glDrawBuffer(GL_BACK);
	
	update_projection_struct(kemoview->view_s);
	modify_view_by_struct(kemoview->view_s);
	
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	quick_draw_objects_gl3(kemoview);
	return;
};

void modify_stereo_kemoview(struct kemoviewer_type *kemoview){
	if(kemoview->view_s->iflag_streo_stutter == SHUTTER_ON){
		if(kemoview->view_s->iflag_view_type == VIEW_STEREO
		   && kemoview->view_s->iflag_streo_anaglyph == ANAGLYPH_OFF){
			modify_stereo_shutter(kemoview);
		} else if(kemoview->view_s->iflag_view_type == VIEW_STEREO
				  && kemoview->view_s->iflag_streo_anaglyph == ANAGLYPH_ON){
			glDrawBuffer(GL_BACK);
			modify_stereo_anaglyph(kemoview);
		} else {
			glDrawBuffer(GL_BACK);
			modify_mono_kemoview(kemoview);
		};
	} else {
		if(kemoview->view_s->iflag_view_type == VIEW_STEREO){
			glDrawBuffer(GL_BACK);
			modify_stereo_anaglyph(kemoview);
		} else {
			glDrawBuffer(GL_BACK);
			modify_mono_kemoview(kemoview);
		}
	}
	return;
};

void rotate_stereo_kemoview(struct kemoviewer_type *kemoview){

	if(kemoview->view_s->iflag_streo_stutter == SHUTTER_ON){
		if(kemoview->view_s->iflag_view_type == VIEW_STEREO
		   && kemoview->view_s->iflag_streo_anaglyph == ANAGLYPH_OFF){
			rotate_stereo_shutter(kemoview);
		} else if(kemoview->view_s->iflag_view_type == VIEW_STEREO
			&& kemoview->view_s->iflag_streo_anaglyph == ANAGLYPH_ON){
			glDrawBuffer(GL_BACK);
			rotate_stereo_anaglyph(kemoview);
		} else {
			glDrawBuffer(GL_BACK);
			rotate_view_by_struct(kemoview->view_s);
			rotate_mono_kemoview(kemoview);
		};
	} else {
		if(kemoview->view_s->iflag_view_type == VIEW_STEREO){
			glDrawBuffer(GL_BACK);
			rotate_stereo_anaglyph(kemoview);
		} else {
			glDrawBuffer(GL_BACK);
			rotate_view_by_struct(kemoview->view_s);
			rotate_mono_kemoview(kemoview);
		}
	}
	return;
};

