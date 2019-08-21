
/* modify_stereo_view_gl.c*/

#include "modify_stereo_view_gl.h"

static void modify_stereo_shutter(struct view_element *view_s){
	update_left_projection_struct(view_s);
	modify_left_view_by_struct(view_s);
	glDrawBuffer(GL_BACK_LEFT);
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	glCallList(view_s->gl_drawID);

	update_right_projection_struct(view_s);
	modify_right_view_by_struct(view_s);
	glDrawBuffer(GL_BACK_RIGHT);
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	glCallList(view_s->gl_drawID);
	return;
};

static void rotate_stereo_shutter(struct view_element *view_s){
	update_left_projection_struct(view_s);
	rotate_left_view_by_struct(view_s);
	glDrawBuffer(GL_BACK_LEFT);
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	glCallList(view_s->gl_drawID);

	update_right_projection_struct(view_s);
	rotate_right_view_by_struct(view_s);
	glDrawBuffer(GL_BACK_RIGHT);
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	glCallList(view_s->gl_drawID);
	return;
};

static void modify_stereo_anaglyph(struct view_element *view_s){
	update_left_projection_struct(view_s);
	modify_left_view_by_struct(view_s);
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	glColorMask(GL_TRUE,GL_FALSE,GL_FALSE,GL_FALSE);
	glCallList(view_s->gl_drawID);
	
	update_right_projection_struct(view_s);
	modify_right_view_by_struct(view_s);
	glClear(GL_DEPTH_BUFFER_BIT);
	glColorMask(GL_FALSE,GL_TRUE,GL_TRUE,GL_FALSE);
	glCallList(view_s->gl_drawID);

	glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
	return;
};

static void rotate_stereo_anaglyph(struct view_element *view_s){
	update_left_projection_struct(view_s);
	rotate_left_view_by_struct(view_s);
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	glColorMask(GL_TRUE,GL_FALSE,GL_FALSE,GL_FALSE);
	glCallList(view_s->gl_drawID);
	glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
			
	update_right_projection_struct(view_s);
	rotate_right_view_by_struct(view_s);
	glClear(GL_DEPTH_BUFFER_BIT);
	glColorMask(GL_FALSE,GL_TRUE,GL_TRUE,GL_FALSE);
	glCallList(view_s->gl_drawID);

	glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
	return;
};

static void modify_mono_kemoview(struct view_element *view_s){
	update_projection_struct(view_s);
	modify_view_by_struct(view_s);
	
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	glCallList(view_s->gl_drawID);
	return;
};

static void rotate_mono_kemoview(struct view_element *view_s){
	update_projection_struct(view_s);
	rotate_view_by_struct(view_s);
	
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	glCallList(view_s->gl_drawID);
	return;
};

void modify_stereo_kemoview(struct mesh_menu_val *menu, struct view_element *view_s){
	if(menu->iflag_streo_stutter == SHUTTER_ON){
		if(view_s->iflag_view_type == VIEW_STEREO
		   && menu->iflag_streo_anaglyph == ANAGLYPH_OFF){
			modify_stereo_shutter(view_s);
		} else if(view_s->iflag_view_type == VIEW_STEREO
				  && menu->iflag_streo_anaglyph == ANAGLYPH_ON){
			glDrawBuffer(GL_BACK);
			modify_stereo_anaglyph(view_s);
		} else {
			glDrawBuffer(GL_BACK);
			modify_mono_kemoview(view_s);
		};
	} else {
		if(view_s->iflag_view_type == VIEW_STEREO){
			modify_stereo_anaglyph(view_s);
		} else {
			modify_mono_kemoview(view_s);
		}
	}
	return;
};

void rotate_stereo_kemoview(struct mesh_menu_val *menu, struct view_element *view_s){

	if(menu->iflag_streo_stutter == SHUTTER_ON){
		if(view_s->iflag_view_type == VIEW_STEREO
		   && menu->iflag_streo_anaglyph == ANAGLYPH_OFF){
			rotate_stereo_shutter(view_s);
		} else if(view_s->iflag_view_type == VIEW_STEREO
			&& menu->iflag_streo_anaglyph == ANAGLYPH_ON){
			glDrawBuffer(GL_BACK);
			rotate_stereo_anaglyph(view_s);
		} else {
			glDrawBuffer(GL_BACK);
			rotate_view_by_struct(view_s);
			rotate_mono_kemoview(view_s);
		};
	} else {
		if(view_s->iflag_view_type == VIEW_STEREO){
			rotate_stereo_anaglyph(view_s);
		} else {
			rotate_view_by_struct(view_s);
			rotate_mono_kemoview(view_s);
		}
	}
	return;
};

