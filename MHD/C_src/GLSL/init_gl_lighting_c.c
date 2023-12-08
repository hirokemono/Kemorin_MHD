
/* init_gl_lighting_c.c*/

#include "init_gl_lighting_c.h"


void set_gl_bg_color(float bg_color[4]){
    glClearColor(bg_color[0], bg_color[1], bg_color[2], bg_color[3]);
    return;
};

void kemo_gl_initial_lighting_c(struct kemoview_shaders *kemo_shaders){	
	if (glslInit()) exit(1);
	LoadShaderFromStrings(kemo_shaders->simple, load_simple_vert(), load_simple_frag());
	LoadShaderFromStrings(kemo_shaders->phong, load_phong_vert(), load_phong_frag());
	LoadShaderFromStrings(kemo_shaders->phong_texure,
                          load_phong_texture_vert(), load_phong_texture_frag());
	LoadShaderFromStrings(kemo_shaders->phong_1color, load_phong_vert(), load_phong_frag());
	LoadShaderFromStrings(kemo_shaders->simple_texure,
                          load_simple_texture_vert(), load_simple_texture_frag());
    LoadShaderFromStrings(kemo_shaders->anaglyph_texure,
                          load_anaglyph_texture_vert(), load_anaglyph_texture_frag());
	
    /*   This glClear send error on Cocoa....  Why?*/
	glClear(GL_COLOR_BUFFER_BIT |GL_DEPTH_BUFFER_BIT);

	glDepthFunc(GL_LEQUAL);
	glEnable(GL_DEPTH_TEST);
	return;
}

void init_gl_menu_setup(struct kemoview_shaders *kemo_shaders){
    LoadShaderFromStrings(kemo_shaders->menu, load_menu_vert(), load_menu_frag());
}
