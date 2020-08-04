
/* init_gl_lighting_c.c*/

#include "init_gl_lighting_c.h"


static const GLfloat default_background[4] = { 0.9, 0.9, 0.9, 1.0 };

void set_bg_color_kemoview(struct mesh_menu_val *mesh_m){
	int i;
	
	for(i=0;i<3;i++){
		if(mesh_m->bg_color[i] < 0.5) mesh_m->text_color[i] = 0.9;
		else mesh_m->text_color[i] = 0.1;
	}
	mesh_m->text_color[3] = ONE;
	
	glClearColor(mesh_m->bg_color[0], mesh_m->bg_color[1], 
				 mesh_m->bg_color[2], mesh_m->bg_color[3]);
	
}
void init_bg_color_kemoview(struct mesh_menu_val *mesh_m){
	int i;
	for(i=0;i<3;i++) mesh_m->bg_color[i] = default_background[i];
    
    set_bg_color_kemoview(mesh_m);
    return;
}

void kemo_gl_initial_lighting_c(struct view_element *view_s, 
			struct kemoview_shaders *kemo_shaders){	
	if (glslInit()) exit(1);
	LoadShaderFromStrings(kemo_shaders->simple, load_simple_vert(), load_simple_frag());
	LoadShaderFromStrings(kemo_shaders->phong, load_phong_vert(), load_phong_frag());
	LoadShaderFromStrings(kemo_shaders->phong_texure,
						load_phong_texture_vert(), load_phong_texture_frag());
	LoadShaderFromStrings(kemo_shaders->phong_1color, load_phong_vert(), load_phong_frag());
	LoadShaderFromStrings(kemo_shaders->simple_texure,
						load_simple_texture_vert(), load_simple_texture_frag());
	
	init_kemoview_perspective(view_s);
	init_projection_struct(view_s);
	
    /*   This glClear send error on Cocoa....  Why?*/
	glClear(GL_COLOR_BUFFER_BIT |GL_DEPTH_BUFFER_BIT);
    //    printf("kemo_gl_initial_lighting_c %d\n", glGetError());

	glDepthFunc(GL_LEQUAL);
	glEnable(GL_DEPTH_TEST);
	return;
}
