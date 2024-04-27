
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

void colormap_to_glsl(const int id_cmap, const int num_cmap, const int num_alpha, 
                      const float table_ref[16], const float table_nrm[16],
                      const float alpha_ref[16], const float alpha_nrm[16],
                      struct shader_ids *phong_w_cmap){
    int id_normalized = 0;
    
    id_normalized = glGetUniformLocation(phong_w_cmap->programId, "colormap.id_cmap");
    glUniform1i(id_normalized, id_cmap);
    
    id_normalized = glGetUniformLocation(phong_w_cmap->programId, "colormap.num_normalize");
    glUniform1i(id_normalized, num_cmap);
    id_normalized =  glGetUniformLocation(phong_w_cmap->programId, "colormap.data_reference");
    glUniform1fv(id_normalized, num_cmap, table_ref);
    id_normalized = glGetUniformLocation(phong_w_cmap->programId, "colormap.data_normalized");
    glUniform1fv(id_normalized, num_cmap, table_nrm);
    
    id_normalized = glGetUniformLocation(phong_w_cmap->programId, "colormap.num_opacity");
    glUniform1i(id_normalized, num_alpha);
    id_normalized =  glGetUniformLocation(phong_w_cmap->programId, "colormap.alpha_reference");
    glUniform1fv(id_normalized, num_alpha, alpha_ref);
    id_normalized = glGetUniformLocation(phong_w_cmap->programId, "colormap.alpha_output");
    glUniform1fv(id_normalized, num_alpha, alpha_nrm);
    return;
};

