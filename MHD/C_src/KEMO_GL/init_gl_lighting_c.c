
/* init_gl_lighting_c.c*/

#include "init_gl_lighting_c.h"


static const GLfloat white[4] =   {WHITE_R,WHITE_G,WHITE_B,WHITE_A};
static const GLfloat default_background[4] = { 0.9, 0.9, 0.9, 1.0 };

static GLfloat  lightposition[4] = {1.5,0.5,2.0,0.0};
static GLfloat  maplightposition[4] = {0.0,0.0,10.0,0.0};

/* GLfloat light0_pos[4]   =   { -35.0, 35.0, 50.0, 0.0 }; */
/* white light*/
/*static GLfloat light0_pos[4]   =   { -1.0, 1.0, 20.0, 0.0 };*/
static GLfloat Whitelight_color[4] =   { 0.8, 0.8, 0.8, 1.0 };

/* cold blue light */
static GLfloat light1_pos[4]   =   { 1.0, -1.0, -20.0, 0.0 };
/*static GLfloat light1_color[4] =   { 0.4, 0.4, 1.0, 1.0 };*/
/* white head light */
static GLfloat light2_pos[4]   =   { 0.0, 0.0, 0.0, 0.0 };
static GLfloat light2_color[4] =   { 1.0, 1.0, 1.0, 1.0 };	

/*static GLfloat lighta_ambient[4] = { 0.5, 0.5, 0.5, 1.0 };*/

void set_bg_color_kemoview(struct mesh_menu_val *mesh_m){
	int i;
	
	for(i=0;i<3;i++){
		if(mesh_m->bg_color[i] < 0.5) mesh_m->text_color[i] = 1.0;
		else mesh_m->text_color[i] = 0.0;
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
	int base;
	
	if(view_s->iflag_shading_profile == 1){
		if (glslInit()) exit(1);
		LoadShaderFromStrings(kemo_shaders->test, load_test_vert(), load_test_frag());
		LoadShaderFromStrings(kemo_shaders->phong, load_phong_vert(), load_phong_frag());
		LoadShaderFromStrings(kemo_shaders->phong_texure,
							  load_phong_texture_vert(), load_phong_texture_frag());
		LoadShaderFromStrings(kemo_shaders->phong_1color, load_phong_vert(), load_phong_frag());
		LoadShaderFromStrings(kemo_shaders->simple_texure,
							  load_simple_texture_vert(), load_simple_texture_frag());
	} else {
		view_s->gl_drawID = glGenLists(IONE);
	};
	
	init_kemoview_perspective(view_s);
	init_projection_struct(view_s);
	
    /*   This glClear send error on Cocoa....  Why?*/
	glClear(GL_COLOR_BUFFER_BIT |GL_DEPTH_BUFFER_BIT);
    //    printf("kemo_gl_initial_lighting_c %d\n", glGetError());
	glLightfv(GL_LIGHT0, GL_DIFFUSE, white);
	glLightfv(GL_LIGHT0, GL_POSITION, lightposition);
	glLightfv(GL_LIGHT1, GL_DIFFUSE,  white );
	glLightfv(GL_LIGHT1, GL_POSITION, light1_pos );
	glLightfv(GL_LIGHT2, GL_DIFFUSE,  light2_color );
	glLightfv(GL_LIGHT2, GL_POSITION, light2_pos );
	
    glLightfv(GL_LIGHT3, GL_DIFFUSE, white);
    glLightfv(GL_LIGHT3, GL_POSITION, maplightposition);
    
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glEnable(GL_LIGHT1);
	glDisable(GL_LIGHT2);
    glDisable(GL_LIGHT3);

    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, Whitelight_color);
	glEnable(GL_NORMALIZE);
	
	glDepthFunc(GL_LEQUAL);
	glEnable(GL_DEPTH_TEST);
	
	base= glGenLists(IONE);
	
	return;
}

void set_gl_3D_lighting_c(){
    
    glEnable(GL_LIGHT0);
    glEnable(GL_LIGHT1);
    glDisable(GL_LIGHT2);
    glDisable(GL_LIGHT3);
    
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, Whitelight_color);
    
    return;
}

void set_gl_map_lighting_c(){

    glDisable(GL_LIGHT0);
    glDisable(GL_LIGHT1);
    glDisable(GL_LIGHT2);
    glEnable(GL_LIGHT3);
    
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, Whitelight_color);
    
    return;
}

void reset_light_from_white_sf_c(int surface_color){
	
	glEnable(GL_LIGHT0);
	glLightModelfv(GL_LIGHT_MODEL_AMBIENT, Whitelight_color);
	
	return;
}

void reset_light_by_size_of_domain(GLdouble r_max){
	GLfloat  lightposi[4];
	
	lightposi[0] =  10.0 / (GLfloat) r_max;
	lightposi[1] =  -2.0 / (GLfloat) r_max;
	lightposi[2] =   5.0 / (GLfloat) r_max;
	lightposi[3] =   ONE;

     glDisable(GL_LIGHT0);
     glLightfv(GL_LIGHT0, GL_DIFFUSE, white);
     glLightfv(GL_LIGHT0, GL_POSITION, lightposi);
     glEnable(GL_LIGHTING);
     glEnable(GL_LIGHT0);
     glLightModelfv(GL_LIGHT_MODEL_AMBIENT, Whitelight_color);

     glDepthFunc(GL_LEQUAL);
     glEnable(GL_DEPTH_TEST);
	return;
}
