
/* view_modifier_glfw.c */


#include "view_modifier_glfw.h"

/* initial settings */

GLFWwindow *glfw_window;
struct kemoviewer_type *    kemoview_GLFW;
struct kemoviewer_gl_type * kemoGL_GLFW;
int iflag_quickdraw = 0;

static int left_button_func =   ROTATE;
static int middle_button_func = PAN;
static int right_button_func =  ZOOM;
static int arrow_key_func =     PAN;

static int      moving_left, moving_middle, moving_right;
static double  begin_left[2], begin_middle[2], begin_right[2];

static double  begin[2];
static double  gTrackBallRotation[4];


void set_GLFW_viewtype_mode(int selected){
    if(selected == RESET) selected = VIEW_3D;

    if(selected == VIEW_3D
                || selected == VIEW_STEREO){
        left_button_func = ROTATE;
    }
    else if(selected == VIEW_MAP
                || selected == VIEW_XY
                || selected == VIEW_XZ
                || selected == VIEW_YZ) {
        left_button_func = PAN;
    };
    return;
}


void mouseButtonCB(GLFWwindow *window, int button, int action, int mods) {
	double xpos;
	double ypos;
	glfwGetCursorPos(window, &xpos, &ypos);
/*	printf("mouseButtonCB %d %d %d %lf, %lf\n", button, action, mods, xpos, ypos);*/

	if(button == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_PRESS) {
		moving_left = 1;
		begin_left[0] = xpos;
		begin_left[1] = ypos;
	};
	if(button == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_RELEASE) {
		moving_left = 0;
	};
	
	if(button == GLFW_MOUSE_BUTTON_MIDDLE && action == GLFW_PRESS) {
		moving_middle = 1;
		begin_middle[0] = xpos;
		begin_middle[1] = ypos;
	};
	if(button == GLFW_MOUSE_BUTTON_MIDDLE && action == GLFW_RELEASE) {
		moving_middle = 0;
	};
	
	if(button == GLFW_MOUSE_BUTTON_RIGHT && action == GLFW_PRESS) {
		moving_right = 1;
		begin_right[0] = xpos;
		begin_right[1] = ypos;
	};
	if(button == GLFW_MOUSE_BUTTON_RIGHT && action == GLFW_RELEASE) {
		moving_right = 0;
	};
	
	if(action == GLFW_RELEASE){
		draw_full(kemoview_GLFW);
	};
	return;
};

void mousePosCB(GLFWwindow *window, double xpos, double ypos) {
	/*! This gets called when the mouse moves */
	double factor;
	int button_function = left_button_func;
	
	/*printf("mousePosCB %.1lf %.1lf\n", xpos, ypos); */
	if (moving_left == 0 && moving_middle == 0 && moving_right == 0) return;
	
	/*! Determine and apply the button function */
	
	if (moving_left == 1) {
        button_function = left_button_func;
        begin[0] = begin_left[0];
        begin[1] = begin_left[1];
	}
	else if (moving_middle == 1) {
        button_function = middle_button_func;
        begin[0] = begin_middle[0];
        begin[1] = begin_middle[1];
	}
	else if (moving_right == 1) {
        button_function = right_button_func;
        begin[0] = begin_right[0];
        begin[1] = begin_right[1];
	};
	
	if (button_function == ZOOM){
		factor = -0.5*(ypos-begin[1]);
		kemoview_zooming(factor, kemoview_GLFW);
	}
	
	if (button_function == WALKTO){
		kemoview_mousedolly(begin, xpos, ypos, kemoview_GLFW);
	}
	else if(button_function == PAN){
		kemoview_mousepan(begin, xpos, ypos, kemoview_GLFW);
	}
	else if (button_function == ROTATE) {
		gTrackBallRotation[0] = ZERO;
		gTrackBallRotation[1] = ZERO;
		gTrackBallRotation[2] = ZERO;
		gTrackBallRotation[3] = ZERO;
		
		kemoview_startTrackball( begin[0], (-begin[1]), kemoview_GLFW);
		kemoview_rollToTrackball( xpos, (-ypos), kemoview_GLFW);
		kemoview_drugging_addToRotationTrackball(kemoview_GLFW);
	}
	else if (button_function == SCALE){
		double current_scale = kemoview_get_view_parameter(kemoview_GLFW, ISET_SCALE, 0);
        
		if (ypos < begin[1]) {
			factor = ONE + TWO_MILI*(begin[1]-ypos);
		}
		else if (ypos > begin[1]) {
			factor = ONE/(ONE  + TWO_MILI*(ypos-begin[1]));
		}
		else {
			factor = ONE;
		};
		current_scale = current_scale * factor;
		kemoview_set_view_parameter(ISET_SCALE, 0, current_scale, kemoview_GLFW);
	};
    /* ! update private variables and redisplay */
	
	if (moving_left) {
		begin_left[0] = xpos;
		begin_left[1] = ypos;
    }
	else if(moving_middle) {
		begin_middle[0] = xpos;
		begin_middle[1] = ypos;
	}
	else if(moving_right) {
		begin_right[0] = xpos;
		begin_right[1] = ypos;
	};
	return;
}

void set_GLFWindowSize(int width, int height,
                       struct kemoviewer_type *kemo_sgl){
	glfwSetWindowSize(glfw_window, width, height);
	kemoview_update_projection_by_viewer_size(width, height,
                                              width, height,
                                              kemo_sgl);
	glViewport(IZERO, IZERO, (GLint) width, (GLint) height);
};

void mouseScrollCB(GLFWwindow *window, double x, double y) {
/*	printf("mouseScrollCB %.1lf %.1lf\n", x, y);*/
    double newScale = x + y;
    kemoview_zooming(newScale, kemoview_GLFW);
}
void charFunCB(GLFWwindow* window, unsigned int charInfo) {
	printf("charFunCB %d\n", charInfo);
}


/*! This routine handles the arrow key operations */
static void keyFuncCB(GLFWwindow* window, int key, int scancode, int action, int mods){
    double x_dbl, y_dbl;
	double factor;
	
/*	printf("keyFuncCB %d %d %d %d\n", key, scancode, action, mods);	*/
	
	x_dbl = ZERO;
	y_dbl = ZERO;
	if (arrow_key_func == ZOOM){
		if (key == GLFW_KEY_DOWN && action == GLFW_PRESS){
			factor = ONE;
		}
		else if (key == GLFW_KEY_UP && action == GLFW_PRESS){
			factor = -ONE;
		}
		else {
			factor = ZERO;
		};
		kemoview_zooming(factor, kemoview_GLFW);
	}
	
	else if (arrow_key_func == WALKTO){
		begin[0] = ZERO;
		begin[1] = ZERO;
		x_dbl = ZERO;
		y_dbl = ZERO;
		if (key == GLFW_KEY_DOWN && action == GLFW_PRESS){
			x_dbl = ONE;
		}
		else if (key == GLFW_KEY_UP && action == GLFW_PRESS){
			y_dbl = -ONE;
		}
		else {
			factor = ZERO;
		};
		kemoview_mousedolly(begin, x_dbl, y_dbl, kemoview_GLFW);
	}
	
	else if (arrow_key_func == PAN){
		begin[0] = ZERO;
		begin[1] = ZERO;
		if (key == GLFW_KEY_LEFT && action == GLFW_PRESS){
			x_dbl = -ONE;
			y_dbl = ZERO;
		}
		else if (key == GLFW_KEY_RIGHT && action == GLFW_PRESS){
			x_dbl = ONE;
			y_dbl = ZERO;
		}
		else if (key == GLFW_KEY_DOWN && action == GLFW_PRESS){
			x_dbl = ZERO;
			y_dbl = ONE;
		}
		else if (key == GLFW_KEY_UP && action == GLFW_PRESS){
			x_dbl = ZERO;
			y_dbl = -ONE;
		};
		kemoview_mousepan(begin, x_dbl, y_dbl, kemoview_GLFW);
	}
	
	else if (arrow_key_func == ROTATE){
		if (key == GLFW_KEY_LEFT && action == GLFW_PRESS){
			x_dbl = begin[0] - TEN;
			y_dbl = begin[1] + ZERO;
		}
		else if (key == GLFW_KEY_RIGHT && action == GLFW_PRESS){
			x_dbl = begin[0] + TEN;
			y_dbl = begin[1] + ZERO;
		}
		else if (key == GLFW_KEY_DOWN && action == GLFW_PRESS){
			x_dbl = begin[0] + ZERO;
			y_dbl = begin[1] + TEN;
		}
		else if (key == GLFW_KEY_UP && action == GLFW_PRESS){
			x_dbl = begin[0] + ZERO;
			y_dbl = begin[1] - TEN;
		};
		kemoview_startTrackball( begin[0], (-begin[1]), kemoview_GLFW);
		kemoview_rollToTrackball( x_dbl, (-y_dbl), kemoview_GLFW);
		kemoview_drugging_addToRotationTrackball(kemoview_GLFW);
	}
	
	else if (arrow_key_func == SCALE){
		double current_scale = kemoview_get_view_parameter(kemoview_GLFW, ISET_SCALE, 0);
        
		if (key == GLFW_KEY_DOWN && action == GLFW_PRESS)
		factor = ONE/(ONE + TWO_CENT);
		else if (key == GLFW_KEY_UP && action == GLFW_PRESS){
			factor = ONE + TWO_CENT;
		}
		else {
			factor = ONE;
		};
		current_scale = current_scale * factor;
		kemoview_set_view_parameter(ISET_SCALE, 0, current_scale, kemoview_GLFW);
 	};
	
	glfwSwapBuffers(window);
	return;
}

GLFWwindow * open_kemoviwer_glfw_window(int npixel_x, int npixel_y){
	glfw_window = glfwCreateWindow(npixel_x, npixel_y, "CalypsoView", NULL, NULL);
	if (!glfw_window)
	{
		glfwTerminate();
		exit(1);
	}

	/* Make the window's context current */
	glfwSetWindowSize(glfw_window, npixel_x, npixel_y);
	glfwMakeContextCurrent(glfw_window);
	return glfw_window;
};


void glfw_callbacks_init(struct kemoviewer_type *kemo_sgl,
                         struct kemoviewer_gl_type *kemo_gl){
    kemoview_GLFW = kemo_sgl;
    kemoGL_GLFW = kemo_gl;
    
    
	/* set callback for mouse button */
	glfwSetMouseButtonCallback(glfw_window, mouseButtonCB);
	/* set callback for cursor position */
	glfwSetCursorPosCallback(glfw_window, mousePosCB);
	/* set callback for cursor position */
	glfwSetScrollCallback(glfw_window, mouseScrollCB);
	
	/* Set callback for keyboard input */
	glfwSetKeyCallback(glfw_window, keyFuncCB);
	glfwSetCharCallback(glfw_window, charFunCB);
	
	return;
}

static void light_for_cube(struct initial_cube_lighting *init_light,
                                   struct kemoview_shaders *kemo_shaders){
    int id_numLight = glGetUniformLocation(kemo_shaders->phong->programId, "num_lights");
	int id_light1Position = glGetUniformLocation(kemo_shaders->phong->programId, "LightSource[0].position");
	int id_light2Position = glGetUniformLocation(kemo_shaders->phong->programId, "LightSource[1].position");
	
	int id_MaterialAmbient = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.ambient");
	int id_MaterialDiffuse = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.diffuse");
	int id_MaterialSpecular = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.specular");
	int id_MaterialShiness = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.shininess");
	
	glUniform1i(id_numLight, init_light->num_light);
	glUniform4fv(id_light1Position, 1, init_light->lightposition[0]);
	glUniform4fv(id_light2Position, 1 , init_light->lightposition[1]);
	
	glUniform4fv(id_MaterialAmbient, 1, init_light->whitelight[0]);
	glUniform4fv(id_MaterialDiffuse, 1, init_light->whitelight[1]);
	glUniform4fv(id_MaterialSpecular, 1, init_light->whitelight[2]);
	glUniform1fv(id_MaterialShiness, 1, init_light->shine);
	return;
};

void test_VAO_4_Phong(struct VAO_ids *VAO, struct gl_strided_buffer *strided_buf){
    VAO->npoint_draw = strided_buf->num_nod_buf;
    if(VAO->npoint_draw <= 0) return;
    
	glBindVertexArray(VAO->id_VAO);
	glDeleteBuffers(1, &VAO->id_vertex);

	glGenBuffers(1, &VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * strided_buf->num_nod_buf*strided_buf->ncomp_buf,
				 strided_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
						  (GLvoid*) (strided_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
						  (GLvoid*) (strided_buf->ist_csurf * sizeof(GL_FLOAT)));
	glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
						  (GLvoid*) (strided_buf->ist_norm * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glEnableVertexAttribArray(2);
	glBindVertexArray(0);
	return;
};

void test_cube_surf_VBO(struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf,
                   struct gl_index_buffer *index_buf)
{
    GLenum ErrorCheckValue = glGetError();
    
    test_VAO_4_Phong(VAO_quad, gl_buf);
    
    glBindVertexArray(VAO_quad->id_VAO);
    glDeleteBuffers(1, &VAO_quad->id_index);
    glGenBuffers(1, &VAO_quad->id_index);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VAO_quad->id_index);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, (36 * sizeof(unsigned int)),
                 index_buf->ie_buf, GL_STATIC_DRAW);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
}

void set_cube_VAO(struct gl_strided_buffer *cube_buf, struct gl_index_buffer *index_buf,
                          struct VAO_ids *cube_VAO){
    cube_VAO->npoint_draw = cube_buf->num_nod_buf;
	if(cube_VAO->npoint_draw <= 0) return;
	test_cube_surf_VBO(cube_VAO, cube_buf, index_buf);
	glBindVertexArray(0);
	return;
};

void draw_test_cube(struct transfer_matrices *matrices, struct VAO_ids *cube_VAO,
                       struct kemoview_shaders *kemo_shaders){
    if(cube_VAO->npoint_draw <= 0) return;

    struct initial_cube_lighting *init_light = init_inital_cube_lighting();

    glUseProgram(kemo_shaders->phong->programId);
    transfer_matrix_to_GL(kemo_shaders->phong, matrices);
	light_for_cube(init_light, kemo_shaders);
    free(init_light);

	glBindVertexArray(cube_VAO->id_VAO);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, cube_VAO->id_index);
	glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, 0);
	return;
}

static void draw_cube_VAO(struct view_element *view_s, struct kemoview_VAOs *kemo_VAOs,
                              struct kemoview_shaders *kemo_shaders){
    double *orthogonal = orthogonal_projection_mat_c(0.0, view_s->nx_frame,
                                                     0.0, view_s->ny_frame,
                                                     -1.0, 1.0);
    struct transfer_matrices *cbar_matrices = plane_transfer_matrices(orthogonal);
    struct transfer_matrices *view_matrices = transfer_matrix_to_shader(view_s);
    struct transfer_matrices *map_matrices = init_projection_matrix_for_map(view_s->nx_frame, view_s->ny_frame);
    free(orthogonal);
	
	/* draw example cube for empty data */
    draw_test_cube(view_matrices, kemo_VAOs->cube_VAO, kemo_shaders);
	
    free(map_matrices);
    free(view_matrices);
    free(cbar_matrices);
    return;
}

static void update_draw_objectss(struct view_element *view_s,
                                struct kemoview_buffers *kemo_buffers,
                                struct kemoview_VAOs *kemo_VAOs,
                                struct kemoview_shaders *kemo_shaders){
/* Set Vertex buffers */
    if(view_s->iflag_draw_mode != SIMPLE_DRAW){
		kemo_buffers->cube_buf->num_nod_buf = kemo_buffers->cube_index_buf->nsize_buf;
		set_cube_VAO(kemo_buffers->cube_buf, kemo_buffers->cube_index_buf,
							 kemo_VAOs->cube_VAO);
    }
	draw_cube_VAO(view_s, kemo_VAOs, kemo_shaders);
	return;
}

void select_anaglyph(struct kemoviewer_type *kemo_sgl){
	kemoview_mono_viewmatrix(kemo_sgl);
	glDrawBuffer(GL_BACK);
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	update_draw_objectss(kemo_sgl->view_s, kemo_sgl->kemo_buffers,
                        kemoGL_GLFW->kemo_VAOs, kemoGL_GLFW->kemo_shaders);
	glfwSwapBuffers(glfw_window);
    return;
}

void draw_full(struct kemoviewer_type *kemo_sgl){
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_sgl);
    kemoview_set_view_integer(ISET_DRAW_MODE, FULL_DRAW, kemo_sgl);
    select_anaglyph(kemo_sgl);
	return;
};

void draw_fast(struct kemoviewer_type *kemo_sgl){
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_sgl);
    kemoview_set_view_integer(ISET_DRAW_MODE, SIMPLE_DRAW, kemo_sgl);
    select_anaglyph(kemo_sgl);
    return;
};

void kemoview_gl_shader_init(struct kemoview_shaders *kemo_shaders){	
	if (glslInit()) exit(1);
	LoadShaderFromStrings(kemo_shaders->phong, load_phong_cmap_vert(), load_phong_cmap_frag());
	
    /*   This glClear send error on Cocoa....  Why?*/
	glClear(GL_COLOR_BUFFER_BIT |GL_DEPTH_BUFFER_BIT);
	
	glDepthFunc(GL_LEQUAL);
	glEnable(GL_DEPTH_TEST);
	return;
}


void kemoview_gl_test_init(struct kemoviewer_gl_type *kemo_gl){
    kemoview_gl_shader_init(kemo_gl->kemo_shaders);
    assign_kemoview_VAOs(kemo_gl->kemo_VAOs);
    return;
}
