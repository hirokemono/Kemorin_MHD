
/* kemoviewer_glut.c */

#include "kemoviewer.h"
#include "m_kemoviewer_structure.h"
#include "kemo_mesh_viewer_glut.h"
#include "glsl.h"
#include "skip_comment_c.h"
#include "vartex_array_object_gl.h"
#include "shaders.h"
#include "drawcube_gl.h"

#define WINDOW_TITLE_PREFIX "Kemoviewer_GL3"
#define NPIX_X  800
#define NPIX_Y  640

static int winid, menu_win;
struct kemoviewer_type *single_kemoview;

unsigned FrameCount = 0;

int CurrentWidth = NPIX_X;
int CurrentHeight = NPIX_Y;
int WindowHandle = 0;

struct VAO_ids VAO_q;

struct shader_ids sampleShader;
struct shader_ids menuShader;

GLuint idx_indexBuf;

static GLint num_faces = 6;

static GLfloat cube_vertices [8][3] = {
	{1.0, 1.0, 1.0}, {1.0, -1.0, 1.0}, {-1.0, -1.0, 1.0}, {-1.0, 1.0, 1.0},
	{1.0, 1.0, -1.0}, {1.0, -1.0, -1.0}, {-1.0, -1.0, -1.0}, {-1.0, 1.0, -1.0} };

static GLfloat cube_vertex_colors [8][3] = {
	{1.0, 1.0, 1.0}, {1.0, 1.0, 0.0}, {0.0, 1.0, 0.0}, {0.0, 1.0, 1.0},
	{1.0, 0.0, 1.0}, {1.0, 0.0, 0.0}, {0.0, 0.0, 0.0}, {0.0, 0.0, 1.0} };

static GLfloat cube_normals [6][3] = {
	{0.0, 0.0, -1.0}, {-1.0, 0.0, 0.0}, {1.0, 0.0, 0.0}, {0.0, 1.0, 0.0},
	{0.0, -1.0, 0.0}, {0.0, 0.0, -1.0}};

static GLuint cube_faces [6][4] = {
	{3, 2, 1, 0}, {2, 3, 7, 6}, {0, 1, 5, 4}, {3, 0, 4, 7}, {1, 2, 6, 5}, {4, 5, 6, 7} };

static GLuint cube_tri_faces [12][3] = {
			{3, 2, 1}, {3, 1, 0}, {2, 3, 7}, {2, 7, 6}, {0, 1, 5}, {0, 5, 4}, 
			{3, 0, 4}, {3, 4, 7}, {1, 2, 6}, {1, 6, 5}, {4, 5, 6}, {4, 6, 7}};

static GLuint cube_edge [12][2] = {
			{0, 1}, {1, 2}, {2, 3}, {3, 0}, {4, 5}, {5, 6}, 
			{6, 7}, {7, 4}, {0, 4}, {1, 5}, {2, 6}, {3, 7}};

static GLuint cube_nodes[8] = {3, 2, 1, 0, 4, 5, 6, 7};


void cube_VBO(GLfloat fsize, struct VAO_ids *VAO_quad){
	
	GLenum ErrorCheckValue = glGetError();
	
	
//	drawCube_Element2(fsize);
	struct buffer_for_gl *gl_buf = (struct buffer_for_gl *) malloc(sizeof(struct buffer_for_gl));
	
	int i;
	
	gl_buf->num_nod_buf = 8;
	gl_buf->ncomp_buf = 20;
	
	gl_buf->ist_xyz =  0;
	gl_buf->ist_norm = 3;
	gl_buf->ist_tex =  6;
	gl_buf->ist_csurf = 8;
	
	int ist_cedge = 12;
	int ist_cnode = 16;
	
    GLuint idx_vertexBuf;
    GLuint idx_indexBuf;
	GLfloat gl_vertex[gl_buf->ncomp_buf * gl_buf->num_nod_buf];
	
	/* Set Stride for each vertex buffer */
	GLsizei stride = sizeof(GLfloat) * gl_buf->ncomp_buf;
	
	setCube_Element(fsize, ist_cedge, ist_cnode, gl_buf);
	
	
	glGenVertexArrays(1, &VAO_quad->id_VAO);
	glBindVertexArray(VAO_quad->id_VAO);
	
	/* Create vertex buffer on GPU and copy data from CPU*/
	glGenBuffers(1, &VAO_quad->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, VAO_quad->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * gl_buf->num_nod_buf*gl_buf->ncomp_buf,
				gl_buf->v_buf, GL_STATIC_DRAW);
	
	/* Set Vertex buffer */
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, stride, (sizeof(GLfloat)*gl_buf->ist_xyz));
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, stride, (sizeof(GLfloat)*gl_buf->ist_norm));
	glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, stride, (sizeof(GLfloat)*gl_buf->ist_tex));
	glVertexAttribPointer(3, 4, GL_FLOAT, GL_FALSE, stride, (sizeof(GLfloat)*gl_buf->ist_csurf));
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glEnableVertexAttribArray(2);
	glEnableVertexAttribArray(3);
	
	
	/* Create index buffer on GPU, and then copy from CPU */
	glGenBuffers(1, &idx_indexBuf);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, idx_indexBuf);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(int)*24, cube_edge, GL_STATIC_DRAW);
	
//	glDrawElements(GL_LINES, 24, GL_UNSIGNED_INT, 0);
	
//	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
//	glBindBuffer(GL_ARRAY_BUFFER, 0);
	
//	free(gl_buf);
	
	ErrorCheckValue = glGetError();
	if (ErrorCheckValue != GL_NO_ERROR)
	{
		fprintf(
					stderr,
					"ERROR: Could not create a VBO: %s \n",
					gluErrorString(ErrorCheckValue)
					);
		
		exit(-1);
	}
	
	glBindVertexArray(0);
	return;
}

void display_menu3(){
	glutSetWindow(menu_win);
	kemoview_draw_glut_menubottun();
	glUseProgram(menuShader.programId);
	VBO_for_Menu(&VAO_q);
	glDrawArrays(GL_POINTS, 0, MENU_HEIGHT*MENU_WIDTH);
	glutSwapBuffers();
	glutSetWindow(winid);
};

void RenderFunction(void)
{
	GLfloat model[16], proj[16];
	int i;
	
	glutSetWindow(winid);
  ++FrameCount;

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	
	
	glUseProgram(sampleShader.programId);
	identity_matrix_to_shader(&sampleShader);
	
	set_quadVBO(&VAO_q);
	glBindVertexArray(VAO_q.id_VAO);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VAO_q.id_index);
	glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
	
	glutSwapBuffers();
}

void IdleFunction(void)
{
  glutPostRedisplay();
}

void TimerFunction(int Value)
{
  if (0 != Value) {
    char* TempString = (char*)
      malloc(512 + strlen(WINDOW_TITLE_PREFIX));

    sprintf(
	    TempString,
	    "%s: %d Frames Per Second @ %d x %d",
	    WINDOW_TITLE_PREFIX,
	    FrameCount * 4,
	    CurrentWidth,
      CurrentHeight
	    );

    glutSetWindowTitle(TempString);
    free(TempString);
  }
  
  FrameCount = 0;
  glutTimerFunc(250, TimerFunction, 1);
}

void Cleanup(void)
{
  destory_shaders(&sampleShader);
  DestroyVBO(&VAO_q);
}

void init_kemoview_GLUT3(int iflag_streo_shutter) {
	int narg_glut = 0;
	char **arg_glut;
	
	/* Initialize arrays for viewer */
	
	kemoview_allocate_single_viwewer_struct(single_kemoview);
	kemoview_set_stereo_shutter(iflag_streo_shutter);
	
	if(iflag_streo_shutter == SHUTTER_ON){
		kemoview_set_anaglyph_flag(ANAGLYPH_OFF);
	} else {
		kemoview_set_anaglyph_flag(ANAGLYPH_ON);
	};
	
	/*! Initializations with GLUT*/
	glutInit(&narg_glut, arg_glut);
	if(iflag_streo_shutter == SHUTTER_ON){
		glutInitDisplayMode(GLUT_RGBA|GLUT_DOUBLE|GLUT_DEPTH
					|GLUT_MULTISAMPLE|GLUT_STEREO|GLUT_3_2_CORE_PROFILE);
		} else {
		glutInitDisplayMode(GLUT_RGBA|GLUT_DOUBLE|GLUT_DEPTH
					|GLUT_MULTISAMPLE|GLUT_3_2_CORE_PROFILE);
	};
	
	/*! Create viewer window */
	kemoview_set_retinamode(IZERO);
	kemoview_set_windowsize(NPIX_X, NPIX_Y);
	glutInitWindowSize(NPIX_X, NPIX_Y);
	
	winid = glutCreateWindow("Kemoviewer");
	set_main_window_id_glut(winid);
	
	/*! Set the display callback  */
	glutDisplayFunc(RenderFunction);
	glutReshapeFunc(modifywindow);
	glutIdleFunc(IdleFunction);
	glutTimerFunc(0, TimerFunction, 0);
	
	/*glutEntryFunc(enter_leave);*/
	  fprintf(
	  stdout,
	  "INFO: OpenGL Version: %s\n",
	  glGetString(GL_VERSION)
	  );
	
	/*! Create menu window*/
	menu_win = glutCreateSubWindow(winid,IZERO,IZERO,MENU_WIDTH,MENU_HEIGHT);
	/*glutEntryFunc(enter_leave);*/
	glutDisplayFunc(display_menu3);
	glutSetWindow(winid);
	
}

void draw_mesh_kemo3(int iflag_streo_shutter, int iflag_dmesh) {
	char *test_vert = load_test_vert();
	char *test_frag = load_test_frag();
	char *menu_vert = load_menu_vert();
	char *menu_frag = load_menu_frag();
	
	
	init_kemoview_GLUT3(iflag_streo_shutter);
	/*! set callback for GLUT*/
	
	glutSetWindow(winid);
	LoadShaderFromStrings(&sampleShader, test_vert, test_frag);
	
	glutSetWindow(menu_win);
	LoadShaderFromStrings(&menuShader, menu_vert, menu_frag);
	
	
	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
	glutMainLoop();
	
	Cleanup();
}

int main(int argc, char *argv[]){
	int iflag_streo_shutter = SHUTTER_OFF;
	int i;
	
	/*	printf("Number of arguments %d\n", argc);*/
	for (i = 0; i < argc; i++) {
/*		printf("%dth arguments: %s\n", i, argv[i]);*/
		if(strcmp(argv[i],"-help") == 0){
			printf("-stereo_shutter: Use streo monitor with shutter\n");
			return 0;
		}
	}
		
	for (i = 0; i < argc; i++) {
		if(strcmp(argv[i],"-stereo_shutter") == 0){
			printf("shutter ON\n");
			iflag_streo_shutter = SHUTTER_ON;
		}
	}
	
	draw_mesh_kemo3(iflag_streo_shutter, IZERO);
	return 0;
};
