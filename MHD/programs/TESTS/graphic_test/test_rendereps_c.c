
/* Copyright (c) Mark J. Kilgard, 1997. */

/* This program is freely distributable without licensing fees 
   and is provided without guarantee or warrantee expressed or 
   implied. This program is -not- in the public domain. */

/* Example showing how to use OpenGL's feedback mode to capture
   transformed vertices and output them as Encapsulated PostScript.
   Handles limited hidden surface removal by sorting and does
   smooth shading (albeit limited due to PostScript). */

/* Compile: cc -o rendereps rendereps_c.c test_rendereps_c.c -lglut -lGLU -lGL -lXmu -lXext -lX11 -lm */

#ifdef WITH_COCOA
#include <OpenGL/GLU.h>
#include <GLUT/glut.h>
#else
#include <GL/GLU.h>
#include <GL/glut.h>
#endif

#include "kemoviewer.h"
#include "rendereps_c.h"

/* OpenGL's GL_3D_COLOR feedback vertex format. */
typedef struct _Feedback3Dcolor {
  GLfloat x;
  GLfloat y;
  GLfloat z;
  GLfloat red;
  GLfloat green;
  GLfloat blue;
  GLfloat alpha;
} Feedback3Dcolor;

int blackBackground = 0;  /* Initially use a white background. */
int lighting = 0;       /* Initially disable lighting. */
int polygonMode = 1;    /* Initially show wireframe. */
int object = 1;         /* Initially show the torus. */

GLfloat angle = 0.0;    /* Angle of rotation for object. */
int moving, begin;      /* For interactive object rotation. */
int size = 1;           /* Size of lines and points. */

/* How many feedback buffer GLfloats each of the three objects need. */
int objectComplexity[3] =
{6000, 14000, 380000};  /* Teapot requires ~1.5 megabytes for
                           its feedback results! */

void
updateBackground(void)
{
  if (blackBackground) {
    /* Clear to black. */
    glClearColor(0.0, 0.0, 0.0, 1.0);
  } else {
    /* Clear to white. */
    glClearColor(1.0, 1.0, 1.0, 1.0);
  }
}

void
updateLighting(void)
{
  if (lighting) {
    glEnable(GL_LIGHTING);
  } else {
    glDisable(GL_LIGHTING);
  }
}

void
updatePolygonMode(void)
{
  switch (polygonMode) {
  case 0:
    glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
    break;
  case 1:
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    break;
  case 2:
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    break;
  }
}


void
choice(int value)
{
  switch (value) {
  case 0:
    glutSetCursor(GLUT_CURSOR_WAIT);
    outputEPS(1, objectComplexity[object], 1, " ", "render");
    glutSetCursor(GLUT_CURSOR_INHERIT);
    break;
  case 1:
    glutSetCursor(GLUT_CURSOR_WAIT);
    outputEPS(1, objectComplexity[object], 0, " ", "render");
    glutSetCursor(GLUT_CURSOR_INHERIT);
    break;
  case 2:
    /* Try to start GNU "ghostview" to preview the EPS. */
    system("ghostview render.eps &");
    break;
  case 3:
    glutSetCursor(GLUT_CURSOR_WAIT);
    outputEPS(1, objectComplexity[object], 0, " ", NULL);
    glutSetCursor(GLUT_CURSOR_INHERIT);
    break;
  case 4:
    blackBackground = 1 - blackBackground;
    updateBackground();
    glutPostRedisplay();
    break;
  case 5:
    lighting = 1 - lighting;
    updateLighting();
    glutPostRedisplay();
    break;
  case 6:
    polygonMode = (polygonMode + 1) % 3;
    updatePolygonMode();
    glutPostRedisplay();
    break;
  case 7:
    size = (size % 5) + 1;
    glLineWidth(size);
    glPointSize(size);
    glutPostRedisplay();
    break;
  case 8:
    object = (object + 1) % 3;
    glutPostRedisplay();
    break;
  case 666:
    exit(0);
    break;
  }
}

/* ARGSUSED2 */
void
mouse(int button, int state, int x, int y)
{
  if (button == GLUT_LEFT_BUTTON && state == GLUT_DOWN) {
    moving = 1;
    begin = x;
  }
  if (button == GLUT_LEFT_BUTTON && state == GLUT_UP) {
    moving = 0;
  }
}

/* ARGSUSED1 */
void
motion(int x, int y)
{
  if (moving) {
    angle = angle + (x - begin);
    begin = x;
    glutPostRedisplay();
  }
}

GLfloat light_diffuse[] =
{0.0, 1.0, 0.0, 1.0};   /* Green light. */
GLfloat light_position[] =
{1.0, 1.0, 1.0, 0.0};


/* render gets called both by "display" (in OpenGL render mode)
   and by "outputEPS" (in OpenGL feedback mode). */
void
render(void)
{
	glDeleteLists(1,1);
	glNewList(1 ,GL_COMPILE);
	
	glPushMatrix();
	glRotatef(angle, 0.0, 1.0, 0.0);
	switch (object) {
	case 0:
		glutSolidSphere(1.0, 10, 10);
		break;
	case 1:
		glutSolidTorus(0.5, 1.0, 15, 15);
		break;
	  case 2:
		glutSolidTeapot(1.0);
		break;
	}
	glPopMatrix();
	glEndList();
	}

void
display(void)
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	render();
	glCallList(1);
	glutSwapBuffers();
}

int
main(int argc, char **argv)
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGB);
  glutCreateWindow("rendereps");
  glutDisplayFunc(display);
  glutMouseFunc(mouse);
  glutMotionFunc(motion);

  glLightfv(GL_LIGHT0, GL_DIFFUSE, light_diffuse);
  glLightfv(GL_LIGHT0, GL_POSITION, light_position);
  glEnable(GL_LIGHT0);

  glMatrixMode(GL_PROJECTION);
  gluPerspective( /* field of view in degree */ 22.0,
  /* aspect ratio */ 1.0,
    /* Z near */ 5.0, /* Z far */ 10.0);
  glMatrixMode(GL_MODELVIEW);
  gluLookAt(0.0, 0.0, 5.0,  /* eye is at (0,0,5) */
    0.0, 0.0, 0.0,      /* center is at (0,0,0) */
    0.0, 1.0, 0.);      /* up is in postivie Y direction */
  glTranslatef(0.0, 0.0, -3.0);

  /* Give the object an "interesting" orientation. */
  glRotatef(25, 1.0, 0.0, 0.0);

  glutCreateMenu(choice);
  glutAddMenuEntry("Write out Encapsulated PS (sorted)", 0);
  glutAddMenuEntry("Write out Encapsulated PS (UNsorted)", 1);
  glutAddMenuEntry("Spawn ghostview to view EPS", 2);
  glutAddMenuEntry("Display feedback buffer", 3);
  glutAddMenuEntry("Toggle black/white background", 4);
  glutAddMenuEntry("Toggle lighting", 5);
  glutAddMenuEntry("Switch fill mode (line, poly, point)", 6);
  glutAddMenuEntry("Switch line/point size", 7);
  glutAddMenuEntry("Switch object", 8);
  glutAddMenuEntry("Quit", 666);
  glutAttachMenu(GLUT_RIGHT_BUTTON);

  updateBackground();
  updateLighting();
  updatePolygonMode();

  glEnable(GL_DEPTH_TEST);
  glColor3f(1.0, 0.0, 0.0);  /* Geometry should appear red. */

  glutMainLoop();
  return 0;             /* ANSI C requires main to return int. */
}
