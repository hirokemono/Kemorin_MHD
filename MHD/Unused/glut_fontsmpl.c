/* glfont8x16.c */
/* ������Ѱդ����ӥåȥޥåץǡ����ˤ��ʸ����ɽ���Υ���ץ� */

#include <GL/glut.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "ysglusefontbitmap.h"

GLuint base;

void init()
{
  int i, n;

/*  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);*/

  /* �ǥ����ץ쥤�ꥹ���ΰ�� 128 �ĳ��� */
	base = glGenLists(0x100);
	printf("Base %d \n",base);
	
	/* ��ʸ���Υӥåȥޥåץǡ�����ƥǥ����ץ쥤�ꥹ�Ȥ˳�����Ƥ� */
	 YsGlUseFontBitmap12x16(base);
	glClearColor(1.0f, 1.0f, 1.0f, 0.0f);
}

void drawString(char *string)
{
	glPushAttrib(GL_LIST_BIT);
	
  /* ʸ���ե���ȥǥ����ץ쥤�ꥹ�Ȥγ��ϰ��� */
	glListBase(base);
 
  /* �ǥ����ץ쥤�ꥹ�� (ʸ����) ������ */
	glCallLists(strlen(string), GL_UNSIGNED_BYTE, (GLubyte *)string);

	glPopAttrib();
}

void display(void)
{
  glClear(GL_COLOR_BUFFER_BIT);
  glColor3f(0.0f, 0.0f, 0.0f);

  /* ������� */
  glRasterPos2i(5, 280); 
  drawString(" !\"#$%&'()*+,-./0123456789:;<=>?"); 

  glRasterPos2f(5.0f, 260.0f); /* ������� */
  drawString("@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"); /* ʸ�������� */

  glRasterPos2f(5.0f, 240.0f); /* ������� */
  drawString("`abcdefghijklmnopqrstuvwxyz{|}~"); /* ʸ�������� */

  glRasterPos2f(70.0f, 25.0f); /* ������� */
  drawString("OpenGL(TM) is a trademark"); /* ʸ�������� */
  glRasterPos2f(70.0f, 5.0f); /* ������� */
  drawString("of Silicon Graphics, Inc."); /* ʸ�������� */

  glFlush();
}

void keyboard(unsigned char key, int x, int y)
{
  switch (key) {
  case '\033': /* \033 �� ESC �� ASCII ������ */
  case 'q':
  case 'Q':
    exit(0);
  default :
    break;
  }
}

void resize(int width, int height)
{
  glViewport(0, 0, width, height);
  glLoadIdentity();
  glOrtho(0.0, (GLdouble)width, 0.0, (GLdouble)height, 0.0, 1.0);
}

int main(int argc, char* argv[])
{
  glutInit( &argc, argv );
  glutInitDisplayMode( GLUT_SINGLE | GLUT_RGB );
  glutInitWindowPosition( 100, 100 );
  glutInitWindowSize( 400, 300 );
  glutCreateWindow( argv[0] );
  glutReshapeFunc( resize );
  glutDisplayFunc( display );
  glutKeyboardFunc( keyboard );

  init();
  glutMainLoop();
  return 0;
}

