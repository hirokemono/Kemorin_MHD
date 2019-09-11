/* glfont8x16.c */
/* 配列で用意したビットマップデータによる文字列表示のサンプル */

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

  /* ディスプレイリスト領域を 128 個獲得 */
	base = glGenLists(0x100);
	printf("Base %d \n",base);
	
	/* 各文字のビットマップデータを各ディスプレイリストに割り当てる */
	 YsGlUseFontBitmap12x16(base);
	glClearColor(1.0f, 1.0f, 1.0f, 0.0f);
}

void drawString(char *string)
{
	glPushAttrib(GL_LIST_BIT);
	
  /* 文字フォントディスプレイリストの開始位置 */
	glListBase(base);
 
  /* ディスプレイリスト (文字列) を描画 */
	glCallLists(strlen(string), GL_UNSIGNED_BYTE, (GLubyte *)string);

	glPopAttrib();
}

void display(void)
{
  glClear(GL_COLOR_BUFFER_BIT);
  glColor3f(0.0f, 0.0f, 0.0f);

  /* 描画位置 */
  glRasterPos2i(5, 280); 
  drawString(" !\"#$%&'()*+,-./0123456789:;<=>?"); 

  glRasterPos2f(5.0f, 260.0f); /* 描画位置 */
  drawString("@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"); /* 文字列描画 */

  glRasterPos2f(5.0f, 240.0f); /* 描画位置 */
  drawString("`abcdefghijklmnopqrstuvwxyz{|}~"); /* 文字列描画 */

  glRasterPos2f(70.0f, 25.0f); /* 描画位置 */
  drawString("OpenGL(TM) is a trademark"); /* 文字列描画 */
  glRasterPos2f(70.0f, 5.0f); /* 描画位置 */
  drawString("of Silicon Graphics, Inc."); /* 文字列描画 */

  glFlush();
}

void keyboard(unsigned char key, int x, int y)
{
  switch (key) {
  case '\033': /* \033 は ESC の ASCII コード */
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

