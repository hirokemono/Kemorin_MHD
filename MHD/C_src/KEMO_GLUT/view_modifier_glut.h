
/* view_modifier_glut.h */

#ifndef VIEW_MODIFIER_GLUT_
#define VIEW_MODIFIER_GLUT_

#include <math.h>

#ifdef __APPLE__ 
#include <GLUT/glut.h>
#else
#include<GL/glut.h>
#endif

#include "kemoviewer.h"

#define ZOOM    1
#define PAN     2
#define ROTATE  3
#define SCALE   4
#define WALKTO  5

/* prototypes */

#ifdef __cplusplus
extern "C" {
#endif

void view_modifier_init() ;
int  menu_init() ;
void display_menu();
void display();
void modifywindow(int width, int height);

void set_left_button(GLint value);

#ifdef __cplusplus
}
#endif

#endif
