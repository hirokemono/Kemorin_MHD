
/* ysglusefontbitmap.c */

#include "ysglusefontbitmap.h"

void ysGlPlotBitmap2d(int listBase, GLdouble xbase, GLdouble ybase, GLubyte *text){
	int len;
	
	len = (int) strlen((char *) text);
	glRasterPos2d(xbase, ybase);
	glPushAttrib(GL_LIST_BIT);
	glListBase(listBase);
	glCallLists(len, GL_UNSIGNED_BYTE, text);
	glPopAttrib();
	
	return;
}

void ysGlPlotBitmap2i(int listBase, GLint ixbase, GLint iybase, GLubyte *text){
	int len;
	
	len = (int) strlen((char *) text);
	glRasterPos2i(ixbase, iybase);
	glPushAttrib(GL_LIST_BIT);
	glListBase(listBase);
	glCallLists(len, GL_UNSIGNED_BYTE, text);
	glPopAttrib();
	
	return;
}

static void ysGlMakeFontBitmapDisplayList(int listBase,unsigned char *fontPtr[],int wid,int hei)
{
	int i;
	glRasterPos2i(0,0);
	for(i=0; i<256; i++)
	{
		glNewList(listBase+i,GL_COMPILE);
		glBitmap(wid,hei,0,0,wid,0,fontPtr[i]);
		glEndList();
	}
}


void YsGlUseFontBitmap6x7(int listBase)
{
	ysGlMakeFontBitmapDisplayList(listBase,YsFont6x7,6,7);
}

void YsGlUseFontBitmap6x8(int listBase)
{
	ysGlMakeFontBitmapDisplayList(listBase,YsFont6x8,6,8);
}

void YsGlUseFontBitmap8x8(int listBase)
{
	ysGlMakeFontBitmapDisplayList(listBase,YsFont8x8,8,8);
}

void YsGlUseFontBitmap8x12(int listBase)
{
	ysGlMakeFontBitmapDisplayList(listBase,YsFont8x12,8,12);
}

void YsGlUseFontBitmap12x16(int listBase)
{
	ysGlMakeFontBitmapDisplayList(listBase,YsFont12x16,12,16);
}

void YsGlUseFontBitmap16x20(int listBase)
{
	ysGlMakeFontBitmapDisplayList(listBase,YsFont16x20,16,20);
}

void YsGlUseFontBitmap16x24(int listBase)
{
	ysGlMakeFontBitmapDisplayList(listBase,YsFont16x24,16,24);
}

void YsGlUseFontBitmap20x28(int listBase)
{
	ysGlMakeFontBitmapDisplayList(listBase,YsFont20x28,20,28);
}

void YsGlUseFontBitmap20x32(int listBase)
{
	ysGlMakeFontBitmapDisplayList(listBase,YsFont20x32,20,32);
}

