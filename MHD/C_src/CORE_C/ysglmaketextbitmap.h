/* ysglmaketextbitmap.h */

#ifndef YSGLMAKETEXTBITMAP_h_
#define YSGLMAKETEXTBITMAP_h_

#include "ysglfontdata.h"
#include <stdio.h>

int YsGlWriteStringToSingleBitBitmap(
	char str[],
    unsigned char bmpPtr[],unsigned int bmpWid,unsigned int bmpHei,
    int bottomLeftX,int bottomLeftY,
    unsigned char *fontPtr[],int fontWid,int fontHei);

int YsGlWriteStringToRGBA8Bitmap(
    char str[],unsigned int c0,unsigned int c1,unsigned int c2,unsigned int c3,
    unsigned char bmpPtr[],unsigned int bmpWid,unsigned int bmpHei,
    int bottomLeftX,int bottomLeftY,
    unsigned char *fontPtr[],int fontWid,int fontHei);
#endif
