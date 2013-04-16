
/* get_ysglbitmap.c */

#include "get_ysglbitmap.h"


void generate_ysfont8x12(const char input, int i_font[12][8]){
	int i_input;
	int i_rev[12][8];
	int iskip = 4;
	int i, j;
	
	i_input = (int)input;
	/*printf("ID: %d HEX ID: %x Char: %c\n",i_input, i_input, i_input);*/
	for (j = 0; j < YSGL_12; j++) {
		uchar_2binarray(YsFont8x12[i_input][iskip*j], i_rev[j]);
	};
	for (j = 0; j < YSGL_12; j++) {
		for(i=0;i<XSGL_8;i++) {i_font[j][i] = i_rev[YSGL_12-j-1][i];};
	};
	return;
}

void generate_ysfont16x24(const char input, int i_font[24][16]){
	int i_input;
	int i_rev[24][16];
	int iskip = 8;
	int i, j;
	
	i_input = (int)input;
	/*printf("ID: %d HEX ID: %x Char: %c\n",i_input, i_input, i_input);*/
	for (j = 0; j < YSGL_24; j++) {
		uchar_2binarray(YsFont16x24[i_input][iskip*j], i_rev[j]);
	};
	for (j = 0; j < YSGL_24; j++) {
		for(i=0;i<XSGL_16;i++) {i_font[j][i] = i_rev[YSGL_24-j-1][i];};
	};
	return;
}

void generate_ysfont8x12_c(const char input, int i_font[12][8]){
	generate_ysfont8x12(input, i_font);
	return;
}

void generate_ysfont16x24_c(const char input, int i_font[24][16]){
	generate_ysfont16x24(input, i_font);
	return;
}
