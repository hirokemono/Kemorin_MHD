
/* draw_menu_bottun_gl.c */
#include <OpenGL/gl3.h>
#include "draw_menu_bottun_gl.h"

static void dtob(int BitSize, int x, int *c) {
	int bit = 1, i;
	
	for (i = 0; i < BitSize; i++) {
		if (x & bit){
		c[i] = 1;
		}else{
		c[i] = 0;
		}
		bit <<= 1;
	}
	return;
}

static void uchar2bin(int BitSize, unsigned char xchr, int *c) {
	int bit = 1, i;
	int x;
	
	x = (int) xchr;
	for (i = 0; i < BitSize; i++) {
		if (x & bit){ c[i] = 1;}
		else        { c[i] = 0;}
		bit <<= 1;
	}
	return;
}

static void menubottun_bitmap(GLubyte *menu_bitmap){
	int Bitlen, bitmap8[8], bitmap32[8*4];
	unsigned char intbit;
	unsigned char MenuFONT8x12_M[48];
	unsigned char MenuFONT8x12_E[48];
	unsigned char MenuFONT8x12_N[48];
	unsigned char MenuFONT8x12_U[48];
	unsigned char *MenuFONT8x12[4] = {MenuFONT8x12_M,MenuFONT8x12_E,MenuFONT8x12_N,MenuFONT8x12_U};
	
	int i, j, k, r, index = 0;
	
	for(i = 0 ; i < MENU_HEIGHT ; i++) {
		r = (i * 0xFF) / MENU_HEIGHT;
		for(j = 0 ; j < MENU_WIDTH ; j++) {
			index = 3*(j + i*MENU_WIDTH);
			menu_bitmap[index  ] = r;
			menu_bitmap[index+1] = (( j * 0xFF ) / MENU_WIDTH);
			menu_bitmap[index+2] = ~r;
		}
	}
	for(i = 0 ; i < MENU_HEIGHT ; i++) {
		for(j = 0 ; j < 3 ; j++) {
			index = 3*(MENU_WIDTH-j-1 + i*MENU_WIDTH);
			for(k=0;k<3;k++) {
				if(menu_bitmap[index+k] < 50*(3-j)){
					menu_bitmap[index+k] = 0;
				} else {
					menu_bitmap[index+k] = menu_bitmap[index+k] - 50*(3-j);
				}
			}
			index = 3*(j + i*MENU_WIDTH);
			for(k=0;k<3;k++) {
				if(menu_bitmap[index+k] < 50*(3-j)){
					menu_bitmap[index+k] = 0;
				} else {
					menu_bitmap[index+k] = menu_bitmap[index+k] - 50*(3-j);
				}
			}
		}
	}
	for(i = 0 ; i < 3 ; i++) {
		for(j = 0 ; j < MENU_WIDTH ; j++) {
			index = 3*(j + i*MENU_WIDTH);
			for(k=0;k<3;k++) {
				if(menu_bitmap[index+k] < 50*(3-i)){
					menu_bitmap[index+k] = 0;
				} else {
					menu_bitmap[index+k] = menu_bitmap[index+k] - 50*(3-i);
				}
			}
			
			index = 3*(j + (MENU_HEIGHT-i-1)*MENU_WIDTH);
			for(k=0;k<3;k++) {
				if(menu_bitmap[index+k] < 50*(3-i)){
					menu_bitmap[index+k] = 0;
				} else {
					menu_bitmap[index+k] = menu_bitmap[index+k] - 50*(3-i);
				}
			}
		}
	}
	
	for(i=0;i<48;i++) {
		MenuFONT8x12_M[i] = YsFont8x12[77][i];
		MenuFONT8x12_E[i] = YsFont8x12[69][i];
		MenuFONT8x12_N[i] = YsFont8x12[78][i];
		MenuFONT8x12_U[i] = YsFont8x12[85][i];
	}
	
	Bitlen = 8*sizeof(unsigned char);
	for(i=0;i<12;i++) {
		for(j=0;j<4;j++) {
			intbit = MenuFONT8x12[j][4*i  ];
			uchar2bin(Bitlen, intbit, bitmap8);
			for(k=0;k<8;k++) bitmap32[7-k+j*Bitlen] = bitmap8[k];
		}
		/*
		for(j=0;j<32;j++) printf("%d",bitmap32[j]);
		printf("\n");
		*/
		for(j=0;j<32;j++){
			if(bitmap32[j]!=0){
				index = 3*((int) (1.5*(float) j+ 9) + (int) (1.5*(float) i+ 6)*MENU_WIDTH);
				for(k=0;k<3;k++) menu_bitmap[index+k] = 0;
				index = 3*((int) (1.5*(float) j+ 10) + (int) (1.5*(float) i+ 6)*MENU_WIDTH);
				for(k=0;k<3;k++) menu_bitmap[index+k] = 0;
				index = 3*((int) (1.5*(float) j+ 9) + (int) (1.5*(float) i+ 7)*MENU_WIDTH);
				for(k=0;k<3;k++) menu_bitmap[index+k] = 0;
				index = 3*((int) (1.5*(float) j+ 10) + (int) (1.5*(float) i+ 7)*MENU_WIDTH);
				for(k=0;k<3;k++) menu_bitmap[index+k] = 0;
			}
		}
		
	}
	
	return;
}

void draw_menubottun_gl(struct VAO_ids *VAO_menu){
	GLubyte menubottun_bits[3*MENU_HEIGHT*MENU_WIDTH];
	
	menubottun_bitmap(menubottun_bits);
	kemoview_indentity_projectionmatrix();
	
	glClearColor(0.3, 0.9, 0.9, 1.0);
	glClear(GL_COLOR_BUFFER_BIT |GL_DEPTH_BUFFER_BIT);
	glRasterPos2i(-1 , -1);
	glDrawPixels(MENU_WIDTH , MENU_HEIGHT , GL_RGB , GL_UNSIGNED_BYTE , menubottun_bits);
	
	glFlush();
	return;
}

void VBO_for_Menu(struct VAO_ids *VAO_menu){
	GLubyte menubottun_bits[3*MENU_HEIGHT*MENU_WIDTH];
	GLfloat Vertices[4*MENU_WIDTH*MENU_HEIGHT];
	GLfloat Colors[4*MENU_WIDTH*MENU_HEIGHT];
	int i, j, idx;
	
	menubottun_bitmap(menubottun_bits);
	for(j=0;j<MENU_HEIGHT;j++){
		for(i=0;i<MENU_WIDTH;i++){
			idx = i + j * MENU_WIDTH;
			Vertices[4*idx  ] =  2.0*((float) i / (float) MENU_WIDTH) - 1.0;
			Vertices[4*idx+1] =  2.0*((float) j / (float) MENU_HEIGHT)- 1.0;
			Vertices[4*idx+2] = 0.0;
			Vertices[4*idx+3] = 1.0;
			
			Colors[4*idx  ] =  (float) ((int) menubottun_bits[3*idx  ]) / 256.0;
			Colors[4*idx+1] =  (float) ((int) menubottun_bits[3*idx+1]) / 256.0;
			Colors[4*idx+2] =  (float) ((int) menubottun_bits[3*idx+2]) / 256.0;
			Colors[4*idx+3] = 1.0;
		}
	}
	GLenum ErrorCheckValue = glGetError();
	
	glGenVertexArrays(1, &VAO_menu->id_VAO);
	glBindVertexArray(VAO_menu->id_VAO);
	
	glGenBuffers(1, &VAO_menu->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, VAO_menu->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(Vertices), Vertices, GL_STATIC_DRAW);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, 0);
	glEnableVertexAttribArray(0);
	
	glGenBuffers(1, &VAO_menu->id_color);
	glBindBuffer(GL_ARRAY_BUFFER, VAO_menu->id_color);
	glBufferData(GL_ARRAY_BUFFER, sizeof(Colors), Colors, GL_STATIC_DRAW);
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, 0, 0);
	glEnableVertexAttribArray(1);
	
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

