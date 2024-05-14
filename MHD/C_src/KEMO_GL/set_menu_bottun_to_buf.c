
/* set_menu_bottun_to_buf.c */
#include "set_menu_bottun_to_buf.h"

/*
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
*/

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

void menubottun_bitmap(unsigned char *menu_bitmap){
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

static long count_menu_to_buf(void){
	return (MENU_HEIGHT * MENU_WIDTH);
};

static long set_menu_to_buf(unsigned char *menubottun_bits,
                            struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
	long i, j, idx, icou;
	for(j=0;j<MENU_HEIGHT;j++){
		for(i=0;i<MENU_WIDTH;i++){
			idx = i + j * MENU_WIDTH;
            set_node_stride_buffer(idx, strided_buf, &point_buf);
            
			strided_buf->v_buf[point_buf.igl_xyzw  ]
                =  2.0*((float) i / (float) MENU_WIDTH) - 1.0;
			strided_buf->v_buf[point_buf.igl_xyzw+1]
                =  2.0*((float) j / (float) MENU_HEIGHT)- 1.0;
			strided_buf->v_buf[point_buf.igl_xyzw+2]
                = 0.0;
            strided_buf->v_buf[point_buf.igl_xyzw+3] = 1.0;
			
			strided_buf->v_buf[point_buf.igl_color  ]
                =  (float) ((int) menubottun_bits[3*idx  ]) / 256.0;
			strided_buf->v_buf[point_buf.igl_color+1]
                =  (float) ((int) menubottun_bits[3*idx+1]) / 256.0;
			strided_buf->v_buf[point_buf.igl_color+2]
                =  (float) ((int) menubottun_bits[3*idx+2]) / 256.0;
			strided_buf->v_buf[point_buf.igl_color+3] = 1.0;
		}
	}
	icou = MENU_HEIGHT * MENU_WIDTH;
	return icou;
};

void const_menu_bottun_buffer(struct gl_strided_buffer *strided_buf){
    unsigned char menubottun_bits[3*MENU_HEIGHT*MENU_WIDTH];
	long num_dot = count_menu_to_buf();
	
	set_buffer_address_4_patch(num_dot, strided_buf);
	resize_strided_buffer(strided_buf);
	
	menubottun_bitmap(menubottun_bits);
	set_menu_to_buf(menubottun_bits, strided_buf);
	return;
};
