
/* icosahedron_c.c */

#include "icosahedron_c.h"

static int ifac_poi[20][3];
static double xyz_ico[12][3];

void init_icosahedron_c(){
	int icosa_connect[60]
		= { 1,  3,  2,     1,  4,  3,     1,  5,  4,    1,  6,  5,
			1,  2,  6,     2,  3, 11,     3, 12, 11,    3,  4, 12,
			4,  8, 12,     4,  5,  8,     5,  9,  8,    5,  6,  9,
			6, 10,  9,     6,  2, 10,     2, 11, 10,    7,  8,  9,
			7,  9, 10,     7, 10, 11,     7, 11, 12,    7, 12,  8};
	
	double radius = ONE;
	double pi;
	double coef, ac, r, b;
	double theta[5];
	
	int i;
	
	pi = FOUR * atan(ONE);
	coef = sin(pi*0.2) * sin(pi*0.2);
	ac = radius*radius * (1.0 - 2.0*coef);
	r = 0.5 * (radius - sqrt(radius*radius - 8.0*coef*ac)) / (2.0*coef);
	b = sqrt (radius*radius-r*r);
	
	theta[0] = 0.5 * pi;
	theta[1] = theta[0] + pi*0.4;
	theta[2] = theta[1] + pi*0.4;
	theta[3] = theta[2] + pi*0.4;
	theta[4] = theta[3] + pi*0.4;
	
	xyz_ico[0][0] = 0.0;
	xyz_ico[0][1] = 0.0;
	xyz_ico[0][2] = radius;
	
	for (i = 0; i < 5; i++) {
        xyz_ico[i+1][0]=  b * cos(theta[i]);
        xyz_ico[i+1][1]=  b * sin(theta[i]);
        xyz_ico[i+1][2]=  r;
	};
	
	xyz_ico[6][0] = 0.0;
	xyz_ico[6][1] = 0.0;
	xyz_ico[6][2] = -radius;
	
	
	theta[0] = -0.5 * pi;
	theta[1] = theta[0] + pi*0.4;
	theta[2] = theta[1] + pi*0.4;
	theta[3] = theta[2] + pi*0.4;
	theta[4] = theta[3] + pi*0.4;
	
	for (i = 0; i < 5; i++) {
        xyz_ico[i+7][0]=  b * cos(theta[i]);
        xyz_ico[i+7][1]=  b * sin(theta[i]);
        xyz_ico[i+7][2]= -r;
	};
	
	/* connectivity */
	
	for (i = 0; i < 20; i++) {
		ifac_poi[i][0] = icosa_connect[3*i  ] - 1;
		ifac_poi[i][1] = icosa_connect[3*i+1] - 1;
		ifac_poi[i][2] = icosa_connect[3*i+2] - 1;
	};
	return;
};


int set_icosahedron_patch(double size, double x_draw[3], 
						  double *xyz_draw, double *norm_draw){
	int icou;
	GLfloat xyz_plot[12][3];
	int i, j, nd;
	int ie1, ie2, ie3;
	
	for (i = 0; i < 12; i++) {
		xyz_plot[i][0]=  x_draw[0] + (GLfloat) xyz_ico[i][0] * size;
		xyz_plot[i][1]=  x_draw[1] + (GLfloat) xyz_ico[i][1] * size;
		xyz_plot[i][2]=  x_draw[2] + (GLfloat) xyz_ico[i][2] * size;
	};
	
	/*! add a points to the display list */
	icou = 0;
	for (i = 0; i < 20; i++) {
		ie1 = ifac_poi[i][0];
		ie2 = ifac_poi[i][1];
		ie3 = ifac_poi[i][2];
		
		for (j = 0; j < 3; j++) {
			for (nd = 0; nd < 3; nd++) {
				ie1 = ifac_poi[i][j];
				xyz_draw[3*icou+nd] = (GLfloat) xyz_plot[ie1][nd];
				norm_draw[3*icou+nd] =  xyz_ico[ie1][nd];
			};
			icou = icou + 1;
		};
	};
	return icou;
}

static void set_circle_of_tube(int ncorner, float radius, float xx_line[3], float norm_nod[3], 
					  float dir_nod[3], float *xx_wall, float *norm_wall) {
	int k, nd;
	float norm_2nd[3], angle, len, len2, pi;
	
	pi = FOUR * atan(ONE);
	norm_2nd[0] = dir_nod[1] * norm_nod[2]
				- dir_nod[2] * norm_nod[1];
	norm_2nd[1] = dir_nod[2] * norm_nod[0]
				- dir_nod[0] * norm_nod[2];
	norm_2nd[2] = dir_nod[0] * norm_nod[1]
				- dir_nod[1] * norm_nod[0];
	len = sqrt(norm_2nd[0]*norm_2nd[0] +norm_2nd[1]*norm_2nd[1]+norm_2nd[2]*norm_2nd[2]);
	len2 = sqrt(norm_nod[0]*norm_nod[0] +norm_nod[1]*norm_nod[1]+norm_nod[2]*norm_nod[2]);
	for (nd=0; nd<3; nd++){ 
		norm_2nd[nd] = norm_2nd[nd]/len;
		norm_nod[nd] = norm_nod[nd]/len2;
	};		
	for(k=0;k<ncorner;k++){
		angle = TWO * pi * (float)k / (float)ncorner;
		for (nd=0; nd<3; nd++) {
			norm_wall[3*k+nd] = norm_nod[nd] * cos(angle)
							  + norm_2nd[nd] * sin(angle);
			xx_wall[3*k+nd] =   xx_line[nd] + radius*norm_wall[3*k+nd];
		};
	};
	return;
}

int set_tube_vertex(int ncorner, float radius, float x_line[6], float dir_line[6],
					float color_line[8], float *xyz, float *nor, float *col) {
	float norm_line[6];
	float xx_w1[3*ncorner], norm_w1[3*ncorner];
	float xx_w2[3*ncorner], norm_w2[3*ncorner];
	int npatch_wall = 0;
	int k, nd;
	
	for(k=0;k<2;k++){
		if(fabs(dir_line[3*k+2]) < 1.0e-40){
			norm_line[3*k  ] =  0.0;
			norm_line[3*k+1] =  0.0;
			norm_line[3*k+2] =  1.0;
		} else {
			norm_line[3*k  ] =  -dir_line[3*k+2];
			norm_line[3*k+1] =  dir_line[3*k+2];
			norm_line[3*k+2] =  dir_line[3*k  ]*dir_line[3*k+2]
					- dir_line[3*k+1]*dir_line[3*k+2];
		};
	};
	
	set_circle_of_tube(ncorner, radius, &x_line[0], &norm_line[0], &dir_line[0],
					   xx_w1, norm_w1);
	set_circle_of_tube(ncorner, radius, &x_line[3], &norm_line[3], &dir_line[3],
					   xx_w2, norm_w2);
	
	for(k=0;k<ncorner-1;k++){
		for (nd=0; nd<3; nd++) {
			xyz[3*(6*k)+  nd] = xx_w1[3*k+  nd];
			xyz[3*(6*k+1)+nd] = xx_w1[3*k+3+nd];
			xyz[3*(6*k+2)+nd] = xx_w2[3*k+3+nd];
			nor[3*(6*k)+  nd] = norm_w1[3*k+  nd];
			nor[3*(6*k+1)+nd] = norm_w1[3*k+3+nd];
			nor[3*(6*k+2)+nd] = norm_w2[3*k+3+nd];
			
			xyz[3*(6*k+3)+nd] = xx_w2[3*k+3+nd];
			xyz[3*(6*k+4)+nd] = xx_w2[3*k+nd];
			xyz[3*(6*k+5)+nd] = xx_w1[3*k+nd];
			nor[3*(6*k+3)+nd] = norm_w2[3*k+3+nd];
			nor[3*(6*k+4)+nd] = norm_w2[3*k+nd];
			nor[3*(6*k+5)+nd] = norm_w1[3*k+nd];
		};
	};
	
	for (nd=0; nd<3; nd++) {
		xyz[3*(6*(ncorner-1))+  nd] = xx_w1[3*(ncorner-1)+nd];
		xyz[3*(6*(ncorner-1)+1)+nd] = xx_w1[nd];
		xyz[3*(6*(ncorner-1)+2)+nd] = xx_w2[nd];
		nor[3*(6*(ncorner-1))+  nd] = norm_w1[3*(ncorner-1)+nd];
		nor[3*(6*(ncorner-1)+1)+nd] = norm_w1[nd];
		nor[3*(6*(ncorner-1)+2)+nd] = norm_w2[nd];
		
		xyz[3*(6*(ncorner-1)+3)+nd] = xx_w2[nd];
		xyz[3*(6*(ncorner-1)+4)+nd] = xx_w2[3*(ncorner-1)+nd];
		xyz[3*(6*(ncorner-1)+5)+nd] = xx_w1[3*(ncorner-1)+nd];
		nor[3*(6*(ncorner-1)+3)+nd] = norm_w2[nd];
		nor[3*(6*(ncorner-1)+4)+nd] = norm_w2[3*(ncorner-1)+nd];
		nor[3*(6*(ncorner-1)+5)+nd] = norm_w1[3*(ncorner-1)+nd];
	};
	for(k=0;k<ncorner;k++){
		for (nd=0; nd<3; nd++) {
			col[4*(6*k)+   nd] = color_line[  nd];
			col[4*(6*k+1)+nd] = color_line[  nd];
			col[4*(6*k+2)+nd] = color_line[4+nd];
			col[4*(6*k+3)+nd] = color_line[4+nd];
			col[4*(6*k+4)+nd] = color_line[4+nd];
			col[4*(6*k+5)+nd] = color_line[  nd];
		};
	};
	
	npatch_wall = 2*ncorner;
	return npatch_wall;
}

int set_cone_vertex(int ncorner, float radius, float x_line[6], float dir_line[6],
                    float color_line[8], float *xyz, float *nor, float *col){
    float norm_line[6];
    float xx_w1[3*ncorner], norm_w1[3*ncorner];
    int num = 0;
    int k, nd;
    
    for(k=0;k<2;k++){
        norm_line[3*k ] =  -dir_line[3*k+2];
        norm_line[3*k+1] =  dir_line[3*k+2];
        norm_line[3*k+2] =  dir_line[3*k  ]*dir_line[3*k+2]
        - dir_line[3*k+1]*dir_line[3*k+2];
    };
    
    set_circle_of_tube(ncorner, radius, &x_line[0], &norm_line[0], &dir_line[0],
                       xx_w1, norm_w1);
    
    for(k=0;k<ncorner-1;k++){
        for (nd=0; nd<3; nd++) {
            xyz[3*(3*k)+  nd] = xx_w1[3*k+  nd];
            xyz[3*(3*k+1)+nd] = xx_w1[3*k+3+nd];
            xyz[3*(3*k+2)+nd] = x_line[nd+3];
            nor[3*(3*k)+  nd] = norm_w1[3*k+  nd];
            nor[3*(3*k+1)+nd] = norm_w1[3*k+3+nd];
            nor[3*(3*k+2)+nd] = 0.5 * (norm_w1[3*k+  nd] + norm_w1[3*k+3+nd]);
        };
    };
    
    for (nd=0; nd<3; nd++) {
        xyz[3*(3*(ncorner-1))+  nd] = xx_w1[3*(ncorner-1)+nd];
        xyz[3*(3*(ncorner-1)+1)+nd] = xx_w1[nd];
        xyz[3*(3*(ncorner-1)+2)+nd] = x_line[nd+3];
        nor[3*(3*(ncorner-1))+  nd] = norm_w1[3*(ncorner-1)+nd];
        nor[3*(3*(ncorner-1)+1)+nd] = norm_w1[nd];
        nor[3*(3*(ncorner-1)+2)+nd] = 0.5 * (norm_w1[3*(ncorner-1)+nd] + norm_w1[nd]);
    };
    for(k=0;k<ncorner;k++){
        for (nd=0; nd<3; nd++) {
            col[4*(3*k)+   nd] = color_line[  nd];
            col[4*(3*k+1)+nd] = color_line[  nd];
            col[4*(3*k+2)+nd] = color_line[4+nd];
        };
    };
    
    num = ncorner;
    return num;
}


int set_tube_strided_buffer(int ncorner, float radius, float x_line[6], float dir_line[6],
					float color_line[8], int ist_buf, struct gl_strided_buffer *strided_buf) {
	float norm_line[6];
	float xx_w1[3*ncorner], norm_w1[3*ncorner];
	float xx_w2[3*ncorner], norm_w2[3*ncorner];
	int npatch_wall = 0;
	int k, nd;
	
	for(k=0;k<2;k++){
		if(fabs(dir_line[3*k+2]) < 1.0e-40){
			norm_line[3*k  ] =  0.0;
			norm_line[3*k+1] =  0.0;
			norm_line[3*k+2] =  1.0;
		} else {
			norm_line[3*k  ] =  -dir_line[3*k+2];
			norm_line[3*k+1] =  dir_line[3*k+2];
			norm_line[3*k+2] =  dir_line[3*k  ]*dir_line[3*k+2]
					- dir_line[3*k+1]*dir_line[3*k+2];
		};
	};
	
//	printf("x_line1 %f %f %f \n", x_line[0], x_line[1], x_line[2]);
//	printf("norm_line1 %f %f %f \n", norm_line[0], norm_line[1], norm_line[2]);
//	printf("dir_line1 %f %f %f \n", dir_line[0], dir_line[1], dir_line[2]);
	set_circle_of_tube(ncorner, radius, &x_line[0], &norm_line[0], &dir_line[0],
					   xx_w1, norm_w1);
	set_circle_of_tube(ncorner, radius, &x_line[3], &norm_line[3], &dir_line[3],
					   xx_w2, norm_w2);
	
	for(k=0;k<ncorner-1;k++){
		set_node_stride_VBO((ist_buf+6*k), strided_buf);
		for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = xx_w1[3*k+  nd];};
		for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = norm_w1[3*k+  nd];};
		for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = color_line[  nd];}
//		printf("%d %f %f %f \n", k, xx_w1[3*k+0], xx_w1[3*k+1], xx_w1[3*k+2]);
		
		set_node_stride_VBO((ist_buf+6*k+1), strided_buf);
		for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = xx_w1[3*k+3+nd];};
		for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = norm_w1[3*k+3+nd];};
		for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = color_line[  nd];}
		
		set_node_stride_VBO((ist_buf+6*k+2), strided_buf);
		for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = xx_w2[3*k+3+nd];};
		for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = norm_w2[3*k+3+nd];};
		for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = color_line[4+nd];}
			
		set_node_stride_VBO((ist_buf+6*k+3), strided_buf);
		for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = xx_w2[3*k+3+nd];};
		for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = norm_w2[3*k+3+nd];};
		for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = color_line[4+nd];}
		
		set_node_stride_VBO((ist_buf+6*k+4), strided_buf);
		for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = xx_w2[3*k+nd];};
		for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = norm_w2[3*k+nd];};
		for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = color_line[4+nd];}
		
		set_node_stride_VBO((ist_buf+6*k+5), strided_buf);
		for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = xx_w1[3*k+nd];};
		for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = norm_w1[3*k+nd];};
		for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = color_line[  nd];}
	};
	
	k = ncorner-1;
	set_node_stride_VBO((ist_buf+6*k), strided_buf);
	for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = xx_w1[3*k+nd];};
	for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = norm_w1[3*k+nd];};
	for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = color_line[  nd];}
	
	set_node_stride_VBO((ist_buf+6*k+1), strided_buf);
	for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = xx_w1[nd];};
	for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = norm_w1[nd];};
	for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = color_line[  nd];}
	
	set_node_stride_VBO((ist_buf+6*k+2), strided_buf);
	for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = xx_w2[nd];};
	for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = norm_w2[nd];};
	for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = color_line[4+nd];}
		
	set_node_stride_VBO((ist_buf+6*k+3), strided_buf);
	for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = xx_w2[nd];};
	for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = norm_w2[nd];};
	for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = color_line[4+nd];}
	
	set_node_stride_VBO((ist_buf+6*k+4), strided_buf);
	for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = xx_w2[3*k+nd];};
	for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = norm_w2[3*k+nd];};
	for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = color_line[4+nd];}
	
	set_node_stride_VBO((ist_buf+6*k+5), strided_buf);
	for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = xx_w1[3*k+nd];};
	for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = norm_w1[3*k+nd];};
	for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = color_line[  nd];}
	
	npatch_wall = 2*ncorner;
	return npatch_wall;
}

