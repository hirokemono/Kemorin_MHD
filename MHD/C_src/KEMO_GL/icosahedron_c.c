
/* icosahedron_c.c */

#include "icosahedron_c.h"

static int ifac_poi[20][3];
static double xyzw_ico[12][4];

const int nnod_ico = 12;
const int ntri_ico = 20;

void init_icosahedron_c(void){
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
	
    xyzw_ico[0][0] = 0.0;
    xyzw_ico[0][1] = 0.0;
    xyzw_ico[0][2] = radius;
    xyzw_ico[0][3] = 1.0;
	
	for (i = 0; i < 5; i++) {
        xyzw_ico[i+1][0]=  b * cos(theta[i]);
        xyzw_ico[i+1][1]=  b * sin(theta[i]);
        xyzw_ico[i+1][2]=  r;
        xyzw_ico[i+1][3]=  1.0;
	};
	
    xyzw_ico[6][0] = 0.0;
    xyzw_ico[6][1] = 0.0;
    xyzw_ico[6][2] = -radius;
    xyzw_ico[6][3] = 1.0;
	
	
	theta[0] = -0.5 * pi;
	theta[1] = theta[0] + pi*0.4;
	theta[2] = theta[1] + pi*0.4;
	theta[3] = theta[2] + pi*0.4;
	theta[4] = theta[3] + pi*0.4;
	
	for (i = 0; i < 5; i++) {
        xyzw_ico[i+7][0]=  b * cos(theta[i]);
        xyzw_ico[i+7][1]=  b * sin(theta[i]);
        xyzw_ico[i+7][2]= -r;
        xyzw_ico[i+7][3]= 1.0;
	};
	
	/* connectivity */
	
	for (i = 0; i < 20; i++) {
		ifac_poi[i][0] = icosa_connect[3*i  ] - 1;
		ifac_poi[i][1] = icosa_connect[3*i+1] - 1;
		ifac_poi[i][2] = icosa_connect[3*i+2] - 1;
	};
	return;
};

int num_icosahedron_node(void){return  nnod_ico;};
int num_icosahedron_patch(void){return ntri_ico;};
long set_icosahedron_node_index(double size, double x_draw[3],
                                double xyzw_draw[48], double norm_draw[48],
                                unsigned int ie_ico[60]){
    int i;
    
    for(i = 0; i < 12; i++) {
        xyzw_draw[4*i  ]=  x_draw[0] + xyzw_ico[i][0] * size;
        xyzw_draw[4*i+1]=  x_draw[1] + xyzw_ico[i][1] * size;
        xyzw_draw[4*i+2]=  x_draw[2] + xyzw_ico[i][2] * size;
        xyzw_draw[4*i+3]=  xyzw_ico[i][3];
        norm_draw[4*i  ]=  xyzw_ico[i][0];
        norm_draw[4*i+1]=  xyzw_ico[i][1];
        norm_draw[4*i+2]=  xyzw_ico[i][2];
        norm_draw[4*i+3]=  xyzw_ico[i][3];
    };
    
/* add a points to the display list */
    for(i = 0; i < 20; i++){
        ie_ico[3*i  ] = ifac_poi[i][0];
        ie_ico[3*i+1] = ifac_poi[i][1];
        ie_ico[3*i+2] = ifac_poi[i][2];
    };
    return 1;
}


static void set_circle_of_tube(int ncorner, double radius, double xx_line[3], double norm_nod[3],
                               double dir_nod[3], double *xyzw_wall, double *norm_wall) {
	int k, nd;
	double norm_2nd[3], angle, len, nrm1, nrm2, r_mod, pi;
	
	pi = FOUR * atan(ONE);
	norm_2nd[0] = dir_nod[1] * norm_nod[2]
				- dir_nod[2] * norm_nod[1];
	norm_2nd[1] = dir_nod[2] * norm_nod[0]
				- dir_nod[0] * norm_nod[2];
	norm_2nd[2] = dir_nod[0] * norm_nod[1]
				- dir_nod[1] * norm_nod[0];
	len =  sqrt(dir_nod[0]*dir_nod[0] + dir_nod[1]*dir_nod[1] + dir_nod[2]*dir_nod[2]);
	nrm2 = sqrt(norm_2nd[0]*norm_2nd[0] + norm_2nd[1]*norm_2nd[1] + norm_2nd[2]*norm_2nd[2]);
	nrm1 = sqrt(norm_nod[0]*norm_nod[0] + norm_nod[1]*norm_nod[1] + norm_nod[2]*norm_nod[2]);
	r_mod =  (len*nrm1) / nrm2;
	for (nd=0; nd<3; nd++){ 
		norm_2nd[nd] = norm_2nd[nd] / nrm2;
		norm_nod[nd] = norm_nod[nd] / nrm1;
	};		
	for(k=0;k<ncorner;k++){
		angle = TWO * pi * (double)k / (double)ncorner;
		for (nd=0; nd<3; nd++) {
			norm_wall[4*k+nd] = norm_nod[nd] * cos(angle)
					          + norm_2nd[nd] * sin(angle);
            xyzw_wall[4*k+nd] =   xx_line[nd] + radius*norm_wall[4*k+nd];
		};
        norm_wall[4*k+3] = 1.0;
        xyzw_wall[4*k+3] = 1.0;
	};
	return;
}

void find_normal_on_line(double norm_line[4],
                         const double dir_line[4]){
	norm_line[0] = -dir_line[2];
	norm_line[1] =  dir_line[2];
	norm_line[2] =  dir_line[0] - dir_line[1];
    norm_line[3] = 1.0;
	return;
};


int num_tube_node(int ncorner){return  (2 * (ncorner+1));};
int num_tube_patch(int ncorner){return (4 * ncorner);};
int set_tube_node_index(int ncorner, double radius,
                        double xyzw_line[8], double dir_line[8],
                        double norm_line[8], double color_line[8],
                        double *xyzw, double *norm, double *col,
                        unsigned int *ie_tube){
    double xyzw_w1[4*ncorner], norm_w1[4*ncorner];
    double xyzw_w2[4*ncorner], norm_w2[4*ncorner];
    int k, nd;
    
    set_circle_of_tube(ncorner, radius, &xyzw_line[0],
                       &norm_line[0], &dir_line[0],
                       xyzw_w1, norm_w1);
    set_circle_of_tube(ncorner, radius, &xyzw_line[4],
                       &norm_line[4], &dir_line[4],
                       xyzw_w2, norm_w2);
    
    for(nd=0; nd<4; nd++){xyzw[  nd] = xyzw_line[  nd];}
    for(nd=0; nd<4; nd++){norm[  nd] = dir_line[   nd];}
    for(nd=0; nd<4; nd++){col[  nd] = color_line[  nd];}
    
    for(k=0;k<ncorner;k++){
        for(nd=0; nd<4; nd++){xyzw[4*(k+1)+nd] = xyzw_w1[4*k+ nd];}
        for(nd=0; nd<4; nd++){norm[4*(k+1)+nd] = norm_w1[4*k+ nd];}
        for(nd=0; nd<4; nd++){col[4*(k+1)+nd] = color_line[nd];};
    };
    
    for(k=0;k<ncorner;k++){
        for(nd=0; nd<4; nd++){xyzw[4*(ncorner+k+1)+nd] = xyzw_w2[4*k+ nd];}
        for(nd=0; nd<4; nd++){norm[4*(ncorner+k+1)+nd] = norm_w2[4*k+ nd];}
        for(nd=0; nd<4; nd++){col[4*(ncorner+k+1)+nd] = color_line[4+nd];};
    };
    
    for(nd=0; nd<4; nd++){xyzw[4*(2*ncorner+1)+nd] = xyzw_line[4+nd];}
    for(nd=0; nd<4; nd++){norm[4*(2*ncorner+1)+nd] = dir_line[ 4+nd];}
    for(nd=0; nd<4; nd++){col[4*(2*ncorner+1)+nd] = color_line[4+nd];}

    for(k=0;k<ncorner-1;k++){
        ie_tube[6*k  ] = k+1;
        ie_tube[6*k+1] = k+2;
        ie_tube[6*k+2] = k+2 + ncorner;

        ie_tube[6*k+3] = k+2 + ncorner;
        ie_tube[6*k+4] = k+1 + ncorner;
        ie_tube[6*k+5] = k+1;
    }
    ie_tube[6*(ncorner-1)  ] = (ncorner-1)+1;
    ie_tube[6*(ncorner-1)+1] = 1;
    ie_tube[6*(ncorner-1)+2] = 1 + ncorner;

    ie_tube[6*(ncorner-1)+3] = 1 + ncorner;
    ie_tube[6*(ncorner-1)+4] = ncorner + ncorner;
    ie_tube[6*(ncorner-1)+5] = (ncorner-1)+1;

    for(k=0;k<ncorner-1;k++){
        ie_tube[6*(k+ncorner)  ] = 0;
        ie_tube[6*(k+ncorner)+1] = k+2;
        ie_tube[6*(k+ncorner)+2] = k+1;
        ie_tube[6*(k+ncorner)+3] = 2*ncorner+1;
        ie_tube[6*(k+ncorner)+4] = k+1 + (ncorner+1);
        ie_tube[6*(k+ncorner)+5] = k+2 + (ncorner+1);
    }
    ie_tube[6*(2*ncorner-1)  ] = 0;
    ie_tube[6*(2*ncorner-1)+1] = 1;
    ie_tube[6*(2*ncorner-1)+2] = ncorner;
    ie_tube[6*(2*ncorner-1)+3] = 2*ncorner+1;
    ie_tube[6*(2*ncorner-1)+4] = ncorner + (ncorner+1);
    ie_tube[6*(2*ncorner-1)+5] = 1 + (ncorner+1);
    
    return (4 * ncorner);
}


int num_cone_node(int ncorner){return  (ncorner + 2);};
int num_cone_patch(int ncorner){return (2*ncorner);};
int set_cone_node_index(int ncorner, double radius,
                        double xyzw_line[8], double dir_line[8],
                        double norm_line[8], double color_line[8],
                        double *xyzw, double *norm, double *col,
                        unsigned int *ie_cone){
    double xyzw_w1[4*ncorner], norm_w1[4*ncorner];
    int k, nd;
    
    set_circle_of_tube(ncorner, radius,
                       &xyzw_line[0], &norm_line[0], &dir_line[0],
                       xyzw_w1, norm_w1);
    
/*  Center of arrow bottom */
    for(nd=0; nd<4; nd++){xyzw[  nd] = xyzw_line[  nd];}
    for(nd=0; nd<4; nd++){norm[  nd] = dir_line[   nd];}
    for(nd=0; nd<4; nd++){col[  nd] = color_line[  nd];}
/*  circle of arrow bottom */
    for(k=0;k<ncorner;k++){
        for(nd=0; nd<4; nd++){xyzw[4*(k+1)+nd] = xyzw_w1[4*k+ nd];};
        for(nd=0; nd<4; nd++){norm[4*(k+1)+nd] = norm_w1[4*k+ nd];};
        for(nd=0; nd<4; nd++){col[4*(k+1)+nd] =  color_line[nd];};
    };
/*  Arrow head */
    for(nd=0; nd<4; nd++){xyzw[4*(ncorner+1)+nd] = xyzw_line[4+nd];}
    for(nd=0; nd<4; nd++){norm[4*(ncorner+1)+nd] = dir_line[ 4+nd];}
    for(nd=0; nd<4; nd++){col[4*(ncorner+1)+nd] = color_line[4+nd];}

    for(k=0;k<ncorner-1;k++){
        ie_cone[3*k  ] = ncorner+1;
        ie_cone[3*k+1] = k+1;
        ie_cone[3*k+2] = k+2;
    };
    ie_cone[3*(ncorner-1)  ] = ncorner+1;
    ie_cone[3*(ncorner-1)+1] = ncorner;
    ie_cone[3*(ncorner-1)+2] = 1;

    for(k=0;k<ncorner-1;k++){
        ie_cone[3*(k+ncorner)  ] = 0;
        ie_cone[3*(k+ncorner)+1] = k+2;
        ie_cone[3*(k+ncorner)+2] = k+1;
    };
    ie_cone[3*(2*ncorner-1)  ] = 0;
    ie_cone[3*(2*ncorner-1)+1] = 1;
    ie_cone[3*(2*ncorner-1)+2] = ncorner;
    
    return (2 * ncorner);
}


int num_arrow_node(int ncorner){
    return (num_tube_node(ncorner) + num_cone_node(ncorner));
};
int num_arrow_patch(int ncorner){
    return (num_tube_patch(ncorner) + num_cone_patch(ncorner));
};
int set_arrow_node_index(int ncorner, double radius,
                         double xyzw_line[8], double dir_line[8],
                         double norm_line[8], double color_line[8],
                         double *xyzw, double *norm, double *col,
                         unsigned int *ie_arrow){
    double ratio_tube = 0.5;
    double ratio_cone = 0.3;
    
    double xyzw_arrow[12];
    double color_arrow[12];
    double norm_arrow[12];
    int nd;
    for(nd=0; nd<4; nd++){xyzw_arrow[  nd] =  xyzw_line[  nd];}
    for(nd=0; nd<4; nd++){color_arrow[  nd] = color_line[  nd];}
    for(nd=0; nd<4; nd++){norm_arrow[  nd] =  norm_line[  nd];}
    for(nd=0; nd<4; nd++){
        xyzw_arrow[  nd] = ratio_cone *         xyzw_line[  nd]
                         + (1.0 - ratio_cone) * xyzw_line[4+nd];
    }
    for(nd=0; nd<4; nd++){
        color_arrow[  nd] = ratio_cone *         color_line[  nd]
                          + (1.0 - ratio_cone) * color_line[4+nd];
    }
    for(nd=0; nd<4; nd++){
        norm_arrow[  nd] = ratio_cone *         norm_line[  nd]
                         + (1.0 - ratio_cone) * norm_line[4+nd];
    }
    for(nd=0; nd<4; nd++){xyzw_arrow[8+nd] =  xyzw_line[4+nd];}
    for(nd=0; nd<4; nd++){color_arrow[8+nd] = color_line[4+nd];}
    for(nd=0; nd<4; nd++){norm_arrow[8+nd] =  norm_line[4+nd];}
    
    set_tube_node_index(ncorner, radius,
                        &xyzw_arrow[0], dir_line,
                        &norm_arrow[0], &color_arrow[0],
                        &xyzw[0], &norm[0], &col[0],
                        &ie_arrow[0]);
    int n_node =  2*(ncorner + 1);
    int n_patch = 4*ncorner;
    set_cone_node_index(ncorner, radius,
                        &xyzw_arrow[4], dir_line,
                        &norm_arrow[4], &color_arrow[4],
                        &xyzw[n_node], &norm[n_node], &col[n_node],
                        &ie_arrow[n_patch]);
    
    for(nd=0; nd<(2*ncorner); nd++){
        ie_arrow[nd+n_patch] = ie_arrow[nd+n_patch] + n_node;
    }
    return (6*ncorner);
}
