
/* find_isoline_on_patch_c.c */


#include "find_isoline_on_patch_c.h"


static void interpolate_line_c(double *xmid, const double *x1, const double *x2,
			const double vt, const double v1, const double v2){
	
	xmid[0] = ((v2-vt)*x1[0] + (vt-v1)*x2[0]) / (v2-v1);
	xmid[1] = ((v2-vt)*x1[1] + (vt-v1)*x2[1]) / (v2-v1);
	xmid[2] = ((v2-vt)*x1[2] + (vt-v1)*x2[2]) / (v2-v1);
	return;
}

static void tangential_vector_of_line_c(double *tanv, const double width, 
			const double *x_line, const double *normal) {
	double vec_l[3];
	double size;
	
	vec_l[0] = x_line[3] - x_line[0];
	vec_l[1] = x_line[4] - x_line[1];
	vec_l[2] = x_line[5] - x_line[2];
	
	tanv[0] = normal[1]*vec_l[2] - normal[2]*vec_l[1];
	tanv[1] = normal[2]*vec_l[0] - normal[0]*vec_l[2];
	tanv[2] = normal[0]*vec_l[1] - normal[1]*vec_l[0];
	
	size = sqrt(tanv[0]*tanv[0] + tanv[1]*tanv[1] + tanv[2]*tanv[2]);
	if(fabs(size) < 1.0e-40){
		tanv[0] = 0.0;
		tanv[1] = 0.0;
		tanv[2] = width;
	} else {
		tanv[0] = tanv[0] * 0.5 * width / size;
		tanv[1] = tanv[1] * 0.5 * width / size;
		tanv[2] = tanv[2] * 0.5 * width / size;
	};
	return;
};

int find_isoline_on_patch_c(double *x_line, 
			const double *xx_tri, const double *d_tri, const double v_line) {
	int idraw;
	double sig[3];
	int nd, i1, i2, i3;
	
	idraw = 0;
	
	for (nd = 0; nd < 3; nd++) {
		i1 = (nd  )%3;
		i2 = (nd+1)%3;
		i3 = (nd+2)%3;
		
		sig[0] = ( d_tri[i2] - v_line ) * ( d_tri[i3] - v_line );
		sig[1] = ( d_tri[i3] - v_line ) * ( d_tri[i1] - v_line );
		sig[2] = ( d_tri[i1] - v_line ) * ( d_tri[i2] - v_line );
		
		if ( (sig[0]==ZERO) && (sig[1]==ZERO) && (sig[2]==ZERO) ){
			for(nd=0; nd<3; nd++){x_line[  nd] = xx_tri[3*i2+nd];};
			for(nd=0; nd<3; nd++){x_line[3+nd] = xx_tri[3*i3+nd];};
			
			idraw = 1;
			break;
		}
		else if ( (sig[0]==ZERO) && (sig[1]==ZERO) && (sig[2]<ZERO) ){
			for(nd=0; nd<3; nd++){x_line[  nd] = xx_tri[3*i3+nd];};
			interpolate_line_c(&x_line[3], &xx_tri[3*i1], &xx_tri[3*i2],
					v_line, d_tri[i1], d_tri[i2]);
			
			idraw = 1;
			break;
		}
		else if ( (sig[0]<ZERO) && (sig[2]<ZERO) ){
			interpolate_line_c(&x_line[0], &xx_tri[3*i1], &xx_tri[3*i2],
					v_line, d_tri[i1], d_tri[i2]);
			interpolate_line_c(&x_line[3], &xx_tri[3*i2], &xx_tri[3*i3],
					v_line, d_tri[i2], d_tri[i3]);
			idraw = 1;
			break;
		}
	}
	
	return idraw;
};

int find_isoribbon_on_patch_c(double *x_ribbon, const double width, 
			const double *xx_tri, const double *d_tri, const double v_line) {
	double x_line[6];
	double normal[3], tanv[3];
	double ext1[3], ext2[3];
	double edge1[3], edge2[3];
	double d_prod;
	
	int idraw;
	double sig[3];
	int nd, i1, i2, i3;
	
	idraw = 0;
	
	for (nd = 0; nd < 3; nd++) {
		i1 = (nd  )%3;
		i2 = (nd+1)%3;
		i3 = (nd+2)%3;
		
		sig[0] = ( d_tri[i2] - v_line ) * ( d_tri[i3] - v_line );
		sig[1] = ( d_tri[i3] - v_line ) * ( d_tri[i1] - v_line );
		sig[2] = ( d_tri[i1] - v_line ) * ( d_tri[i2] - v_line );
		
		if ( (sig[0]==ZERO) && (sig[1]==ZERO) && (sig[2]==ZERO) ){
			for(nd=0; nd<3; nd++){x_line[  nd] = xx_tri[3*i2+nd];};
			for(nd=0; nd<3; nd++){x_line[3+nd] = xx_tri[3*i3+nd];};
			
			cal_normal_4_triangle_c(&xx_tri[0], &xx_tri[3], &xx_tri[6], normal);
			tangential_vector_of_line_c(tanv, width, x_line, normal);
			
			/*
			for(nd=0; nd<3; nd++) {ext1[nd] = tanv[nd];};
			for(nd=0; nd<3; nd++) {ext2[nd] = tanv[nd];};
			*/
			
			idraw = 1;
			break;
		}
		else if ( (sig[0]==ZERO) && (sig[1]==ZERO) && (sig[2]<ZERO) ){
			for(nd=0; nd<3; nd++){x_line[nd] = xx_tri[3*i3+nd];};
			for(nd=0; nd<3; nd++) {edge1[nd] = xx_tri[3*i2+nd] - xx_tri[3*i1+nd];};
			interpolate_line_c(&x_line[3], &xx_tri[3*i1], &xx_tri[3*i2],
					v_line, d_tri[i1], d_tri[i2]);
			
			cal_normal_4_triangle_c(&xx_tri[0], &xx_tri[3], &xx_tri[6], normal);
			tangential_vector_of_line_c(tanv, width, x_line, normal);
			
			/*
			for(nd=0; nd<3; nd++) {ext1[nd] = tanv[nd];};
			for(nd=0; nd<3; nd++) {edge2[nd] = xx_tri[3*i2+nd] - xx_tri[3*i1+nd];};
			twist_ribbon_corner(ext2, tanv, edge2);
			*/
			
			idraw = 1;
			break;
		}
		else if ( (sig[0]<ZERO) && (sig[2]<ZERO) ){
			interpolate_line_c(&x_line[0], &xx_tri[3*i1], &xx_tri[3*i2],
					v_line, d_tri[i1], d_tri[i2]);
			interpolate_line_c(&x_line[3], &xx_tri[3*i2], &xx_tri[3*i3],
					v_line, d_tri[i2], d_tri[i3]);
			
			cal_normal_4_triangle_c(&xx_tri[0], &xx_tri[3], &xx_tri[6], normal);
			tangential_vector_of_line_c(tanv, width, x_line, normal);
			
			/*
			for(nd=0; nd<3; nd++) {edge1[nd] = xx_tri[3*i2+nd] - xx_tri[3*i1+nd];};
			for(nd=0; nd<3; nd++) {edge2[nd] = xx_tri[3*i3+nd] - xx_tri[3*i2+nd];};
			twist_ribbon_corner(ext1, tanv, edge1);
			twist_ribbon_corner(ext2, tanv, edge2);
			*/
			
			idraw = 1;
			break;
		}
	}
	if(idraw == 0) return idraw;
	
	for(nd=0; nd<3; nd++){
		x_ribbon[   nd] = x_line[  nd] - tanv[nd];
		x_ribbon[ 3+nd] = x_line[  nd] + tanv[nd];
		x_ribbon[ 6+nd] = x_line[3+nd] + tanv[nd];
	
		x_ribbon[ 9+nd] = x_line[3+nd] + tanv[nd];
		x_ribbon[12+nd] = x_line[3+nd] - tanv[nd];
		x_ribbon[15+nd] = x_line[  nd] - tanv[nd];
	};
	
	if(v_line < 0.0){
		for(nd=0; nd<3; nd++){
			x_ribbon[ 9+nd] = 0.5 * (x_ribbon[3+nd] + x_ribbon[ 6+nd]);
			x_ribbon[12+nd] = 0.5 * (x_ribbon[  nd] + x_ribbon[12+nd]);
			x_ribbon[ 6+nd] = x_ribbon[ 9+nd];
		};
	};
	
	return idraw;
};
