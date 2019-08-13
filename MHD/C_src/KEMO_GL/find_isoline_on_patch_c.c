
/* find_isoline_on_patch_c.c */


#include "find_isoline_on_patch_c.h"


static void interpolate_line_c(double *xmid, const double *x1, const double *x2,
			const double vt, const double v1, const double v2){
	
	xmid[0] = ((v2-vt)*x1[0] + (vt-v1)*x2[0]) / (v2-v1);
	xmid[1] = ((v2-vt)*x1[1] + (vt-v1)*x2[1]) / (v2-v1);
	xmid[2] = ((v2-vt)*x1[2] + (vt-v1)*x2[2]) / (v2-v1);
	return;
}

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
			x_line[0] = xx_tri[3*i2  ];
			x_line[1] = xx_tri[3*i2+1];
			x_line[2] = xx_tri[3*i2+2];
			x_line[3] = xx_tri[3*i3  ];
			x_line[4] = xx_tri[3*i3+1];
			x_line[5] = xx_tri[3*i3+2];
			
			idraw = 1;
		}
		else if ( (sig[0]==ZERO) && (sig[1]==ZERO) && (sig[2]<ZERO) ){
			x_line[0] = xx_tri[3*i3  ];
			x_line[1] = xx_tri[3*i3+1];
			x_line[2] = xx_tri[3*i3+2];
			
			interpolate_line_c(&x_line[3], &xx_tri[3*i1], &xx_tri[3*i2],
					v_line, d_tri[i1], d_tri[i2]);
			
			idraw = 1;
		}
		else if ( (sig[0]<ZERO) && (sig[2]<ZERO) ){
			interpolate_line_c(&x_line[0], &xx_tri[3*i1], &xx_tri[3*i2],
					v_line, d_tri[i1], d_tri[i2]);
			interpolate_line_c(&x_line[3], &xx_tri[3*i2], &xx_tri[3*i3],
					v_line, d_tri[i2], d_tri[i3]);
			idraw = 1;
		}
	}
	
	return idraw;
};

int find_isoribbon_on_patch_c(double *x_ribbon, const double width, 
			const double *xx_tri, const double *d_tri, const double v_line) {
	double x_line[6];
	double normal[3], vec_l[3], ext[3];
	double size;
	int nd, i1, i2, i3;
	
	int idraw = find_isoline_on_patch_c(x_line, xx_tri, d_tri, v_line);
	if(idraw == 0) return idraw;
	
	cal_normal_4_triangle_c(&xx_tri[0], &xx_tri[3], &xx_tri[6], normal);
	for(nd=0; nd<3; nd++) {vec_l[nd] = x_line[nd+3] - x_line[nd];};
	
	ext[0] = normal[1]*vec_l[2] - normal[2]*vec_l[1];
	ext[1] = normal[2]*vec_l[0] - normal[0]*vec_l[2];
	ext[2] = normal[0]*vec_l[1] - normal[1]*vec_l[0];
	size = sqrt(ext[0]*ext[0] + ext[1]*ext[1] + ext[2]*ext[2]);
	ext[0] = ext[0] * 0.5 * width / size;
	ext[1] = ext[1] * 0.5 * width / size;
	ext[2] = ext[2] * 0.5 * width / size;
	
	for(nd=0; nd<3; nd++){
		x_ribbon[   nd] = x_line[  nd] - ext[nd];
		x_ribbon[ 3+nd] = x_line[  nd] + ext[nd];
		x_ribbon[ 6+nd] = x_line[3+nd] + ext[nd];
	
		x_ribbon[ 9+nd] = x_line[3+nd] + ext[nd];
		x_ribbon[12+nd] = x_line[3+nd] - ext[nd];
		x_ribbon[15+nd] = x_line[  nd] - ext[nd];
	};
	
	return idraw;
};
