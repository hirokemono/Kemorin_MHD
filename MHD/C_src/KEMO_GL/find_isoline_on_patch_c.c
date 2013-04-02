
/* find_isoline_on_patch_c.c */


#include "find_isoline_on_patch_c.h"


static void interpolate_line_c(double *xmid, double *x1, double *x2,
			double vt, double v1, double v2){
	
	xmid[0] = ((v2-vt)*x1[0] + (vt-v1)*x2[0]) / (v2-v1);
	xmid[1] = ((v2-vt)*x1[1] + (vt-v1)*x2[1]) / (v2-v1);
	xmid[2] = ((v2-vt)*x1[2] + (vt-v1)*x2[2]) / (v2-v1);
	return;
}

int find_isoline_on_patch_c(double *x_line, 
			double *xx_tri, double *d_tri, double v_line) {
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
