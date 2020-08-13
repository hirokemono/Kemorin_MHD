
/* find_isoline_on_patch_c.c */


#include "find_isoline_on_patch_c.h"


static void interpolate_line_c(double xmid[3], double xexit[3], 
			const double normal[3], const double x1[3], const double x2[3],
			const double vt, const double v1, const double v2){
	double ledge, lexit;
	double xedge[3];
	int nd;
	for(nd=0;nd<3;nd++){
		xmid[nd] =  ((v2-vt)*x1[nd] + (vt-v1)*x2[nd]) / (v2-v1);
		xedge[nd] = (x2[nd] - x1[nd]);
	}
	xexit[0] = xedge[1]*normal[2] - xedge[2]*normal[1];
	xexit[1] = xedge[2]*normal[0] - xedge[0]*normal[2];
	xexit[2] = xedge[0]*normal[1] - xedge[1]*normal[0];
	
	ledge = sqrt(xedge[0]*xedge[0] + xedge[1]*xedge[1] + xedge[2]*xedge[2]);
	lexit = sqrt(xexit[0]*xexit[0] + xexit[1]*xexit[1] + xexit[2]*xexit[2]);
	for(nd=0;nd<3;nd++){
		xedge[nd] = xedge[nd] / ledge;
		xexit[nd] = xexit[nd] / lexit;
	}
	
	return;
}

static void line_direction_at_corner(double xexit[3],
			const double x1[3], const double x2[3]){
	double lexit;
	int nd;
	for(nd=0;nd<3;nd++){xexit[nd] = x2[nd] - x1[nd];};
	lexit = sqrt(xexit[0]*xexit[0] + xexit[1]*xexit[1] + xexit[2]*xexit[2]);
//	for(nd=0;nd<3;nd++){xexit[nd] = xexit[nd] / lexit;};
	return;
}

int mark_isoline_on_patch_c(const double *d_tri, const double v_line){
	
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
		
		if ( (sig[0]==ZERO) && (sig[1]==ZERO) && (sig[2]<ZERO) ){
			idraw = 1;
			break;
		}
		else if ( (sig[0]<ZERO) && (sig[2]<ZERO) ){
			idraw = 1;
			break;
		}
	}
	return idraw;
};

int find_isoline_on_patch_c(double x_line[6], double dir_line[6], double norm_line[6], 
			const double *xx_tri, const double *d_tri, const double v_line){
	
	int idraw;
	double sig[3], ref_dir[3];
	double dot;
	int k1, nd, i1, i2, i3;
	
	idraw = 0;
	
	for (k1 = 0; k1 < 3; k1++) {
		i1 = (k1  )%3;
		i2 = (k1+1)%3;
		i3 = (k1+2)%3;
		
		sig[0] = ( d_tri[i2] - v_line ) * ( d_tri[i3] - v_line );
		sig[1] = ( d_tri[i3] - v_line ) * ( d_tri[i1] - v_line );
		sig[2] = ( d_tri[i1] - v_line ) * ( d_tri[i2] - v_line );
		
		if ( (sig[0]==ZERO) && (sig[1]==ZERO) && (sig[2]<ZERO) ){
			line_direction_at_corner(ref_dir, &x_line[3], &x_line[0]);
			cal_normal_4_triangle_c(&xx_tri[0], &xx_tri[3], &xx_tri[6], &norm_line[0]);
			for(nd=0; nd<3; nd++){norm_line[3+nd] = norm_line[nd];};
			
			for(nd=0; nd<3; nd++){x_line[nd] = xx_tri[3*i3+nd];};
			line_direction_at_corner(&dir_line[0], &x_line[3], &x_line[0]);
			
			interpolate_line_c(&x_line[3], &dir_line[3], 
						&norm_line[3], &xx_tri[3*i1], &xx_tri[3*i2],
						v_line, d_tri[i1], d_tri[i2]);
			
			dot = (ref_dir[0]*dir_line[3] + ref_dir[1]*dir_line[4] + ref_dir[2]*dir_line[5]);
			if(dot < 0.0){
				for(nd=0; nd<3; nd++){dir_line[3+nd] = -dir_line[3+nd];};
			}
			
			idraw = 1;
			break;
		}
		else if ( (sig[0]<ZERO) && (sig[2]<ZERO) ){
			line_direction_at_corner(ref_dir, &x_line[3], &x_line[0]);
			cal_normal_4_triangle_c(&xx_tri[0], &xx_tri[3], &xx_tri[6], &norm_line[0]);
			for(nd=0; nd<3; nd++){norm_line[3+nd] = norm_line[nd];};
			
			interpolate_line_c(&x_line[0], &dir_line[0], 
						&norm_line[0],  &xx_tri[3*i1], &xx_tri[3*i2], 
						v_line, d_tri[i1], d_tri[i2]);
			interpolate_line_c(&x_line[3], &dir_line[3], 
						&norm_line[3],  &xx_tri[3*i2], &xx_tri[3*i3], 
						v_line, d_tri[i2], d_tri[i3]);
			
			dot = (ref_dir[0]*dir_line[0] + ref_dir[1]*dir_line[1] + ref_dir[2]*dir_line[2]);
			if(dot < 0.0){
				for(nd=0; nd<3; nd++){dir_line[nd] = -dir_line[nd];};
			}
			dot = (ref_dir[0]*dir_line[3] + ref_dir[1]*dir_line[4] + ref_dir[2]*dir_line[5]);
			if(dot < 0.0){
				for(nd=0; nd<3; nd++){dir_line[3+nd] = -dir_line[3+nd];};
			}
			idraw = 1;
			break;
		}
	}
	if(idraw == 0) return idraw;
	
	if(v_line < 0.0){
		for(nd=0; nd<3; nd++){
			x_line[ 3+nd] =   0.5*(x_line[  nd] + x_line[3+nd]);
			dir_line[ 3+nd] = 0.5*(dir_line[  nd] + dir_line[3+nd]);
		};
	};
	
	return idraw;
};
