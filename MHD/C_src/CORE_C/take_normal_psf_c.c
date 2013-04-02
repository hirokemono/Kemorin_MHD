
/* take_normal_psf_c.c */

#include <math.h>
#include <stdio.h>

#include "take_normal_psf_c.h"


static void take_normal_ele_psf(struct psf_data *viz_s){
	int i, i1, i2, i3;
	
	for (i = 0; i < viz_s->nele_viz; i++){
		i1 = viz_s->ie_viz[i][0] - 1;
		i2 = viz_s->ie_viz[i][1] - 1;
		i3 = viz_s->ie_viz[i][2] - 1;
		if(i1 <0 || i1 >= viz_s->nnod_viz) printf("i1 fault %d %d %d\n", i, i1, viz_s->nnod_viz);
		if(i2 <0 || i2 >= viz_s->nnod_viz) printf("i2 fault %d %d %d\n", i, i2, viz_s->nnod_viz);
		if(i3 <0 || i3 >= viz_s->nnod_viz) printf("i3 fault %d %d %d\n", i, i3, viz_s->nnod_viz);
		cal_center_4_triangle_c(viz_s->xx_viz[i1], viz_s->xx_viz[i2], viz_s->xx_viz[i3],
								viz_s->x_ele_viz[i]);
		viz_s->area_viz[i] = cal_normal_4_triangle_c(viz_s->xx_viz[i1], viz_s->xx_viz[i2],
													 viz_s->xx_viz[i3], viz_s->norm_ele[i]);
	};

	viz_s->area_total = 0.0;
	for (i = 0; i < viz_s->nele_viz; i++){
		viz_s->area_total = viz_s->area_total + viz_s->area_viz[i];
	};
	return;
};


static void take_normal_nod_psf(struct psf_data *viz_s){
	int n, i, i1, i2, i3;
	double d, xe[3], d2h[3];
	
	for (i = 0; i < viz_s->nele_viz; i++){
		i1 = viz_s->ie_viz[i][0] - 1;
		i2 = viz_s->ie_viz[i][1] - 1;
		i3 = viz_s->ie_viz[i][2] - 1;
		
		xe[0] = ( viz_s->xx_viz[i1][0] + viz_s->xx_viz[i2][0] + viz_s->xx_viz[i3][0] ) / 3.0;
		xe[1] = ( viz_s->xx_viz[i1][1] + viz_s->xx_viz[i2][1] + viz_s->xx_viz[i3][1] ) / 3.0;
		xe[2] = ( viz_s->xx_viz[i1][2] + viz_s->xx_viz[i2][2] + viz_s->xx_viz[i3][2] ) / 3.0;
		
		d2h[0] = sqrt( (xe[0] - viz_s->xx_viz[i1][0])*(xe[0] - viz_s->xx_viz[i1][0])
				     + (xe[1] - viz_s->xx_viz[i1][1])*(xe[1] - viz_s->xx_viz[i1][1])
				     + (xe[2] - viz_s->xx_viz[i1][2])*(xe[2] - viz_s->xx_viz[i1][2]) );
		d2h[1] = sqrt( (xe[0] - viz_s->xx_viz[i2][0])*(xe[0] - viz_s->xx_viz[i2][0])
				     + (xe[1] - viz_s->xx_viz[i2][1])*(xe[1] - viz_s->xx_viz[i2][1])
				     + (xe[2] - viz_s->xx_viz[i2][2])*(xe[2] - viz_s->xx_viz[i2][2]) );
		d2h[2] = sqrt( (xe[0] - viz_s->xx_viz[i3][0])*(xe[0] - viz_s->xx_viz[i3][0])
				     + (xe[1] - viz_s->xx_viz[i3][1])*(xe[1] - viz_s->xx_viz[i3][1])
				     + (xe[2] - viz_s->xx_viz[i3][2])*(xe[2] - viz_s->xx_viz[i3][2]) );
		
		if ( d2h[0] == 0.0 ) {
			viz_s->norm_nod[i1][0] = viz_s->norm_ele[i][0];
			viz_s->norm_nod[i1][1] = viz_s->norm_ele[i][1];
			viz_s->norm_nod[i1][2] = viz_s->norm_ele[i][2];
			}
		else {
			viz_s->norm_nod[i1][0] = viz_s->norm_nod[i1][0]
					+ viz_s->norm_ele[i][0] * (d2h[1]+d2h[2]);
			viz_s->norm_nod[i1][1] = viz_s->norm_nod[i1][1]
					+ viz_s->norm_ele[i][1] * (d2h[1]+d2h[2]);
			viz_s->norm_nod[i1][2] = viz_s->norm_nod[i1][2]
					+ viz_s->norm_ele[i][2] * (d2h[1]+d2h[2]);
		};
		
		if ( d2h[1] == 0.0 ) {
			viz_s->norm_nod[i2][0] = viz_s->norm_ele[i][0];
			viz_s->norm_nod[i2][1] = viz_s->norm_ele[i][1];
			viz_s->norm_nod[i2][2] = viz_s->norm_ele[i][2];
		}
		else {
			viz_s->norm_nod[i2][0] = viz_s->norm_nod[i2][0]
					+ viz_s->norm_ele[i][0] * (d2h[0]+d2h[2]);
			viz_s->norm_nod[i2][1] = viz_s->norm_nod[i2][1] 
					+ viz_s->norm_ele[i][1] * (d2h[0]+d2h[2]);
			viz_s->norm_nod[i2][2] = viz_s->norm_nod[i2][2]
					+ viz_s->norm_ele[i][2] * (d2h[0]+d2h[2]);
		};
		
		if ( d2h[2] == 0.0 ) {
			viz_s->norm_nod[i3][0] = viz_s->norm_ele[i][0];
			viz_s->norm_nod[i3][1] = viz_s->norm_ele[i][1];
			viz_s->norm_nod[i3][2] = viz_s->norm_ele[i][2]; 
		}
        else {
			viz_s->norm_nod[i3][0] = viz_s->norm_nod[i3][0]
					+ viz_s->norm_ele[i][0] * (d2h[0]+d2h[1]);
			viz_s->norm_nod[i3][1] = viz_s->norm_nod[i3][1]
					+ viz_s->norm_ele[i][1] * (d2h[0]+d2h[1]);
			viz_s->norm_nod[i3][2] = viz_s->norm_nod[i3][2]
					+ viz_s->norm_ele[i][2] * (d2h[0]+d2h[1]);
		}
		
	};
	
	for (n = 0; n < viz_s->nnod_viz; n++){
		d = sqrt( viz_s->norm_nod[n][0]*viz_s->norm_nod[n][0]
				+ viz_s->norm_nod[n][1]*viz_s->norm_nod[n][1]
				+ viz_s->norm_nod[n][2]*viz_s->norm_nod[n][2]);
		if(d == 0.0){
			viz_s->norm_nod[n][0] = 0.0;
			viz_s->norm_nod[n][1] = 0.0;
			viz_s->norm_nod[n][2] = 0.0;
		} else {
			d = 1.0 / d;
			viz_s->norm_nod[n][0] = viz_s->norm_nod[n][0] * d;
			viz_s->norm_nod[n][1] = viz_s->norm_nod[n][1] * d;
			viz_s->norm_nod[n][2] = viz_s->norm_nod[n][2] * d;
		}
	};
	
	return;
};

static void take_length_ele_fline(struct psf_data *viz_s){
	int i, i1, i2, nd;
	double len, len2;
	
	viz_s->area_total = 0.0;

	for (i = 0; i < viz_s->nnod_viz; i++){
		for (nd=0; nd<3; nd++) {viz_s->dir_nod[i][nd] = 0.0;};
	}
	
	for (i = 0; i < viz_s->nele_viz; i++){
		i1 = viz_s->ie_viz[i][0] - 1;
		i2 = viz_s->ie_viz[i][1] - 1;
		
		for (nd=0; nd<3; nd++) {
			viz_s->dir_ele[i][nd] = viz_s->xx_viz[i2][nd] - viz_s->xx_viz[i1][nd];
		}
		
		viz_s->norm_ele[i][0] = -viz_s->dir_ele[i][2];
		viz_s->norm_ele[i][1] =  viz_s->dir_ele[i][2];
		viz_s->norm_ele[i][2] =  viz_s->dir_ele[i][0]*viz_s->dir_ele[i][2]
							   - viz_s->dir_ele[i][1]*viz_s->dir_ele[i][2];

		viz_s->length_ele[i]
			= sqrt( viz_s->dir_ele[i][0]*viz_s->dir_ele[i][0]
				  + viz_s->dir_ele[i][1]*viz_s->dir_ele[i][1]
				  + viz_s->dir_ele[i][2]*viz_s->dir_ele[i][2] );
		
		if ( viz_s->length_ele[i] == 0.0){
			viz_s->dir_ele[i][0] = 0.0;
			viz_s->dir_ele[i][1] = 0.0;
			viz_s->dir_ele[i][2] = 0.0;
		}
		else{
			viz_s->dir_ele[i][0] 
				= viz_s->dir_ele[i][0] / viz_s->length_ele[i];
			viz_s->dir_ele[i][1]
				= viz_s->dir_ele[i][1] / viz_s->length_ele[i];
			viz_s->dir_ele[i][2]
				= viz_s->dir_ele[i][2] / viz_s->length_ele[i];
		}

		viz_s->length_total = viz_s->length_total + viz_s->length_ele[i];
		
		len = sqrt( viz_s->norm_ele[i][0]*viz_s->norm_ele[i][0]
				  + viz_s->norm_ele[i][1]*viz_s->norm_ele[i][1]
				  + viz_s->norm_ele[i][2]*viz_s->norm_ele[i][2] );

		for (nd=0; nd<3; nd++) {
			viz_s->x_ele_viz[i][nd] = (viz_s->xx_viz[i1][nd] + viz_s->xx_viz[i2][nd])*HALF;
			viz_s->norm_ele[i][nd] = viz_s->norm_ele[i][nd] / len;

			viz_s->dir_nod[i1][nd] = viz_s->dir_nod[i1][nd] + viz_s->dir_ele[i][nd];
			viz_s->dir_nod[i2][nd] = viz_s->dir_nod[i2][nd] + viz_s->dir_ele[i][nd];
		}		

	};

	for (i = 0; i < viz_s->nnod_viz; i++){
		viz_s->norm_nod[i][0] = -viz_s->dir_nod[i][2];
		viz_s->norm_nod[i][1] =  viz_s->dir_nod[i][2];
		viz_s->norm_nod[i][2] =  viz_s->dir_nod[i][0]*viz_s->dir_nod[i][2]
							   - viz_s->dir_nod[i][1]*viz_s->dir_nod[i][2];
		len= sqrt(  viz_s->norm_nod[i][0]*viz_s->norm_nod[i][0]
				  + viz_s->norm_nod[i][1]*viz_s->norm_nod[i][1]
				  + viz_s->norm_nod[i][2]*viz_s->norm_nod[i][2] );
		len2= sqrt( viz_s->dir_nod[i][0]*viz_s->dir_nod[i][0]
				  + viz_s->dir_nod[i][1]*viz_s->dir_nod[i][1]
				  + viz_s->dir_nod[i][2]*viz_s->dir_nod[i][2] );
		
		for (nd=0; nd<3; nd++) {
			viz_s->norm_nod[i][nd] = viz_s->norm_nod[i][nd] / len;
			viz_s->dir_nod[i][nd] =  viz_s->dir_nod[i][nd] / len2;
		}
	}
	
	return;
};



static void take_rms_ave_psf(struct psf_data *viz_s){
	int icomp, i, i1, i2, i3;
	double d;
	
	for (icomp = 0; icomp < viz_s->ncomptot; icomp++){
		viz_s->d_rms[icomp] = 0.0;
		viz_s->d_ave[icomp] = 0.0;
		for (i = 0; i < viz_s->nele_viz; i++){
			i1 = viz_s->ie_viz[i][0] - 1;
			i2 = viz_s->ie_viz[i][1] - 1;
			i3 = viz_s->ie_viz[i][2] - 1;
			d = ( viz_s->d_nod[i1][icomp] + viz_s->d_nod[i2][icomp]
					+ viz_s->d_nod[i3][icomp] ) / 3.0;
			
			viz_s->d_rms[icomp] = viz_s->d_rms[icomp]
					 + d * d * viz_s->area_viz[i];
			viz_s->d_ave[icomp] = viz_s->d_ave[icomp]
					 + d * viz_s->area_viz[i];
		}
		viz_s->d_rms[icomp] = sqrt(viz_s->d_rms[icomp] / viz_s->area_total );
		viz_s->d_ave[icomp] = viz_s->d_ave[icomp] / viz_s->area_total;
	}
	return;
}

static void take_rms_ave_fline(struct psf_data *viz_s){
	int icomp, i, i1, i2;
	double d;
	
	for (icomp = 0; icomp < viz_s->ncomptot; icomp++){
		viz_s->d_rms[icomp] = 0.0;
		viz_s->d_ave[icomp] = 0.0;
		for (i = 0; i < viz_s->nele_viz; i++){
			i1 = viz_s->ie_viz[i][0] - 1;
			i2 = viz_s->ie_viz[i][1] - 1;
			d = ( viz_s->d_nod[i1][icomp] + viz_s->d_nod[i2][icomp]) / 2.0;
			
			viz_s->d_rms[icomp] = viz_s->d_rms[icomp]
					 + d * d * viz_s->length_ele[i];
			viz_s->d_ave[icomp] = viz_s->d_ave[icomp]
					 + d * viz_s->length_ele[i];
		}
		viz_s->d_rms[icomp] = sqrt(viz_s->d_rms[icomp] / viz_s->length_total );
		viz_s->d_ave[icomp] = viz_s->d_ave[icomp] / viz_s->length_total;
	}
	return;
}

static void take_minmax_psf_fields(struct psf_data *viz_s){
	int ifld, icomp, n;
	
	for (ifld = 0; ifld < viz_s->nfield; ifld++){
		for (n = 0; n < viz_s->nnod_viz; n++) viz_s->d_amp[n][ifld] = 0.0;
		for (icomp = viz_s->istack_comp[ifld]; icomp < viz_s->istack_comp[ifld+1]; icomp++){
			for (n = 0; n < viz_s->nnod_viz; n++){
				viz_s->d_amp[n][ifld] = viz_s->d_amp[n][ifld]
						+ viz_s->d_nod[n][icomp] * viz_s->d_nod[n][icomp];
			};
		};
		for (n = 0; n < viz_s->nnod_viz; n++) viz_s->d_amp[n][ifld] = sqrt(viz_s->d_amp[n][ifld]);
	};
	
	for (icomp = 0; icomp < viz_s->ncomptot; icomp++){
		viz_s->d_min[icomp] = viz_s->d_nod[0][icomp];
		viz_s->d_max[icomp] = viz_s->d_nod[0][icomp];
		for (n = 1; n < viz_s->nnod_viz; n++){
			if ( viz_s->d_nod[n][icomp] < viz_s->d_min[icomp] )
			{
				viz_s->d_min[icomp] = viz_s->d_nod[n][icomp];
			};
			if ( viz_s->d_nod[n][icomp] > viz_s->d_max[icomp] )
			{
				viz_s->d_max[icomp] = viz_s->d_nod[n][icomp];
			};
		};
	}
	
	for (ifld = 0; ifld < viz_s->nfield; ifld++){
		viz_s->amp_min[ifld] = viz_s->d_nod[0][ifld];
		viz_s->amp_max[ifld] = viz_s->d_nod[0][ifld];
		for (n = 1; n < viz_s->nnod_viz; n++){
			if ( viz_s->d_nod[n][ifld] < viz_s->amp_min[ifld] )
			{
				viz_s->amp_min[ifld] = viz_s->d_nod[n][ifld];
			};
			if ( viz_s->d_nod[n][ifld] > viz_s->amp_max[ifld] )
			{
				viz_s->amp_max[ifld] = viz_s->d_nod[n][ifld];
			};
		};
	};
	
	return;
}

void take_normal_psf(struct psf_data *viz_s){
	alloc_psf_norm_s(viz_s);
	take_normal_ele_psf(viz_s);
	take_normal_nod_psf(viz_s);
	return;
}

void take_length_fline(struct psf_data *viz_s){
	alloc_psf_norm_s(viz_s);
	alloc_psf_length_s(viz_s);
	take_length_ele_fline(viz_s);
	return;
}

void take_minmax_psf(struct psf_data *viz_s){
	take_rms_ave_psf(viz_s);
	take_minmax_psf_fields(viz_s);
	return;
}

void take_minmax_fline(struct psf_data *viz_s){
	take_rms_ave_fline(viz_s);
	take_minmax_psf_fields(viz_s);
	return;
}
