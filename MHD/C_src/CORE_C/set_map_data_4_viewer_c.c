
/*  set_map_data_4_viewer_c.c */

#include "set_map_data_4_viewer_c.h"


void count_map_patch_from_psf_c(struct psf_data *psf_s, struct psf_map_data *psf_map_s){
    int iele;
    long i1, i2, i3;
	double y1, y2, y3;
	
	psf_map_s->nnod_add_map = 0;
	psf_map_s->nele_add_map = 0;
	for (iele = 0; iele < psf_s->nele_viz; iele++){
		i1 = psf_s->ie_viz[iele][0];
		i2 = psf_s->ie_viz[iele][1];
		i3 = psf_s->ie_viz[iele][2];
		y1 = psf_s->xx_viz[i1-1][1];
		y2 = psf_s->xx_viz[i2-1][1];
		y3 = psf_s->xx_viz[i3-1][1];
		
		if     ( ((y1*y2) < ZERO) && ((y2*y3) < ZERO) ){
			psf_map_s->nnod_add_map = psf_map_s->nnod_add_map + 2;
			psf_map_s->nele_add_map = psf_map_s->nele_add_map + 2;
		}
		else if( ((y1*y2) < ZERO) && (y3 == ZERO) ){
			psf_map_s->nnod_add_map = psf_map_s->nnod_add_map + 1;
			psf_map_s->nele_add_map = psf_map_s->nele_add_map + 1;
		}
		else if( ((y1*y2) < ZERO) && ((y3*y1) < ZERO) ){
			psf_map_s->nnod_add_map = psf_map_s->nnod_add_map + 2;
			psf_map_s->nele_add_map = psf_map_s->nele_add_map + 2;
		}
		
		else if( ((y2*y3) < ZERO) && (y1 == ZERO) ){
			psf_map_s->nnod_add_map = psf_map_s->nnod_add_map + 1;
			psf_map_s->nele_add_map = psf_map_s->nele_add_map + 1;
		}
		else if( ((y2*y3) < ZERO) && ((y3*y1) < ZERO) ){
			psf_map_s->nnod_add_map = psf_map_s->nnod_add_map + 2;
			psf_map_s->nele_add_map = psf_map_s->nele_add_map + 2;
		}
		
		else if( ((y3*y1) < ZERO) && (y2 == ZERO) ){
			psf_map_s->nnod_add_map = psf_map_s->nnod_add_map + 1;
			psf_map_s->nele_add_map = psf_map_s->nele_add_map + 1;
		};
	};
	
	psf_map_s->nnod_map = psf_s->nnod_viz + psf_map_s->nnod_add_map;
	psf_map_s->nele_map = psf_s->nele_viz + psf_map_s->nele_add_map;
	psf_map_s->ncomptot_map = psf_s->ncomptot;
	
	return;
}


void set_map_patch_from_psf_c(struct psf_data *psf_s, struct psf_map_data *psf_map_s){
    int iele;
    long i1, i2, i3, i4, i5, je;
	int icou_n, icou_e;
	double y1, y2, y3;
	
	for (iele = 0; iele < psf_s->nele_viz; iele++){
		psf_map_s->ie_map[iele][0] = psf_s->ie_viz[iele][0];
		psf_map_s->ie_map[iele][1] = psf_s->ie_viz[iele][1];
		psf_map_s->ie_map[iele][2] = psf_s->ie_viz[iele][2];
	};
	
	icou_n = 0;
	icou_e = 0;
	for (iele = 0; iele < psf_s->nele_viz; iele++){
		i1 = psf_s->ie_viz[iele][0];
		i2 = psf_s->ie_viz[iele][1];
		i3 = psf_s->ie_viz[iele][2];
		y1 = psf_s->xx_viz[i1-1][1];
		y2 = psf_s->xx_viz[i2-1][1];
		y3 = psf_s->xx_viz[i3-1][1];
		
		if     ( ((y1*y2) < ZERO) && ((y2*y3) < ZERO) ){
			icou_n = icou_n + 2;
			icou_e = icou_e + 2;
			i4 = psf_s->nnod_viz + icou_n - 1;
			i5 = psf_s->nnod_viz + icou_n;
			
			je = psf_s->nele_viz + icou_e;
			psf_map_s->ie_map[je-2][0] = i1;
			psf_map_s->ie_map[je-2][1] = i4;
			psf_map_s->ie_map[je-2][2] = i5;
			
			psf_map_s->ie_map[je-1][0] = i1;
			psf_map_s->ie_map[je-1][1] = i5;
			psf_map_s->ie_map[je-1][2] = i3;
			
			psf_map_s->ie_map[iele][0] = i4;
			psf_map_s->ie_map[iele][1] = i2;
			psf_map_s->ie_map[iele][2] = i5;
			
			psf_map_s->inod_org_map[icou_n-2][0] = i1;
			psf_map_s->inod_org_map[icou_n-2][1] = i2;
			psf_map_s->inod_org_map[icou_n-1][0] = i2;
			psf_map_s->inod_org_map[icou_n-1][1] = i3;
			
			psf_map_s->coef_itp_map[icou_n-2][0] = y2;
			psf_map_s->coef_itp_map[icou_n-2][1] = y1;
			psf_map_s->coef_itp_map[icou_n-1][0] = y3;
			psf_map_s->coef_itp_map[icou_n-1][1] = y2;
			

		}
		else if( ((y1*y2) < ZERO) && (y3 == ZERO) ){
			icou_n = icou_n + 1;
			icou_e = icou_e + 1;
			i4 = psf_s->nnod_viz + icou_n;
			
			je = psf_s->nele_viz + icou_e;
			psf_map_s->ie_map[je-1][0] = i1;
			psf_map_s->ie_map[je-1][1] = i4;
			psf_map_s->ie_map[je-1][2] = i3;
			
			psf_map_s->ie_map[iele][0] = i4;
			psf_map_s->ie_map[iele][1] = i2;
			psf_map_s->ie_map[iele][2] = i3;
			
			psf_map_s->inod_org_map[icou_n-1][0] = i1;
			psf_map_s->inod_org_map[icou_n-1][1] = i2;
			
			psf_map_s->coef_itp_map[icou_n-1][0] = y2;
			psf_map_s->coef_itp_map[icou_n-1][1] = y1;
		}
		else if( ((y1*y2) < ZERO) && ((y3*y1) < ZERO) ){
			icou_n = icou_n + 2;
			icou_e = icou_e + 2;
			i4 = psf_s->nnod_viz + icou_n - 1;
			i5 = psf_s->nnod_viz + icou_n;
			
			je = psf_s->nele_viz + icou_e;
			psf_map_s->ie_map[je-2][0] = i5;
			psf_map_s->ie_map[je-2][1] = i2;
			psf_map_s->ie_map[je-2][2] = i3;
			
			psf_map_s->ie_map[je-1][0] = i5;
			psf_map_s->ie_map[je-1][1] = i3;
			psf_map_s->ie_map[je-1][2] = i4;
			
			psf_map_s->ie_map[iele][0] = i1;
			psf_map_s->ie_map[iele][1] = i5;
			psf_map_s->ie_map[iele][2] = i4;
			
			psf_map_s->inod_org_map[icou_n-2][0] = i1;
			psf_map_s->inod_org_map[icou_n-2][1] = i2;
			psf_map_s->inod_org_map[icou_n-1][0] = i3;
			psf_map_s->inod_org_map[icou_n-1][1] = i1;
			
			psf_map_s->coef_itp_map[icou_n-2][0] = y2;
			psf_map_s->coef_itp_map[icou_n-2][1] = y1;
			psf_map_s->coef_itp_map[icou_n-1][0] = y1;
			psf_map_s->coef_itp_map[icou_n-1][1] = y3;
		}
		
		else if( ((y2*y3) < ZERO) && (y1 == ZERO) ){
			icou_n = icou_n + 1;
			icou_e = icou_e + 1;
			i4 = psf_s->nnod_viz + icou_n;
			
			je = psf_s->nele_viz + icou_e;
			psf_map_s->ie_map[je-1][0] = i4;
			psf_map_s->ie_map[je-1][1] = i1;
			psf_map_s->ie_map[je-1][2] = i2;
			
			psf_map_s->ie_map[iele][0] = i1;
			psf_map_s->ie_map[iele][1] = i4;
			psf_map_s->ie_map[iele][2] = i3;
			
			psf_map_s->inod_org_map[icou_n-1][0] = i2;
			psf_map_s->inod_org_map[icou_n-1][1] = i3;
			
			psf_map_s->coef_itp_map[icou_n-1][0] = y3;
			psf_map_s->coef_itp_map[icou_n-1][1] = y2;
		}
		else if( ((y2*y3) < ZERO) && ((y3*y1) < ZERO) ){
			icou_n = icou_n + 2;
			icou_e = icou_e + 2;
			i4 = psf_s->nnod_viz + icou_n - 1;
			i5 = psf_s->nnod_viz + icou_n;
			
			je = psf_s->nele_viz + icou_e;
			psf_map_s->ie_map[je-2][0] = i1;
			psf_map_s->ie_map[je-2][1] = i2;
			psf_map_s->ie_map[je-2][2] = i4;
			
			psf_map_s->ie_map[je-1][0] = i1;
			psf_map_s->ie_map[je-1][1] = i4;
			psf_map_s->ie_map[je-1][2] = i5;
			
			psf_map_s->ie_map[iele][0] = i5;
			psf_map_s->ie_map[iele][1] = i4;
			psf_map_s->ie_map[iele][2] = i3;
			
			psf_map_s->inod_org_map[icou_n-2][0] = i2;
			psf_map_s->inod_org_map[icou_n-2][1] = i3;
			psf_map_s->inod_org_map[icou_n-1][0] = i3;
			psf_map_s->inod_org_map[icou_n-1][1] = i1;
			
			psf_map_s->coef_itp_map[icou_n-2][0] = y3;
			psf_map_s->coef_itp_map[icou_n-2][1] = y2;
			psf_map_s->coef_itp_map[icou_n-1][0] = y1;
			psf_map_s->coef_itp_map[icou_n-1][1] = y3;
		}
		
		else if( ((y3*y1) < ZERO) && (y2 == ZERO) ){
			icou_n = icou_n + 1;
			icou_e = icou_e + 1;
			i4 = psf_s->nnod_viz + icou_n;
			
			je = psf_s->nele_viz + icou_e;
			psf_map_s->ie_map[je-1][0] = i1;
			psf_map_s->ie_map[je-1][1] = i2;
			psf_map_s->ie_map[je-1][2] = i4;
			
			psf_map_s->ie_map[iele][0] = i4;
			psf_map_s->ie_map[iele][1] = i2;
			psf_map_s->ie_map[iele][2] = i3;
			
			psf_map_s->inod_org_map[icou_n-1][0] = i3;
			psf_map_s->inod_org_map[icou_n-1][1] = i1;
			
			psf_map_s->coef_itp_map[icou_n-1][0] = y1;
			psf_map_s->coef_itp_map[icou_n-1][1] = y3;
		};
		
	};
	
	
	return;
}


void set_map_grid_from_psf_c(struct psf_data *psf_s, struct psf_map_data *psf_map_s){
	long inod, icou, i1, i2;
	double y1, y2;
	
	for (inod = 0; inod < psf_s->nnod_viz; inod++){
		psf_map_s->inod_map[inod] = psf_s->inod_viz[inod];
		psf_map_s->xx_map[inod][0] = psf_s->xx_viz[inod][0];
		psf_map_s->xx_map[inod][1] = psf_s->xx_viz[inod][1];
		psf_map_s->xx_map[inod][2] = psf_s->xx_viz[inod][2];
	};
	for (icou = 0; icou < psf_map_s->nnod_add_map; icou++){
		inod = icou + psf_s->nnod_viz;
		i1 = psf_map_s->inod_org_map[icou][0];
		i2 = psf_map_s->inod_org_map[icou][1];
		y1 = psf_map_s->coef_itp_map[icou][0];
		y2 = psf_map_s->coef_itp_map[icou][1];
		psf_map_s->inod_map[inod] = inod+1;
		psf_map_s->xx_map[inod][0] = (y1*psf_s->xx_viz[i1-1][0] - y2*psf_s->xx_viz[i2-1][0]) / (y1-y2);
		psf_map_s->xx_map[inod][1] = ZERO;
		psf_map_s->xx_map[inod][2] = (y1*psf_s->xx_viz[i1-1][2] - y2*psf_s->xx_viz[i2-1][2]) / (y1-y2);
	};
	
	return;
}

void set_map_data_from_psf_c(struct psf_data *psf_s, struct psf_map_data *psf_map_s){
	long inod, icou, j, i1, i2;
	double y1, y2;
	
	for (inod = 0; inod < psf_s->nnod_viz; inod++){
		for (j = 0; j < psf_s->ncomptot; j++){
			psf_map_s->d_nod_map[inod][j] = psf_s->d_nod[inod][j];
		};
	};
	for (icou = 0; icou < psf_map_s->nnod_add_map; icou++){
		inod = icou + psf_s->nnod_viz;
		i1 = psf_map_s->inod_org_map[icou][0];
		i2 = psf_map_s->inod_org_map[icou][1];
		y1 = psf_map_s->coef_itp_map[icou][0];
		y2 = psf_map_s->coef_itp_map[icou][1];
		for (j = 0; j < psf_s->ncomptot; j++){
			psf_map_s->d_nod_map[inod][j] 
			  = (y1*psf_s->d_nod[i1-1][j] - y2*psf_s->d_nod[i2-1][j]) / (y1-y2);
		};
	};
	
	return;
}
