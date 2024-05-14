
/* take_normal_psf_c.c */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "take_normal_psf_c.h"


static double cal_psf_grid_range(long nnod_viz, double *xyzw_viz,
                                 double *xmin_psf, double *xmax_psf,
                                 double *center_psf){
    double rmax_psf;
    
	int i, nd;
    double r_tmp;
	
	for (nd = 0; nd < 3; nd++) {
		xmin_psf[nd] = xyzw_viz[nd];
		xmax_psf[nd] = xyzw_viz[nd];
	};
	for (i = 1; i < nnod_viz; i++) {
		for (nd = 0; nd < 3; nd++) {
			if ( xmin_psf[nd] > xyzw_viz[i*IFOUR + nd]) {
				xmin_psf[nd] = xyzw_viz[i*IFOUR + nd];
            }
			if ( xmax_psf[nd] < xyzw_viz[i*IFOUR + nd]) {
				xmax_psf[nd] = xyzw_viz[i*IFOUR + nd];
            }
		}
	};
    
    rmax_psf = 0.5*(xmax_psf[0]-xmin_psf[0]);
	for (nd = 1; nd < 3; nd++) {
        r_tmp = 0.5*(xmax_psf[nd]-xmin_psf[nd]);
        if(rmax_psf < r_tmp) rmax_psf = r_tmp;
	};
	for (nd = 0; nd < 3; nd++){
        center_psf[nd] = 0.5 * (xmax_psf[nd]+xmin_psf[nd]);
    };
	return rmax_psf;
}

static void take_normal_ele_psf(struct psf_data *viz_s){
	long i, i1, i2, i3;
	
	for (i = 0; i < viz_s->nele_viz; i++){
		i1 = viz_s->ie_viz[i][0] - 1;
		i2 = viz_s->ie_viz[i][1] - 1;
		i3 = viz_s->ie_viz[i][2] - 1;
		if(i1 <0 || i1 >= viz_s->nnod_viz) printf("i1 fault %ld %ld %ld\n", i, i1, viz_s->nnod_viz);
		if(i2 <0 || i2 >= viz_s->nnod_viz) printf("i2 fault %ld %ld %ld\n", i, i2, viz_s->nnod_viz);
		if(i3 <0 || i3 >= viz_s->nnod_viz) printf("i3 fault %ld %ld %ld\n", i, i3, viz_s->nnod_viz);
		cal_center_4_triangle_c(&viz_s->xyzw_viz[i1*IFOUR],
                                &viz_s->xyzw_viz[i2*IFOUR],
                                &viz_s->xyzw_viz[i3*IFOUR],
								&viz_s->xyzw_ele_viz[i*IFOUR]);
		viz_s->area_viz[i] = cal_normal_4_triangle_c(&viz_s->xyzw_viz[i1*IFOUR],
                                                     &viz_s->xyzw_viz[i2*IFOUR],
													 &viz_s->xyzw_viz[i3*IFOUR],
                                                     &viz_s->norm_ele[i*IFOUR]);
	};
    
	viz_s->area_total = 0.0;
	for (i = 0; i < viz_s->nele_viz; i++){
		viz_s->area_total = viz_s->area_total + viz_s->area_viz[i];
	};
	return;
};

void easy_normal_nod_psf(struct psf_data *viz_s){
	int k;
    long i, i1;
	double d;
    int *nele_for_nod;
    
	nele_for_nod = (int *)calloc(viz_s->nnod_viz, sizeof(int *));

	for (i = 0; i < viz_s->nnod_viz; i++){
        nele_for_nod[i] = 0;
        viz_s->norm_nod[4*i+0] = 0.0;
        viz_s->norm_nod[4*i+1] = 0.0;
        viz_s->norm_nod[4*i+2] = 0.0;
        viz_s->norm_nod[4*i+3] = 1.0;
    };

    for (i = 0; i < viz_s->nele_viz; i++){
        for (k=0; k<3; k++){
            i1 = viz_s->ie_viz[i][k] - 1;
            nele_for_nod[i1] = nele_for_nod[i1] + 1;
            
            viz_s->norm_nod[4*i1  ] = viz_s->norm_nod[4*i1  ]
                                    + viz_s->norm_ele[4*i  ];
            viz_s->norm_nod[4*i1+1] = viz_s->norm_nod[4*i1+1]
                                    + viz_s->norm_ele[4*i+1];
            viz_s->norm_nod[4*i1+2] = viz_s->norm_nod[4*i1+2]
                                    + viz_s->norm_ele[4*i+2];
        };
    };
    
	for (i = 0; i < viz_s->nnod_viz; i++){
 		if(nele_for_nod[i] == 0){
            viz_s->norm_nod[4*i+0] = 0.0;
            viz_s->norm_nod[4*i+1] = 0.0;
            viz_s->norm_nod[4*i+2] = 0.0;
            viz_s->norm_nod[4*i+3] = 0.0;
        } else {
            d = (double) ONE / nele_for_nod[i];
            viz_s->norm_nod[4*i  ] = viz_s->norm_nod[4*i  ] * d;
            viz_s->norm_nod[4*i+1] = viz_s->norm_nod[4*i+1] * d;
            viz_s->norm_nod[4*i+2] = viz_s->norm_nod[4*i+2] * d;
            viz_s->norm_nod[4*i+3] = 1.0;
        };
    };
    
    free(nele_for_nod);
	
	return;
};

static void take_normal_nod_psf(struct psf_data *viz_s){
	int n, k, ist, ied, l;
    long i, i1, i2, i3;
	double d, xe[3], d2h[3];
    int *nele_for_nod;
    int *istack_ele_for_nod;
    int ntot_ele_for_nod;
    double *dist_to_ele;
    int iflag_zero;
    
	nele_for_nod = (int *)calloc(viz_s->nnod_viz, sizeof(int *));
	istack_ele_for_nod = (int *)calloc((viz_s->nnod_viz+1), sizeof(int *));
	for (k = 0; k<3; k++){
        for (i = 0; i < viz_s->nele_viz; i++){
            i1 = viz_s->ie_viz[i][k] - 1;
            nele_for_nod[i1] = nele_for_nod[i1] + 1;
        };
    };
    for (i = 0; i < viz_s->nnod_viz; i++){
        istack_ele_for_nod[i+1] = istack_ele_for_nod[i] + nele_for_nod[i];
        nele_for_nod[i] = 0;
    };
    ntot_ele_for_nod = istack_ele_for_nod[viz_s->nnod_viz];
    
	dist_to_ele = (double *)calloc(ntot_ele_for_nod, sizeof(double *));
    
    for (i = 0; i < viz_s->nele_viz; i++){
		i1 = viz_s->ie_viz[i][0] - 1;
		i2 = viz_s->ie_viz[i][1] - 1;
		i3 = viz_s->ie_viz[i][2] - 1;
		
        xe[0] = ( viz_s->xyzw_viz[i1*IFOUR + 0]
                  + viz_s->xyzw_viz[i2*IFOUR + 0] + viz_s->xyzw_viz[i3*IFOUR + 0] ) / 3.0;
        xe[1] = ( viz_s->xyzw_viz[i1*IFOUR + 1]
                  + viz_s->xyzw_viz[i2*IFOUR + 1] + viz_s->xyzw_viz[i3*IFOUR + 1] ) / 3.0;
        xe[2] = ( viz_s->xyzw_viz[i1*IFOUR + 2]
                  + viz_s->xyzw_viz[i2*IFOUR + 2] + viz_s->xyzw_viz[i3*IFOUR + 2] ) / 3.0;
        
		d2h[0] = sqrt(  (xe[0] - viz_s->xyzw_viz[i1*IFOUR + 0])*(xe[0] - viz_s->xyzw_viz[i1*IFOUR + 0])
                      + (xe[1] - viz_s->xyzw_viz[i1*IFOUR + 1])*(xe[1] - viz_s->xyzw_viz[i1*IFOUR + 1])
                      + (xe[2] - viz_s->xyzw_viz[i1*IFOUR + 2])*(xe[2] - viz_s->xyzw_viz[i1*IFOUR + 2]) );
		d2h[1] = sqrt(  (xe[0] - viz_s->xyzw_viz[i2*IFOUR + 0])*(xe[0] - viz_s->xyzw_viz[i2*IFOUR + 0])
                      + (xe[1] - viz_s->xyzw_viz[i2*IFOUR + 1])*(xe[1] - viz_s->xyzw_viz[i2*IFOUR + 1])
                      + (xe[2] - viz_s->xyzw_viz[i2*IFOUR + 2])*(xe[2] - viz_s->xyzw_viz[i2*IFOUR + 2]) );
		d2h[2] = sqrt(  (xe[0] - viz_s->xyzw_viz[i3*IFOUR + 0])*(xe[0] - viz_s->xyzw_viz[i3*IFOUR + 0])
                      + (xe[1] - viz_s->xyzw_viz[i3*IFOUR + 1])*(xe[1] - viz_s->xyzw_viz[i3*IFOUR + 1])
                      + (xe[2] - viz_s->xyzw_viz[i3*IFOUR + 2])*(xe[2] - viz_s->xyzw_viz[i3*IFOUR + 2]) );
        k = istack_ele_for_nod[i1] + nele_for_nod[i1];
        nele_for_nod[i1] = nele_for_nod[i1] + 1;
        dist_to_ele[k] = d2h[0];
        k = istack_ele_for_nod[i2] + nele_for_nod[i2];
        nele_for_nod[i2] = nele_for_nod[i2] + 1;
        dist_to_ele[k] = d2h[1];
        k = istack_ele_for_nod[i3] + nele_for_nod[i3];
        nele_for_nod[i3] = nele_for_nod[i3] + 1;
        dist_to_ele[k] = d2h[2];
    };
    for (i = 0; i < viz_s->nnod_viz; i++){
        ist = istack_ele_for_nod[i];
        ied = istack_ele_for_nod[i+1];
        iflag_zero = 0;
        for (k=ist; k<ied; k++){
            if (dist_to_ele[k] == 0.0) {iflag_zero = 1;};
        };
        if (iflag_zero == 1){
            for (k=ist; k<ied; k++){
                if (dist_to_ele[k] == 0.0){
                    dist_to_ele[k] = 1.0;
                } else {
                    dist_to_ele[k] = 0.0;
                };
            };
        } else {
            for (k=ist; k<ied; k++){
                dist_to_ele[k] = 1.0 / dist_to_ele[k];
            };
        };
        nele_for_nod[i] = 0;
        viz_s->norm_nod[4*i  ] = 0.0;
        viz_s->norm_nod[4*i+1] = 0.0;
        viz_s->norm_nod[4*i+2] = 0.0;
        viz_s->norm_nod[4*i+3] = 1.0;
    };
    for (i = 0; i < viz_s->nele_viz; i++){
        for (k=0; k<3; k++){
            i1 = viz_s->ie_viz[i][k] - 1;
            l = istack_ele_for_nod[i1] + nele_for_nod[i1];
            nele_for_nod[i1] = nele_for_nod[i1] + 1;
            
            viz_s->norm_nod[4*i1  ] = viz_s->norm_nod[4*i1  ]
                                    + dist_to_ele[l] * viz_s->norm_ele[4*i  ];
            viz_s->norm_nod[4*i1+1] = viz_s->norm_nod[4*i1+1]
                                    + dist_to_ele[l] * viz_s->norm_ele[4*i+1];
            viz_s->norm_nod[4*i1+2] = viz_s->norm_nod[4*i1+2]
                                    + dist_to_ele[l] * viz_s->norm_ele[4*i+2];
        };
    };
    
    free(dist_to_ele);
    free(istack_ele_for_nod);
    free(nele_for_nod);
	
	for (n = 0; n < viz_s->nnod_viz; n++){
		d = sqrt(  viz_s->norm_nod[4*n  ]*viz_s->norm_nod[4*n  ]
                 + viz_s->norm_nod[4*n+1]*viz_s->norm_nod[4*n+1]
                 + viz_s->norm_nod[4*n+2]*viz_s->norm_nod[4*n+2]);
		if(d == 0.0){
			viz_s->norm_nod[4*n+0] = 0.0;
			viz_s->norm_nod[4*n+1] = 0.0;
			viz_s->norm_nod[4*n+2] = 0.0;
            viz_s->norm_nod[4*n+3] = 1.0;
		} else {
			d = 1.0 / d;
			viz_s->norm_nod[4*n+0] = viz_s->norm_nod[4*n+0] * d;
			viz_s->norm_nod[4*n+1] = viz_s->norm_nod[4*n+1] * d;
			viz_s->norm_nod[4*n+2] = viz_s->norm_nod[4*n+2] * d;
            viz_s->norm_nod[4*n+3] = 1.0;
		}
	};
	
	return;
};

static void take_length_ele_fline(struct fline_data *fline_d){
    long i, i1, i2;
    int nd;
	double len2;

	for (i = 0; i < fline_d->nnod_fline; i++){
        fline_d->dir_nod[4*i  ] = 0.0;
        fline_d->dir_nod[4*i+1] = 0.0;
        fline_d->dir_nod[4*i+2] = 0.0;
        fline_d->dir_nod[4*i+3] = 1.0;
    }
	
	for (i = 0; i < fline_d->nedge_fline; i++){
		i1 = fline_d->iedge_fline[i][0] - 1;
		i2 = fline_d->iedge_fline[i][1] - 1;
		for (nd=0; nd<3; nd++) {
            fline_d->dir_edge[4*i+nd] = fline_d->xyzw_fline[i2*IFOUR + nd]
                                        - fline_d->xyzw_fline[i1*IFOUR + nd];
		}
    }
    
    fline_d->length_total = 0.0;
    for (i = 0; i < fline_d->nedge_fline; i++){
        fline_d->length_edge[i]
        = sqrt(  fline_d->dir_edge[4*i+0]*fline_d->dir_edge[4*i+0]
               + fline_d->dir_edge[4*i+1]*fline_d->dir_edge[4*i+1]
               + fline_d->dir_edge[4*i+2]*fline_d->dir_edge[4*i+2] );
		
		if (fline_d->length_edge[i] == 0.0){
            fline_d->dir_edge[4*i+0] = 0.0;
            fline_d->dir_edge[4*i+1] = 0.0;
            fline_d->dir_edge[4*i+2] = 0.0;
            fline_d->dir_edge[4*i+3] = 1.0;
		}
		else{
            fline_d->dir_edge[4*i+0]
                = fline_d->dir_edge[4*i+0] / fline_d->length_edge[i];
            fline_d->dir_edge[4*i+1]
                = fline_d->dir_edge[4*i+1] / fline_d->length_edge[i];
            fline_d->dir_edge[4*i+2]
                = fline_d->dir_edge[4*i+2] / fline_d->length_edge[i];
            fline_d->dir_edge[4*i+3] = 1.0;
		}
        
		fline_d->length_total = fline_d->length_total + fline_d->length_edge[i];
    };
    
	for (i = 0; i < fline_d->nedge_fline; i++){
		i1 = fline_d->iedge_fline[i][0] - 1;
		i2 = fline_d->iedge_fline[i][1] - 1;
		for (nd=0; nd<3; nd++) {
            fline_d->xyzw_edge_fline[4*i+nd] = (fline_d->xyzw_fline[i1*IFOUR + nd]
                                                + fline_d->xyzw_fline[i2*IFOUR + nd])*HALF;
            
            fline_d->dir_nod[4*i1+nd] = fline_d->dir_nod[4*i1+nd] + fline_d->dir_edge[4*i+nd];
            fline_d->dir_nod[4*i2+nd] = fline_d->dir_nod[4*i2+nd] + fline_d->dir_edge[4*i+nd];
		}		
        
	};
    
	for (i = 0; i < fline_d->nnod_fline; i++){
		len2= sqrt(  fline_d->dir_nod[4*i+0]*fline_d->dir_nod[4*i+0]
                   + fline_d->dir_nod[4*i+1]*fline_d->dir_nod[4*i+1]
                   + fline_d->dir_nod[4*i+2]*fline_d->dir_nod[4*i+2] );
		
		for (nd=0; nd<3; nd++) {
            fline_d->dir_nod[4*i+nd] =  fline_d->dir_nod[4*i+nd] / len2;
		}
	}
	return;
};


static void sum_rms_ave_psf(long ist, long ied,
                            struct psf_data *viz_s){
    int icomp;
    long i, i1, i2, i3;
    double d;
    
    for(icomp = 0; icomp < viz_s->ncomptot; icomp++){
        viz_s->d_rms[icomp] = 0.0;
        viz_s->d_ave[icomp] = 0.0;
        for (i = ist; i < ied; i++){
            i1 = viz_s->ie_viz[i][0] - 1;
            i2 = viz_s->ie_viz[i][1] - 1;
            i3 = viz_s->ie_viz[i][2] - 1;
            d = (  viz_s->d_nod[i1*viz_s->ncomptot + icomp]
                 + viz_s->d_nod[i2*viz_s->ncomptot + icomp]
                 + viz_s->d_nod[i3*viz_s->ncomptot + icomp] ) / 3.0;
            
            viz_s->d_rms[icomp] = viz_s->d_rms[icomp]
            + d * d * viz_s->area_viz[i];
            viz_s->d_ave[icomp] = viz_s->d_ave[icomp]
            + d * viz_s->area_viz[i];
        }
    }
    return;
}

static void sum_rms_ave_fline(long ist, long ied,
                              struct fline_data *fline_d){
    int icomp;
    long i, i1, i2;
    double d;
    
    for (icomp = 0; icomp < fline_d->ncomptot; icomp++){
        fline_d->d_rms[icomp] = 0.0;
        fline_d->d_ave[icomp] = 0.0;
        for (i = 0; i < fline_d->nedge_fline; i++){
            i1 = fline_d->iedge_fline[i][0] - 1;
            i2 = fline_d->iedge_fline[i][1] - 1;
            d = (  fline_d->d_nod[i1*fline_d->ncomptot + icomp]
                 + fline_d->d_nod[i2*fline_d->ncomptot + icomp]) / 2.0;
            
            fline_d->d_rms[icomp] = fline_d->d_rms[icomp]
                + d * d * fline_d->length_edge[i];
            fline_d->d_ave[icomp] = fline_d->d_ave[icomp]
                + d * fline_d->length_edge[i];
        }
    }
    return;
}

static void take_rms_ave_psf(struct psf_data *viz_s){
    int icomp;
	
    sum_rms_ave_psf(IZERO, viz_s->nele_viz, viz_s);
    
    for (icomp = 0; icomp < viz_s->ncomptot; icomp++){
		viz_s->d_rms[icomp] = sqrt(viz_s->d_rms[icomp] / viz_s->area_total );
		viz_s->d_ave[icomp] = viz_s->d_ave[icomp] / viz_s->area_total;
	}
	return;
}

static void take_rms_ave_fline(struct fline_data *fline_d){
    int icomp;
    sum_rms_ave_fline(IZERO, fline_d->nedge_fline, fline_d);

    for (icomp = 0; icomp < fline_d->ncomptot; icomp++){
        fline_d->d_rms[icomp] = sqrt(fline_d->d_rms[icomp] / fline_d->length_total );
        fline_d->d_ave[icomp] = fline_d->d_ave[icomp] / fline_d->length_total;
	}
	return;
}

static void take_minmax_psf_each_component(long nnod_viz, long nfield, long ncomptot,
                                           long *istack_comp, double *d_nod, double *d_amp,
                                           double *d_min, double *d_max){
    int ifld, n;
    long icomp;
    
    for (ifld = 0; ifld < nfield; ifld++){
        for(n = 0; n < nnod_viz; n++) d_amp[n*nfield + ifld] = 0.0;
        for (icomp = istack_comp[ifld]; icomp < istack_comp[ifld+1]; icomp++){
            for (n = 0; n < nnod_viz; n++){
                d_amp[n*nfield + ifld] = d_amp[n*nfield + ifld]
                + d_nod[n*ncomptot + icomp] * d_nod[n*ncomptot + icomp];
            };
        };
        for (n = 0; n < nnod_viz; n++){
            d_amp[n*nfield + ifld] = sqrt(d_amp[n*nfield + ifld]);
        };
    };
    
    for (icomp = 0; icomp < ncomptot; icomp++){
        d_min[icomp] = d_nod[icomp];
        d_max[icomp] = d_nod[icomp];
        for (n = 1; n < nnod_viz; n++){
            if ( d_nod[n*ncomptot + icomp] < d_min[icomp] )
            {
                d_min[icomp] = d_nod[n*ncomptot + icomp];
            };
            if ( d_nod[n*ncomptot + icomp] > d_max[icomp] )
            {
                d_max[icomp] = d_nod[n*ncomptot + icomp];
            };
        };
        
        if (d_min[icomp] >= d_max[icomp]) {
            d_min[icomp] = ZERO;
            d_max[icomp] = ONE;
        }
    }
    return;
}

static void take_minmax_viz_fields(long nfield, long *istack_comp,
                                   double *d_min, double *d_max,
                                   double *amp_min, double *amp_max){
    int ifld;
    long icomp, ist, ied;
	for (ifld = 0; ifld < nfield; ifld++){
        ist = istack_comp[ifld];
        ied = istack_comp[ifld+1];
		amp_min[ifld] = d_min[ist];
		amp_max[ifld] = d_max[ist];
        
		for(icomp=ist+1; icomp<ied; icomp++){
			if (d_min[icomp] < amp_min[ifld]){amp_min[ifld] = d_min[icomp];};
			if (d_max[icomp] > amp_max[ifld]){amp_max[ifld] = d_max[icomp];};
		};
	};
	
	return;
}

void take_normal_psf(struct psf_data *viz_s){
	alloc_psf_norm_s(viz_s);
    viz_s->rmax_psf = cal_psf_grid_range(viz_s->nnod_viz, viz_s->xyzw_viz,
                                         viz_s->xmin_psf, viz_s->xmax_psf,
                                         viz_s->center_psf);
	take_normal_ele_psf(viz_s);
	take_normal_nod_psf(viz_s);
	return;
}

void take_length_fline(struct fline_data *fline_d){
    fline_d->rmax_psf = cal_psf_grid_range(fline_d->nnod_fline, fline_d->xyzw_fline,
                                           fline_d->xmin_psf, fline_d->xmax_psf,
                                           fline_d->center_psf);
    
    alloc_fline_data(fline_d);
	take_length_ele_fline(fline_d);
	return;
}

void take_minmax_psf(struct psf_data *viz_s){
	take_rms_ave_psf(viz_s);
    take_minmax_psf_each_component(viz_s->nnod_viz,
                                   viz_s->nfield, viz_s->ncomptot,
                                   viz_s->istack_comp,
                                   viz_s->d_nod, viz_s->d_amp,
                                   viz_s->d_min, viz_s->d_max);
    take_minmax_viz_fields(viz_s->nfield, viz_s->istack_comp,
                           viz_s->d_min, viz_s->d_max,
                           viz_s->amp_min, viz_s->amp_max);
	return;
}

void take_minmax_fline(struct fline_data *fline_d){
	take_rms_ave_fline(fline_d);
    take_minmax_psf_each_component(fline_d->nnod_fline,
                                   fline_d->nfield, fline_d->ncomptot,
                                   fline_d->istack_comp,
                                   fline_d->d_nod, fline_d->d_amp,
                                   fline_d->d_min, fline_d->d_max);
    take_minmax_viz_fields(fline_d->nfield, fline_d->istack_comp,
                           fline_d->d_min, fline_d->d_max,
                           fline_d->amp_min, fline_d->amp_max);
	return;
}
