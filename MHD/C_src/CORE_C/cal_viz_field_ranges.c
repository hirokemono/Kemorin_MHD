/*
 *  cal_viz_field_ranges.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "cal_viz_field_ranges.h"

double cal_psf_grid_range(long nnod_viz, double *xyzw_viz,
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


void take_minmax_psf_each_component(long nnod_viz, long nfield, long ncomptot,
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

void take_minmax_viz_fields(long nfield, long *istack_comp,
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

void cal_colat_and_longitude(long nadded_for_phi0,
                             long nnod_viz, double *xyzw_viz, 
                             double *rt_viz){
    long i;
    double pi = FOUR * atan(ONE);
    double rtpw[4];
    
    for(i=0;i<nnod_viz;i++){
        xyzw_to_rtpw_c(IONE, &xyzw_viz[4*i], rtpw);
        
        rt_viz[2*i  ] = rtpw[1];
        rt_viz[2*i+1] = rtpw[2];
        rt_viz[2*i+1] = fmod(rtpw[2]+pi,(TWO*pi));
        if(rt_viz[2*i+1]*rt_viz[2*i+1] < 1.0e-25){
            rt_viz[2*i+1] = TWO * pi;
        }

    }

    for(i=nnod_viz-2.0*nadded_for_phi0;i<nnod_viz-nadded_for_phi0;i++){
//        rt_viz[2*i+1] = 0.;
    }
    for(i=nnod_viz-nadded_for_phi0;i<nnod_viz;i++){
        rt_viz[2*i+1] = 0.0;
    }
    return;
}

void shift_longitude(double add_phi, long nnod_viz, double *xyzw_viz){
    long i;
    double pi = FOUR * atan(ONE);
    double rtpw[4];
    double r[3]={0.,0.,0.};
    
    for(i=0;i<nnod_viz;i++){
        xyzw_to_rtpw_c(IONE, &xyzw_viz[4*i], rtpw);
        
        r[0] = rtpw[0];
        rtpw[2] = fmod(rtpw[2]+add_phi,(TWO*pi));
        sph_vector_to_xyz_vect(rtpw[1], rtpw[2], r, &xyzw_viz[4*i]);
    }
    return;
}

