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

static double take_minimum_psf_each_component(long nnod_viz, long nstraid, double *d_nod){
    double d_min = d_nod[0];
    long inod;
#pragma omp parallel for reduction(min:d_min)
    for(inod = 0; inod < nnod_viz; inod++){
        if(d_nod[inod*nstraid] < d_min){
            d_min = d_nod[inod*nstraid];
        };
    };
    return d_min;
};

void take_maximum_psf_each_component(long nnod_viz, long nstraid, double *d_nod,
                                     double *d_min, double *d_max){
    *d_min = take_minimum_psf_each_component(nnod_viz, nstraid, d_nod);
    *d_max = take_minimum_psf_each_component(nnod_viz, nstraid, d_nod);
    if(*d_min > *d_max){
        *d_min = ZERO;
        *d_max = ONE;
    }
    return;
};

void take_minmax_psf_all_component(long nnod_viz, long ncomptot, double *d_nod,
                                   double *d_min, double *d_max){
    long icomp;
    for(icomp = 0; icomp < ncomptot; icomp++){
        take_maximum_psf_each_component(nnod_viz, ncomptot, &d_nod[icomp],
                                        &d_min[icomp], &d_max[icomp]);
    }
    return;
}

void cal_colat_and_longitude(long nadded_for_phi0,
                             long nnod_viz, double *xyzw_viz, 
                             double *angle_viz){
    long i;
    double pi = FOUR * atan(ONE);
    double rtpw[4];
    
    for(i=0;i<nnod_viz;i++){
        xyzw_to_rtpw_c(IONE, &xyzw_viz[4*i], rtpw);
        
        angle_viz[2*i  ] = rtpw[1];
        angle_viz[2*i+1] = rtpw[2];
        angle_viz[2*i+1] = fmod(rtpw[2]+pi,(TWO*pi));
        if(angle_viz[2*i+1]*angle_viz[2*i+1] < 1.0e-25){
            angle_viz[2*i+1] = TWO * pi;
        }

    }

    for(i=nnod_viz-2.0*nadded_for_phi0;i<nnod_viz-nadded_for_phi0;i++){
//        angle_viz[2*i+1] = 0.;
    }
    for(i=nnod_viz-nadded_for_phi0;i<nnod_viz;i++){
        angle_viz[2*i+1] = 0.0;
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


void pick_component_from_viz_field(int id_field, int id_coord, int i_pick,
                                    long nnod_viz, double *angle_viz,
                                    long ncomptot, int ist_field, double *d_nod,
                                    double *d_pick){
    int inod;
#pragma omp parallel for
    for(inod = 0; inod < nnod_viz; inod++){
        d_pick[inod] = (double) pick_one_component_c(id_field, id_coord, i_pick,
                                                     angle_viz[2*inod], angle_viz[2*inod+1],
                                                     &d_nod[ncomptot*inod + ist_field]);
    };
    return;
}

