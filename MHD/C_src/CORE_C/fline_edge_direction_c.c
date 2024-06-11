/*
 *  fline_edge_direction_c.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "fline_edge_direction_c.h"

static void take_rms_ave_points(struct psf_data *point_d){
    int icomp;
    long i;
    double d;
    for(icomp = 0; icomp < point_d->ncomptot; icomp++){
        point_d->d_rms[icomp] = 0.0;
        point_d->d_ave[icomp] = 0.0;
        for(i = 0; i < point_d->nnod_viz; i++){
            d = point_d->d_nod[i*point_d->ncomptot + icomp];
            
            point_d->d_rms[icomp] = point_d->d_rms[icomp] + d * d;
            point_d->d_ave[icomp] = point_d->d_ave[icomp] + d;
        }
    }

    for (icomp = 0; icomp < point_d->ncomptot; icomp++){
        point_d->d_rms[icomp] = sqrt(point_d->d_rms[icomp] / (double) point_d->nnod_viz );
        point_d->d_ave[icomp] = point_d->d_ave[icomp] / (double) point_d->nnod_viz;
    }
    return;
}

static void take_length_ele_fline(struct psf_data *fline_d,
                                  struct fline_directions *fline_dir){
    long i, i1, i2;
    int nd;
	double len2;

	for (i = 0; i < fline_d->nnod_viz; i++){
        fline_dir->dir_nod[4*i  ] = 0.0;
        fline_dir->dir_nod[4*i+1] = 0.0;
        fline_dir->dir_nod[4*i+2] = 0.0;
        fline_dir->dir_nod[4*i+3] = 1.0;
    }
	
	for (i = 0; i < fline_d->nele_viz; i++){
		i1 = fline_d->ie_viz[i][0] - 1;
		i2 = fline_d->ie_viz[i][1] - 1;
		for (nd=0; nd<3; nd++) {
            fline_dir->dir_edge[4*i+nd] = fline_d->xyzw_viz[i2*IFOUR + nd]
                                        - fline_d->xyzw_viz[i1*IFOUR + nd];
		}
    }
    
    fline_dir->total_length = 0.0;
    for (i = 0; i < fline_d->nele_viz; i++){
        fline_dir->length_edge[i]
          = sqrt(  fline_dir->dir_edge[4*i+0] * fline_dir->dir_edge[4*i+0]
                 + fline_dir->dir_edge[4*i+1] * fline_dir->dir_edge[4*i+1]
                 + fline_dir->dir_edge[4*i+2] * fline_dir->dir_edge[4*i+2] );
		
		if(fline_dir->length_edge[i] == 0.0){
            fline_dir->dir_edge[4*i+0] = 0.0;
            fline_dir->dir_edge[4*i+1] = 0.0;
            fline_dir->dir_edge[4*i+2] = 0.0;
            fline_dir->dir_edge[4*i+3] = 1.0;
		}
		else{
            fline_dir->dir_edge[4*i+0]
                = fline_dir->dir_edge[4*i+0] / fline_dir->length_edge[i];
            fline_dir->dir_edge[4*i+1]
                = fline_dir->dir_edge[4*i+1] / fline_dir->length_edge[i];
            fline_dir->dir_edge[4*i+2]
                = fline_dir->dir_edge[4*i+2] / fline_dir->length_edge[i];
            fline_dir->dir_edge[4*i+3] = 1.0;
		}
        
        fline_dir->total_length = fline_dir->total_length + fline_dir->length_edge[i];
    };
    
	for (i = 0; i < fline_d->nele_viz; i++){
		i1 = fline_d->ie_viz[i][0] - 1;
		i2 = fline_d->ie_viz[i][1] - 1;
		for (nd=0; nd<3; nd++) {
            fline_d->xyzw_ele_viz[4*i+nd] = (fline_d->xyzw_viz[i1*IFOUR + nd]
                                             + fline_d->xyzw_viz[i2*IFOUR + nd])*HALF;
            
            fline_dir->dir_nod[4*i1+nd] = fline_dir->dir_nod[4*i1+nd]
                                         + fline_dir->dir_edge[4*i+nd];
            fline_dir->dir_nod[4*i2+nd] = fline_dir->dir_nod[4*i2+nd]
                                         + fline_dir->dir_edge[4*i+nd];
		}		
        
	};
    
	for (i = 0; i < fline_d->nnod_viz; i++){
		len2= sqrt(  fline_dir->dir_nod[4*i+0] * fline_dir->dir_nod[4*i+0]
                   + fline_dir->dir_nod[4*i+1] * fline_dir->dir_nod[4*i+1]
                   + fline_dir->dir_nod[4*i+2] * fline_dir->dir_nod[4*i+2] );
		
		for (nd=0; nd<3; nd++) {
            fline_dir->dir_nod[4*i+nd] =  fline_dir->dir_nod[4*i+nd] / len2;
		}
	}
	return;
};

static void sum_rms_ave_fline(long ist, long ied,
                              struct fline_directions *fline_dir,
                              struct psf_data *fline_d){
    int icomp;
    long i, i1, i2;
    double d;
    
    for (icomp = 0; icomp < fline_d->ncomptot; icomp++){
        fline_d->d_rms[icomp] = 0.0;
        fline_d->d_ave[icomp] = 0.0;
        for (i = 0; i < fline_d->nele_viz; i++){
            i1 = fline_d->ie_viz[i][0] - 1;
            i2 = fline_d->ie_viz[i][1] - 1;
            d = (  fline_d->d_nod[i1*fline_d->ncomptot + icomp]
                 + fline_d->d_nod[i2*fline_d->ncomptot + icomp]) / 2.0;
            
            fline_d->d_rms[icomp] = fline_d->d_rms[icomp]
                + d * d * fline_dir->length_edge[i];
            fline_d->d_ave[icomp] = fline_d->d_ave[icomp]
                + d * fline_dir->length_edge[i];
        }
    }
    return;
}

static void take_rms_ave_fline(struct fline_directions *fline_dir,
                               struct psf_data *fline_d){
    int icomp;
    sum_rms_ave_fline(IZERO, fline_d->nele_viz, fline_dir, fline_d);

    for (icomp = 0; icomp < fline_d->ncomptot; icomp++){
        fline_d->d_rms[icomp] = sqrt(fline_d->d_rms[icomp] / fline_dir->total_length );
        fline_d->d_ave[icomp] = fline_d->d_ave[icomp] / fline_dir->total_length;
    }
    return;
}


void take_length_fline(struct psf_data *fline_d,
                       struct fline_directions *fline_dir){
    fline_d->rmax_psf = cal_psf_grid_range(fline_d->nnod_viz, fline_d->xyzw_viz,
                                           fline_d->xmin_psf, fline_d->xmax_psf,
                                           fline_d->center_psf);
    alloc_fline_direction_data(fline_d, fline_dir);
	take_length_ele_fline(fline_d, fline_dir);
	return;
}


void take_minmax_fline(struct fline_directions *fline_dir,
                       struct psf_data *fline_d){
	take_rms_ave_fline(fline_dir, fline_d);
    take_minmax_psf_each_component(fline_d->nnod_viz,
                                   fline_d->nfield, fline_d->ncomptot,
                                   fline_d->istack_comp,
                                   fline_d->d_nod, fline_d->d_amp,
                                   fline_d->d_min, fline_d->d_max);
    take_minmax_viz_fields(fline_d->nfield, fline_d->istack_comp,
                           fline_d->d_min, fline_d->d_max,
                           fline_d->amp_min, fline_d->amp_max);
	return;
}

void take_minmax_points(struct psf_data *point_d){
    point_d->rmax_psf = cal_psf_grid_range(point_d->nnod_viz, point_d->xyzw_viz,
                                           point_d->xmin_psf, point_d->xmax_psf,
                                           point_d->center_psf);

    
	take_rms_ave_points(point_d);
    take_minmax_psf_each_component(point_d->nnod_viz,
                                   point_d->nfield, point_d->ncomptot,
                                   point_d->istack_comp,
                                   point_d->d_nod, point_d->d_amp,
                                   point_d->d_min, point_d->d_max);
    take_minmax_viz_fields(point_d->nfield, point_d->istack_comp,
                           point_d->d_min, point_d->d_max,
                           point_d->amp_min, point_d->amp_max);
	return;
}

