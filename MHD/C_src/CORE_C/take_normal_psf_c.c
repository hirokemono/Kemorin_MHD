
/* take_normal_psf_c.c */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "take_normal_psf_c.h"


static void take_normal_ele_psf(struct psf_data *viz_s,
                                struct psf_normals *psf_n){
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
		psf_n->area_ele[i] = cal_normal_4_triangle_c(&viz_s->xyzw_viz[i1*IFOUR],
                                                     &viz_s->xyzw_viz[i2*IFOUR],
													 &viz_s->xyzw_viz[i3*IFOUR],
                                                     &psf_n->norm_ele[i*IFOUR]);
	};
    
	psf_n->total_area = 0.0;
	for (i = 0; i < viz_s->nele_viz; i++){
		psf_n->total_area = psf_n->total_area + psf_n->area_ele[i];
	};
	return;
};

void easy_normal_nod_psf(struct psf_data *viz_s,
                         struct psf_normals *psf_n){
	int k;
    long i, i1;
	double d;
    int *nele_for_nod;
    
	nele_for_nod = (int *)calloc(viz_s->nnod_viz, sizeof(int *));
    
	for (i = 0; i < viz_s->nnod_viz; i++){
        nele_for_nod[i] = 0;
        psf_n->norm_nod[4*i+0] = 0.0;
        psf_n->norm_nod[4*i+1] = 0.0;
        psf_n->norm_nod[4*i+2] = 0.0;
        psf_n->norm_nod[4*i+3] = 1.0;
    };

    for (i = 0; i < viz_s->nele_viz; i++){
        for (k=0; k<3; k++){
            i1 = viz_s->ie_viz[i][k] - 1;
            nele_for_nod[i1] = nele_for_nod[i1] + 1;
            
            psf_n->norm_nod[4*i1  ] = psf_n->norm_nod[4*i1  ]
                                    + psf_n->norm_ele[4*i  ];
            psf_n->norm_nod[4*i1+1] = psf_n->norm_nod[4*i1+1]
                                    + psf_n->norm_ele[4*i+1];
            psf_n->norm_nod[4*i1+2] = psf_n->norm_nod[4*i1+2]
                                    + psf_n->norm_ele[4*i+2];
        };
    };
    
	for (i = 0; i < viz_s->nnod_viz; i++){
 		if(nele_for_nod[i] == 0){
            psf_n->norm_nod[4*i+0] = 0.0;
            psf_n->norm_nod[4*i+1] = 0.0;
            psf_n->norm_nod[4*i+2] = 0.0;
            psf_n->norm_nod[4*i+3] = 0.0;
        } else {
            d = (double) ONE / nele_for_nod[i];
            psf_n->norm_nod[4*i  ] = psf_n->norm_nod[4*i  ] * d;
            psf_n->norm_nod[4*i+1] = psf_n->norm_nod[4*i+1] * d;
            psf_n->norm_nod[4*i+2] = psf_n->norm_nod[4*i+2] * d;
            psf_n->norm_nod[4*i+3] = 1.0;
        };
    };
    
    free(nele_for_nod);
	
	return;
};

static void take_normal_nod_psf(struct psf_data *viz_s,
                                struct psf_normals *psf_n){
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
        psf_n->norm_nod[4*i  ] = 0.0;
        psf_n->norm_nod[4*i+1] = 0.0;
        psf_n->norm_nod[4*i+2] = 0.0;
        psf_n->norm_nod[4*i+3] = 1.0;
    };
    for (i = 0; i < viz_s->nele_viz; i++){
        for (k=0; k<3; k++){
            i1 = viz_s->ie_viz[i][k] - 1;
            l = istack_ele_for_nod[i1] + nele_for_nod[i1];
            nele_for_nod[i1] = nele_for_nod[i1] + 1;
            
            psf_n->norm_nod[4*i1  ] = psf_n->norm_nod[4*i1  ]
                                    + dist_to_ele[l] * psf_n->norm_ele[4*i  ];
            psf_n->norm_nod[4*i1+1] = psf_n->norm_nod[4*i1+1]
                                    + dist_to_ele[l] * psf_n->norm_ele[4*i+1];
            psf_n->norm_nod[4*i1+2] = psf_n->norm_nod[4*i1+2]
                                    + dist_to_ele[l] * psf_n->norm_ele[4*i+2];
        };
    };
    
    free(dist_to_ele);
    free(istack_ele_for_nod);
    free(nele_for_nod);
	
	for (n = 0; n < viz_s->nnod_viz; n++){
		d = sqrt(  psf_n->norm_nod[4*n  ]*psf_n->norm_nod[4*n  ]
                 + psf_n->norm_nod[4*n+1]*psf_n->norm_nod[4*n+1]
                 + psf_n->norm_nod[4*n+2]*psf_n->norm_nod[4*n+2]);
		if(d == 0.0){
			psf_n->norm_nod[4*n+0] = 0.0;
			psf_n->norm_nod[4*n+1] = 0.0;
			psf_n->norm_nod[4*n+2] = 0.0;
            psf_n->norm_nod[4*n+3] = 1.0;
		} else {
			d = 1.0 / d;
			psf_n->norm_nod[4*n+0] = psf_n->norm_nod[4*n+0] * d;
			psf_n->norm_nod[4*n+1] = psf_n->norm_nod[4*n+1] * d;
			psf_n->norm_nod[4*n+2] = psf_n->norm_nod[4*n+2] * d;
            psf_n->norm_nod[4*n+3] = 1.0;
		}
	};
	
	return;
};


static void sum_rms_ave_psf(long ist, long ied,
                            struct psf_data *viz_s,
                            struct psf_normals *psf_n){
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
            + d * d * psf_n->area_ele[i];
            viz_s->d_ave[icomp] = viz_s->d_ave[icomp]
            + d * psf_n->area_ele[i];
        }
    }
    return;
}

static void take_rms_ave_psf(struct psf_data *viz_s,
                             struct psf_normals *psf_n){
    int icomp;
	
    sum_rms_ave_psf(IZERO, viz_s->nele_viz, viz_s, psf_n);
    
    for (icomp = 0; icomp < viz_s->ncomptot; icomp++){
		viz_s->d_rms[icomp] = sqrt(viz_s->d_rms[icomp] / psf_n->total_area );
		viz_s->d_ave[icomp] = viz_s->d_ave[icomp] / psf_n->total_area;
	}
	return;
}

void take_normal_psf(long nadded_for_phi0,
                     struct psf_data *viz_s,
                     struct psf_normals *psf_n){
    viz_s->rmax_psf = cal_psf_grid_range(viz_s->nnod_viz, viz_s->xyzw_viz,
                                         viz_s->xmin_psf, viz_s->xmax_psf,
                                         viz_s->center_psf);
    cal_colat_and_longitude(nadded_for_phi0, 
                            viz_s->nnod_viz, viz_s->xyzw_viz,
                            psf_n->rt_viz);
    take_normal_ele_psf(viz_s, psf_n);
	take_normal_nod_psf(viz_s, psf_n);
	return;
}

void take_minmax_psf(struct psf_data *viz_s,
                     struct psf_normals *psf_n){
	take_rms_ave_psf(viz_s, psf_n);
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
