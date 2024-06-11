
/* check_psf_data_viewer_c.c */

#include "check_psf_data_viewer_c.h"

void check_psf_data_c(struct psf_data *tako,
                      struct psf_normals *psf_n){
	int i;
	/*int kst, ked;
	printf("nnod_viz: %ld \n", tako->nnod_viz);
	printf("nele_viz: %ld \n", tako->nele_viz);

	 printf("node \n");
	for (i = 0; i < tako->nnod_viz; i++) {
		printf(" %d %d %.12e %.12e %.12e \n", i, 
				tako->inod_viz[i], tako->xyzw_viz[i][0],
				tako->xyzw_viz[i][1], tako->xyzw_viz[i][2]);
	};
	printf("connectivity \n");
	for (i = 0; i < tako->nele_viz; i++) {
		printf(" %d %d %d %d \n", i, 
				tako->ie_viz[i][0], tako->ie_viz[i][1], tako->ie_viz[i][2]);
		}
	*/
	
	
	printf("nfield: %ld \n", tako->nfield);
	printf("ncomptot: %ld \n", tako->ncomptot);
	
	printf("istack_comp");
	for (i = 0; i < tako->nfield+1; i++) {
		printf(" %ld ", tako->istack_comp[i]);
	};
	printf("\n");
	
	printf("field name and ncomp \n");
	for (i = 0; i < tako->nfield; i++) {
		printf("%s %ld \n", tako->data_name[i], tako->ncomp[i]);
		/*
		 kst = tako->istack_comp[i];
		 ked = tako->istack_comp[i+1];
		for (j = 0; j < tako->nnod_viz; j++){
			printf("%d %d ", j, tako->inod_viz[j]);
			for (k = kst; k < ked; k++){
				printf(" %.12e ", tako->d_nod[j+tako->ncomptot + k]);
			}
			printf("\n");
		}
		*/
	};
	
	/*
	 printf("patch normal \n");
	for (i = 0; i < tako->nele_viz; i++) {
		printf(" %d %.12e %.12e %.12e %.12e \n", i, 
				psf_n->norm_ele[4*i  ], psf_n->norm_ele[4*i+1],
                psf_n->norm_ele[4*i+2], psf_n->area_ele[i]);
	};
	 printf("node normal \n");
	for (i = 0; i < tako->nnod_viz; i++) {
		printf(" %d %.12e %.12e %.12e \n", i, 
                psf_n->norm_nod_psf[i][0], psf_n->norm_nod_psf[i][1],
                psf_n->norm_nod_psf[i][2]);
	};
	*/
};

void check_psf_ave_rms_c(struct psf_data *tako,
                         struct psf_normals *psf_n){
	long i, k, kst, ked;
	
	printf("area size: %e \n", psf_n->total_area);
	printf("average and RMS value  \n");
	for (i = 0; i < tako->nfield; i++) {
		kst = tako->istack_comp[i];
		ked = tako->istack_comp[i+1];
		for (k = kst; k < ked; k++){
			printf("field name: %s_%ld %ld %e %e \n", tako->data_name[i],k-kst+1,
					k, tako->d_ave[k], tako->d_rms[k]);
		}
	}
	return;
}

static void check_min_max_data_c(long nfield, long *istack_comp,
                                 char **data_name, double *d_min, double *d_max){
	long i, k, kst, ked;
	
	printf("min and max value  \n");
	for (i = 0; i < nfield; i++) {
		kst = istack_comp[i];
		ked = istack_comp[i+1];
		for (k = kst; k < ked; k++){
			printf("field name: %s_%ld %ld %e %e \n",
                   data_name[i], k-kst+1, k, d_min[k], d_max[k]);
		}
	}
	return;
}

void check_psf_min_max_c(struct psf_data *psf_s){
    check_min_max_data_c(psf_s->nfield, psf_s->istack_comp,
                         psf_s->data_name, psf_s->d_min, psf_s->d_max);
    return;
}

void check_fline_min_max_c(struct psf_data *psf_s,
                           struct psf_data *fline_d){
    check_min_max_data_c(psf_s->nfield, psf_s->istack_comp,
                         psf_s->data_name, fline_d->d_min, psf_s->d_max);
    return;
}

