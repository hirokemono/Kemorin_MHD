
/* sort_by_patch_distance.c */

#include <Accelerate/Accelerate.h>
#include <time.h>
#include "sort_by_patch_distance.h"

void copy_patch_distance_mesh(struct viewer_mesh *mesh_s){
	int i;
	
	for(i=0; i < mesh_s->num_pe_sf;i++){mesh_s->ip_domain_far[i] = i+1;};
	
	for(i=0; i < mesh_s->nsurf_domain_sf;i++) mesh_s->iele_domain_far[i] = i;
	for(i=0; i < mesh_s->nele_ele_sf;i++) mesh_s->iele_grp_far[i] = i;
	for(i=0; i < mesh_s->nsurf_surf_sf;i++) mesh_s->isurf_grp_far[i] = i;
	
	return;
}

void sort_by_patch_distance_mesh(struct viewer_mesh *mesh_s, struct view_element *view_s){
	int ip, i, j;
    long num, ist, ied;

	num = mesh_s->nsurf_each_tri * mesh_s->nsurf_viewer;
	set_distance_in_model(view_s, num, mesh_s->surf_center_view,  mesh_s->z_ele_view);
	set_distance_in_model(view_s, (long) mesh_s->num_pe_sf, 
                          mesh_s->domain_center,  mesh_s->z_center_view);
	
	for(i=0; i < mesh_s->num_pe_sf;i++){
		mesh_s->ip_domain_far[i] = i+1;
	};	
	for(i=0; i < mesh_s->nsurf_domain_sf;i++){
		j = mesh_s->isurf_domain_sf[i] * mesh_s->nsurf_each_tri;
		mesh_s->iele_domain_far[i] = i;
		mesh_s->z_domain_view[i] = mesh_s->z_ele_view[j];
	};
	for(i=0; i < mesh_s->nele_ele_sf;i++) {
		j = mesh_s->ele_item_sf[i] * mesh_s->nsurf_each_tri;
		mesh_s->iele_grp_far[i] = i;
		mesh_s->z_ele_grp_view[i] = mesh_s->z_ele_view[j];
	};
	for(i=0; i < mesh_s->nsurf_surf_sf;i++) {
		j = mesh_s->surf_item_sf[i] * mesh_s->nsurf_each_tri;
		mesh_s->isurf_grp_far[i] = i;
		mesh_s->z_surf_grp_view[i] = mesh_s->z_ele_view[j];
	};
	
	ied = mesh_s->num_pe_sf - 1;
	quicksort_double_c(mesh_s->z_center_view, mesh_s->ip_domain_far, IZERO, ied);
	
	for(ip=0; ip<mesh_s->num_pe_sf;ip++){
		ist = mesh_s->isurf_stack_domain_sf[ip];
		ied = mesh_s->isurf_stack_domain_sf[ip+1]-1;
		quicksort_double_c(mesh_s->z_domain_view, mesh_s->iele_domain_far, ist, ied);
	}
	
	for(j=0; j < mesh_s->ngrp_ele_sf;j++) {
		for(ip=0; ip < mesh_s->num_pe_sf;ip++){
			i = ip + j*mesh_s->num_pe_sf;
			ist = mesh_s->ele_stack_sf[i];
			ied = mesh_s->ele_stack_sf[i+1]-1;
			quicksort_double_c(mesh_s->z_ele_grp_view, mesh_s->iele_grp_far, ist, ied);
		}
	}
	
	for(j=0; j < mesh_s->ngrp_surf_sf;j++) {
		for(ip=0; ip < mesh_s->num_pe_sf;ip++){
			i = ip + j*mesh_s->num_pe_sf;
			ist = mesh_s->surf_stack_sf[i];
			ied = mesh_s->surf_stack_sf[i+1]-1;
			quicksort_double_c(mesh_s->z_surf_grp_view, mesh_s->isurf_grp_far, ist, ied);
		}
	}
	/*
	for(ip=0; ip<mesh_s->num_pe_sf;ip++){
		ist = mesh_s->isurf_stack_domain_sf[ip];
		ied = mesh_s->isurf_stack_domain_sf[ip+1]-1;
		for(i=ist;i<ied+1;i++){
			j = mesh_s->iele_domain_far[i];
			printf("mesh_s->iele_domain_far %d %d\n",i,mesh_s->isurf_domain_sf[j]);
		}
	}
	*/
	return;
}


int sort_by_patch_distance_psfs(struct psf_data **psf_s, struct psf_menu_val **psf_m, 
                                 struct kemo_array_control *psf_a, struct view_element *view_s){
    long ntot_tmp;
    int i, iele;
    long icou;
    long icou_solid_psf, icou_trans_psf, icou_solid_txt, icou_trans_txt;
    
    icou = 0;
    
    ntot_tmp = psf_a->ntot_psf_patch;
    psf_a->istack_solid_psf_patch = 0;
    psf_a->istack_solid_psf_txtur = 0;
    psf_a->istack_trans_psf_patch = 0;
    psf_a->istack_trans_psf_txtur = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_a->iflag_loaded[i] != 0 && psf_m[i]->draw_psf_solid != 0){ 
            if(psf_m[i]->cmap_psf_comp[psf_m[i]->icomp_draw_psf]->min_opacity >= 1.0){
                if(psf_m[i]->psf_patch_color == TEXTURED_SURFACE){
                    psf_a->istack_solid_psf_txtur = psf_a->istack_solid_psf_txtur + psf_s[i]->nele_viz;
                } else {
                    psf_a->istack_solid_psf_patch = psf_a->istack_solid_psf_patch + psf_s[i]->nele_viz;
                };
            } else {
                if(psf_m[i]->psf_patch_color == TEXTURED_SURFACE){
                    psf_a->istack_trans_psf_txtur = psf_a->istack_trans_psf_txtur + psf_s[i]->nele_viz;
                } else {
                    psf_a->istack_trans_psf_patch = psf_a->istack_trans_psf_patch + psf_s[i]->nele_viz;
                };
            };
        };
    };
    psf_a->istack_solid_psf_patch = psf_a->istack_solid_psf_patch + psf_a->istack_solid_psf_txtur;
    psf_a->istack_trans_psf_txtur = psf_a->istack_trans_psf_txtur + psf_a->istack_solid_psf_patch;
    psf_a->istack_trans_psf_patch = psf_a->istack_trans_psf_patch + psf_a->istack_trans_psf_txtur;
    psf_a->ntot_psf_patch = psf_a->istack_trans_psf_patch;
    
    if(psf_a->ntot_psf_patch > ntot_tmp){
        dealloc_psfs_sorting_list(psf_a);
        alloc_psfs_sorting_list(psf_a);
    };
    
    icou_solid_txt = 0;
    icou_solid_psf = psf_a->istack_solid_psf_txtur;
    icou_trans_txt = psf_a->istack_solid_psf_patch;
    icou_trans_psf = psf_a->istack_trans_psf_txtur;
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_a->iflag_loaded[i] != 0 && psf_m[i]->draw_psf_solid != 0){ 
            if(psf_m[i]->cmap_psf_comp[psf_m[i]->icomp_draw_psf]->min_opacity >= 1.0){
                if(psf_m[i]->psf_patch_color == TEXTURED_SURFACE){
                    icou = icou_solid_txt;
                    icou_solid_txt = icou_solid_txt + psf_s[i]->nele_viz;
                } else {
                    icou = icou_solid_psf;
                    icou_solid_psf = icou_solid_psf + psf_s[i]->nele_viz;
                };
            } else {
                if(psf_m[i]->psf_patch_color == TEXTURED_SURFACE){
                    icou = icou_trans_txt;
                    icou_trans_txt = icou_trans_txt + psf_s[i]->nele_viz;
                } else {
                    icou = icou_trans_psf;
                    icou_trans_psf = icou_trans_psf + psf_s[i]->nele_viz;
                };
            };
            
            set_distance_in_model(view_s, psf_s[i]->nele_viz, psf_s[i]->x_ele_viz,
                                  &psf_a->z_ele_viz[icou]);

            for(iele=0; iele < psf_s[i]->nele_viz;iele++) {
                psf_a->ipsf_viz_far[icou] = i+1;
                psf_a->iele_viz_far[icou] = iele+1;
                icou = icou+1;
            };
        };
    };
    
    struct tm *st_time, *ed_time;
    struct timeval start_usec, end_usec;
    int sec_D, usec_D;
    int sec_Q, usec_Q;

    int j;
    int *idx_tmp = (int *) calloc(psf_a->ntot_psf_patch,sizeof(int));
    int *iele_tmp = (int *) calloc(psf_a->ntot_psf_patch,sizeof(int));
    int *ipsf_tmp = (int *) calloc(psf_a->ntot_psf_patch,sizeof(int));
    double *z_tmp = (double *) calloc(psf_a->ntot_psf_patch,sizeof(double));
    double *y_tmp = (double *) calloc(psf_a->ntot_psf_patch,sizeof(double));
    double *w_tmp = (double *) calloc(psf_a->ntot_psf_patch,sizeof(double));
    long *ldx_tmp = (long *) calloc(psf_a->ntot_psf_patch,sizeof(long));
    int *kdx_tmp = (int *) calloc(psf_a->ntot_psf_patch,sizeof(int));
//    vDSP_Length *kdx_tmp = (vDSP_Length *) calloc(psf_a->ntot_psf_patch,sizeof(vDSP_Length));
    vDSP_Length *i_unused = (vDSP_Length *) calloc(psf_a->ntot_psf_patch,sizeof(vDSP_Length));
    long lnum = psf_a->istack_trans_psf_patch - psf_a->istack_solid_psf_patch;
    if( (psf_a->istack_trans_psf_patch - psf_a->istack_solid_psf_patch) > 0){
        if(gettimeofday(&start_usec, NULL) == -1){
            fprintf(stderr,"gettimeofday ERRNO=%d", errno);
            return -1;
        }
        for(i=0; i<lnum; i++){
            w_tmp[i] = psf_a->z_ele_viz[i+psf_a->istack_solid_psf_patch];
            kdx_tmp[i] = i;
        };
        quicksort_double_c(w_tmp, kdx_tmp, IZERO, (lnum-1));
//        vDSP_vsortiD(&y_tmp[psf_a->istack_solid_psf_patch],
//                     &ldx_tmp[psf_a->istack_solid_psf_patch],
//                     i_unused, lnum, 0);
        if(gettimeofday(&end_usec, NULL) == -1){
            fprintf(stderr,"gettimeofday ERRNO=%d", errno);
            return -1;
        }
        for(i=0; i<psf_a->istack_solid_psf_patch; i++){
            ldx_tmp[i] = i;
            y_tmp[i] =   psf_a->z_ele_viz[i];
        };
        for(i=0; i<lnum; i++){
            ldx_tmp[i+psf_a->istack_solid_psf_patch] = kdx_tmp[i]
                                                      + psf_a->istack_solid_psf_patch;
            y_tmp[i+psf_a->istack_solid_psf_patch] =   w_tmp[i];
        };
        st_time = localtime(&start_usec.tv_sec);
        ed_time = localtime(&end_usec.tv_sec);
        sec_D = ed_time->tm_sec - st_time->tm_sec;
        usec_D = end_usec.tv_usec - start_usec.tv_usec;

        if(gettimeofday(&start_usec, NULL) == -1){
            fprintf(stderr,"gettimeofday ERRNO=%d", errno);
            return -1;
        }
        for(i=0; i<psf_a->ntot_psf_patch; i++){
            z_tmp[i] = psf_a->z_ele_viz[i];
            idx_tmp[i] = i;
        };
        quicksort_double_c(z_tmp, idx_tmp,
                           psf_a->istack_solid_psf_patch, (psf_a->ntot_psf_patch-1));
        if(gettimeofday(&end_usec, NULL) == -1){
            fprintf(stderr,"gettimeofday ERRNO=%d", errno);
            return -1;
        }
        st_time = localtime(&start_usec.tv_sec);
        ed_time = localtime(&end_usec.tv_sec);
        st_time = localtime(&start_usec.tv_sec);
        ed_time = localtime(&end_usec.tv_sec);
        sec_Q = ed_time->tm_sec - st_time->tm_sec;
        usec_Q = end_usec.tv_usec - start_usec.tv_usec;
        
        for(i=0; i<psf_a->ntot_psf_patch; i++){
            if(idx_tmp[i] != (int) ldx_tmp[i]){
                printf("Failed vDSP_vsortiD %d %d %d %lf %lf \n", i,
                       idx_tmp[i], (int) ldx_tmp[i],
                       z_tmp[i], y_tmp[ldx_tmp[i]]);
            }
        };
        printf("vDSP_vsortiD %02d.%d\n",sec_D,usec_D);
        printf("quicksort_double_c %02d.%d\n",sec_Q,usec_Q);


        for(i=0; i<psf_a->ntot_psf_patch; i++){
            j = idx_tmp[i];
            iele_tmp[i] = psf_a->iele_viz_far[j];
            ipsf_tmp[i] = psf_a->ipsf_viz_far[j];
        };
        for(i=0; i<psf_a->ntot_psf_patch; i++){
            psf_a->iele_viz_far[i] = iele_tmp[i];
            psf_a->ipsf_viz_far[i] = ipsf_tmp[i];
        };
    };
    free(i_unused);
    free(ldx_tmp);
    free(y_tmp);
    free(iele_tmp);
    free(ipsf_tmp);
    free(z_tmp);

    return (int) psf_a->ntot_psf_patch;
}
