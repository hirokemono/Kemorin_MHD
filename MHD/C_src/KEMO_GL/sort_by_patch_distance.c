
/* sort_by_patch_distance.c */

#include <time.h>
#include "sort_by_patch_distance.h"

const int numThread = 16;

void copy_patch_distance_mesh(struct viewer_mesh *mesh_s){
    int i;
    
    for(i=0; i < mesh_s->num_pe_sf;i++){mesh_s->ip_domain_far[i] = i+1;};
    
    for(i=0; i < mesh_s->nsurf_domain_sf;i++){mesh_s->iele_domain_far[i] = i;};
    for(i=0; i < mesh_s->nele_ele_sf;i++){    mesh_s->iele_grp_far[i] = i;};
    for(i=0; i < mesh_s->nsurf_surf_sf;i++){  mesh_s->isurf_grp_far[i] = i;};
	
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

void count_patch_distance_psfs(struct psf_data **psf_s, struct psf_menu_val **psf_m, 
                               struct kemo_array_control *psf_a){
    long ntot_tmp;
    int i;
    
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
    return;
};

void set_patch_indices_for_psfs(struct psf_data **psf_s,
                                struct psf_menu_val **psf_m,
                                struct kemo_array_control *psf_a){
    int i, iele;
    long icou;
    long icou_solid_psf, icou_trans_psf, icou_solid_txt, icou_trans_txt;
    
    icou = 0;
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
            for(iele=0; iele < psf_s[i]->nele_viz;iele++) {
                psf_a->ipsf_viz_far[icou] = i+1;
                psf_a->iele_viz_far[icou] = iele+1;
                icou = icou+1;
            };
        };
    };
    return;
};

void set_trans_patch_distance_psfs(struct view_element *view_s,
                                   struct psf_data **psf_s,
                                   struct kemo_array_control *psf_a,
                                   double *z_ele_viz){
    int i, ipsf;
    long icou = 0;
    long jcou = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(icou >= psf_a->ntot_psf_patch) break;
        if(icou >= psf_a->istack_solid_psf_patch){
            ipsf = psf_a->ipsf_viz_far[icou] - 1;
            jcou = icou - psf_a->istack_solid_psf_patch;
            set_distance_in_model(view_s, psf_s[ipsf]->nele_viz,
                                  psf_s[ipsf]->x_ele_viz, &z_ele_viz[jcou]);
        };
        icou = icou + psf_s[ipsf]->nele_viz;
    }
    return;
};


#ifdef __APPLE__
const int sort_zbuf_ele_by_vDSP(long lnum, double *z_ele_viz, long *ldx_tmp){
    int i;
    
    vDSP_Length *i_unused = (vDSP_Length *) calloc(lnum,sizeof(vDSP_Length));
    vDSP_Length *kdx_tmp = (vDSP_Length *) calloc(lnum, sizeof(vDSP_Length));
    for(i=0;i<lnum;i++){kdx_tmp[i] = i;};
    
    vDSP_vsortiD(z_ele_viz, kdx_tmp, i_unused, lnum, 1);
    
    for(i=0; i<lnum; i++){ldx_tmp[i] = kdx_tmp[i];};
    free(i_unused);
    free(kdx_tmp);
    return 0;
}
#endif

const int sort_zbuf_ele_by_bitonic(long lnum, long narrayP2, double *z_ele_viz, long *ldx_tmp){
    long i;
    
    double max_z =  max_Double_Array_pthreads(numThread, lnum, z_ele_viz);
    for(i=0;i<lnum;i++){ldx_tmp[i] = i;};
    for(i=lnum;i<narrayP2;i++){
        z_ele_viz[i] = max_z + 1.0;
        ldx_tmp[i] = -1;
    };
    
    bitonicsort_Double_Pthread(numThread, narrayP2, z_ele_viz, ldx_tmp);
    return 0;
}

const int sort_zbuf_ele_by_quicksort(long lnum, double *z_ele_viz, long *idx_tmp){
/*
    struct tm *st_time, *ed_time;
    struct timeval start_usec, end_usec;
    int sec_Q, usec_Q;
    if(gettimeofday(&start_usec, NULL) == -1){
        fprintf(stderr,"gettimeofday ERRNO=%d", errno);
        return -1;
    }
*/
    for(int i=0; i<lnum; i++){idx_tmp[i] = i;};
    quicksort_double_c(z_ele_viz, idx_tmp, ZERO, (lnum-1));
/*
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
    printf("quicksort_double_c %02d.%d\n",sec_Q,usec_Q);
*/
    return 0;
}

void set_patch_order_by_distance(long lnum, const long *idx_tmp, struct kemo_array_control *psf_a){
    long i, j;
    int *iele_tmp = (int *) calloc(lnum,sizeof(int));
    int *ipsf_tmp = (int *) calloc(lnum,sizeof(int));
    for(i=0; i<lnum; i++){
        j = idx_tmp[i];
        iele_tmp[i] = psf_a->iele_viz_far[j+psf_a->istack_solid_psf_patch];
        ipsf_tmp[i] = psf_a->ipsf_viz_far[j+psf_a->istack_solid_psf_patch];
    };
    for(i=0; i<lnum; i++){
        psf_a->iele_viz_far[i+psf_a->istack_solid_psf_patch] = iele_tmp[i];
        psf_a->ipsf_viz_far[i+psf_a->istack_solid_psf_patch] = ipsf_tmp[i];
    };
    free(iele_tmp);
    free(ipsf_tmp);
    return;
}

void select_sort_zbuf_ele(long lnum, long narrayP2, double *z_ele_viz, long *idx_tmp){
    if(lnum < 65536){
#ifdef __APPLE__
        sort_zbuf_ele_by_vDSP(lnum, z_ele_viz, idx_tmp);
#else
        sort_zbuf_ele_by_quicksort(lnum, z_ele_viz, idx_tmp);
#endif
    }else{
        sort_zbuf_ele_by_bitonic(lnum, narrayP2, z_ele_viz, idx_tmp);
    };
    return;
}

int sort_by_patch_distance_psfs(struct psf_data **psf_s, struct psf_menu_val **psf_m,
                                 struct kemo_array_control *psf_a, struct view_element *view_s){
    count_patch_distance_psfs(psf_s, psf_m, psf_a);
    set_patch_indices_for_psfs(psf_s, psf_m, psf_a);
    
    long lnum = psf_a->istack_trans_psf_patch - psf_a->istack_solid_psf_patch;
    if(lnum > 0){
        int nextP2 =  1 + (int) log2((double) (lnum-1));
        long narrayP2 =  1 << nextP2;
        double *z_ele_viz = (double *)calloc(narrayP2,sizeof(double));
        long *idx_tmp = (long *) calloc(narrayP2,sizeof(long));
        
        set_trans_patch_distance_psfs(view_s, psf_s, psf_a, z_ele_viz);
        
        select_sort_zbuf_ele(lnum, narrayP2, z_ele_viz, idx_tmp);
        set_patch_order_by_distance(lnum, idx_tmp, psf_a);
        free(z_ele_viz);
        free(idx_tmp);
    };
    return (int) psf_a->ntot_psf_patch;
}
