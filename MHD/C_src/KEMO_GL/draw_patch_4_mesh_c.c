
/* draw_patch_4_mesh_c.c */

#include "draw_patch_4_mesh_c.h"

#define NPAGE 4096

struct mesh_sorting_work * alloc_mesh_sorting_work(long ntot_trans_patch, struct viewer_mesh *mesh_s){
    struct mesh_sorting_work * mesh_sort = (struct mesh_sorting_work *) malloc(sizeof(struct mesh_sorting_work));
    if(mesh_sort == NULL) {
        printf("malloc error for mesh_sorting_work\n");
        exit(0);
    }
    
    mesh_sort->nextP2_trans_patch = 1 + (int) log2((double) (ntot_trans_patch-1));
    mesh_sort->ntotP2_trans_patch =  1 << mesh_sort->nextP2_trans_patch;
    
    posix_memalign((void**)&mesh_sort->index_trans_patch, NPAGE,
                   mesh_sort->ntotP2_trans_patch*sizeof(long));
    if(mesh_sort->index_trans_patch == NULL) {
        printf("malloc error for index_trans_patch\n");
        exit(0);
    }
    posix_memalign((void**)&mesh_sort->z_trans_patch, NPAGE,
                   mesh_sort->ntotP2_trans_patch*sizeof(float));
    if(mesh_sort->z_trans_patch == NULL) {
        printf("malloc error for z_trans_patch\n");
        exit(0);
    }
    
    long num = mesh_s->nsurf_each_tri * mesh_s->nsurf_viewer;
    mesh_sort->z_ele_view = (double *)calloc(num,sizeof(double));
    if(mesh_sort->z_ele_view == NULL) {
        printf("malloc error for z_ele_view\n");
        exit(0);
    }
    return mesh_sort;
};

void dealloc_mesh_sorting_work(struct mesh_sorting_work *mesh_sort){
    free(mesh_sort->index_trans_patch);
    free(mesh_sort->z_trans_patch);
    free(mesh_sort->z_ele_view);
}

static void const_solid_mesh_patch_bufffer(int nthreads, int shading_mode,
                                           struct viewer_mesh *mesh_s,
                                           struct mesh_menu_val *mesh_m,
                                           struct gl_strided_buffer *mesh_solid_buf){
	mesh_s->ntot_solid_patch = count_solid_mesh_patches(mesh_s, mesh_m);
    if(mesh_s->ntot_solid_patch > 0){
        set_mesh_patch_colors(mesh_m, mesh_s);
        set_solid_mesh_patches_to_buf(mesh_m, mesh_s, mesh_s->iele_solid_patch);
    };
    
    set_buffer_address_4_patch(ITHREE*mesh_s->ntot_solid_patch, mesh_solid_buf);
	if(mesh_solid_buf->num_nod_buf <= 0) return;
    resize_strided_buffer(mesh_solid_buf);
    
    long num_tri = sel_mesh_patch_to_buffer_pthread(shading_mode, mesh_m->polygon_mode,
                                                    mesh_s, nthreads, IZERO,
                                                    mesh_s->ntot_solid_patch,
                                                    mesh_s->iele_solid_patch,
                                                    mesh_solid_buf);
	return;
}

void sort_transparent_mesh_patches(int nthreads, struct viewer_mesh *mesh_s,
                                   struct mesh_sorting_work *mesh_sort){
    long i;
    float rmax = max_Float_Array_pthreads(nthreads, mesh_s->ntot_trans_patch, mesh_sort->z_trans_patch);
    for(i=mesh_s->ntot_trans_patch;i<mesh_sort->ntotP2_trans_patch;i++){
        mesh_sort->z_trans_patch[i] = rmax + 10000.0;
        mesh_sort->index_trans_patch[i] = -1;
    }

    if(nthreads > 1){
        bitonicsort_Float_Pthread(nthreads, mesh_sort->ntotP2_trans_patch,
                                  mesh_sort->z_trans_patch, mesh_sort->index_trans_patch);
    }else{
        quicksort_real_c(mesh_sort->z_trans_patch, mesh_sort->index_trans_patch,
                         IZERO, mesh_sort->ntotP2_trans_patch-1);
    };
    
    for(i=0;i<mesh_s->ntot_trans_patch;i++){
        mesh_s->iele_trans_patch[i] = mesh_sort->index_trans_patch[i];
    }
    return;
}


void const_trans_mesh_buffer(int nthreads,
                             struct viewer_mesh *mesh_s,
                             struct mesh_menu_val *mesh_m,
                             struct view_element *view_s,
                             struct gl_strided_buffer *mesh_trns_buf){
    mesh_trns_buf->num_nod_buf = 0;
    if(mesh_m->iflag_draw_mesh == 0) return;

    
    mesh_s->ntot_trans_patch = count_transparent_mesh_patches(mesh_s, mesh_m);
    if(mesh_s->ntot_trans_patch > 0){
        set_mesh_patch_colors(mesh_m, mesh_s);
        struct mesh_sorting_work * mesh_sort = alloc_mesh_sorting_work(mesh_s->ntot_trans_patch, mesh_s);
        
        long num = mesh_s->nsurf_each_tri * mesh_s->nsurf_viewer;
        set_distance_in_model(view_s, num, mesh_s->surf_center_view, mesh_sort->z_ele_view);
        set_transparent_mesh_patches_to_buf(mesh_m, mesh_s,
                                            mesh_s->iele_trans_patch);
        set_trans_mesh_patch_for_sort(mesh_s, mesh_s->iele_trans_patch, mesh_sort->z_ele_view,
                                      mesh_sort->z_trans_patch, mesh_sort->index_trans_patch);
        sort_transparent_mesh_patches(nthreads, mesh_s, mesh_sort);
        dealloc_mesh_sorting_work(mesh_sort);
    };
    
    set_buffer_address_4_patch(ITHREE*mesh_s->ntot_trans_patch, mesh_trns_buf);
    if(mesh_trns_buf->num_nod_buf > 0){
        resize_strided_buffer(mesh_trns_buf);
        long num_tri = sel_mesh_patch_to_buffer_pthread(view_s->shading_mode,
                                                        mesh_m->polygon_mode,
                                                        mesh_s, nthreads, IZERO,
                                                        mesh_s->ntot_trans_patch,
                                                        mesh_s->iele_trans_patch,
                                                        mesh_trns_buf);
    };
    return;
};


static void const_mesh_grids_buffer(int nthreads,
                                    struct viewer_mesh *mesh_s,
                                    struct mesh_menu_val *mesh_m,
                                    struct gl_strided_buffer *mesh_buf){
    long num;
    num = mesh_s->num_pe_sf*mesh_s->ngrp_nod_sf+1;
    long *istack_edge_domain_edge =   (long *) calloc(num, sizeof(long));
    num = mesh_s->ngrp_ele_sf*mesh_s->ngrp_nod_sf+1;
    long *istack_ele_grp_edge =  (long *) calloc(num, sizeof(long));
    num = mesh_s->ngrp_surf_sf*mesh_s->ngrp_nod_sf+1;
    long *istack_surf_grp_edge = (long *) calloc(num, sizeof(long));

    long num_edge = count_mesh_grid_to_buf(mesh_s, mesh_m,
                                           istack_edge_domain_edge,
                                           istack_ele_grp_edge,
                                           istack_surf_grp_edge);
    
    set_buffer_address_4_patch(ITWO*num_edge, mesh_buf);
	if(mesh_buf->num_nod_buf <= 0) return;
	
	resize_strided_buffer(mesh_buf);
	set_mesh_grid_to_buf(nthreads, istack_edge_domain_edge,
                         istack_ele_grp_edge,
                         istack_surf_grp_edge,
                         mesh_s, mesh_m, mesh_buf);
    free(istack_edge_domain_edge);
    free(istack_ele_grp_edge);
    free(istack_surf_grp_edge);
	return;
}


static void const_mesh_nodes_ico_buffer(int nthreads, struct view_element *view_s,
                                        struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
                                        struct gl_strided_buffer *mesh_buf){
    long num;
    num = mesh_s->num_pe_sf*mesh_s->ngrp_nod_sf+1;
    long *istack_node_domain_patch =   (long *) calloc(num, sizeof(long));
    num = mesh_s->ngrp_nod_sf*mesh_s->ngrp_nod_sf+1;
    long *istack_node_nod_grp_patch =  (long *) calloc(num, sizeof(long));
    num = mesh_s->ngrp_ele_sf*mesh_s->ngrp_nod_sf+1;
    long *istack_node_ele_grp_patch =  (long *) calloc(num, sizeof(long));
    num = mesh_s->ngrp_surf_sf*mesh_s->ngrp_nod_sf+1;
    long *istack_node_surf_grp_patch = (long *) calloc(num, sizeof(long));
    
	long num_ico =   count_mesh_node_to_buf(mesh_s, mesh_m,
                                            istack_node_domain_patch,
                                            istack_node_nod_grp_patch,
                                            istack_node_ele_grp_patch,
                                            istack_node_surf_grp_patch);
    long num_patch = ITHREE * num_icosahedron_patch() * num_ico;
    
    
    double ref_diam = 4.0;
    double node_diam;
    if(mesh_m->node_diam <= 0.0){
        node_diam = ref_diam * set_tube_radius_by_axis(view_s);
    }else{
        node_diam = mesh_m->node_diam;
    };
    
    set_buffer_address_4_patch(num_patch, mesh_buf);
    if(mesh_buf->num_nod_buf > 0){
        resize_strided_buffer(mesh_buf);
        
        set_mesh_node_to_buf(nthreads, istack_node_domain_patch,
                             istack_node_nod_grp_patch,
                             istack_node_ele_grp_patch,
                             istack_node_surf_grp_patch,
                             node_diam, mesh_s, mesh_m, mesh_buf);
    };
    free(istack_node_domain_patch);
    free(istack_node_nod_grp_patch);
    free(istack_node_ele_grp_patch);
    free(istack_node_surf_grp_patch);
    return;
}

void const_solid_mesh_buffer(int nthreads,
                             struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
                             struct view_element *view_s,
                             struct gl_strided_buffer *mesh_solid_buf,
                             struct gl_strided_buffer *mesh_grid_buf,
                             struct gl_strided_buffer *mesh_node_buf){
    mesh_solid_buf->num_nod_buf = 0;
    mesh_grid_buf->num_nod_buf = 0;
    mesh_node_buf->num_nod_buf = 0;
    if(mesh_m->iflag_draw_mesh == 0) return;
        
    const_mesh_grids_buffer(nthreads, mesh_s, mesh_m, mesh_grid_buf);
    const_mesh_nodes_ico_buffer(nthreads, view_s, mesh_s, mesh_m, mesh_node_buf);
    const_solid_mesh_patch_bufffer(nthreads, view_s->shading_mode, mesh_s, mesh_m,
                                   mesh_solid_buf);
    return;
};
