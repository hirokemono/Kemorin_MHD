
/* draw_patch_4_mesh_c.c */

#include "draw_patch_4_mesh_c.h"

#define NPAGE 4096
#define NTHREADS 32


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

static void const_solid_mesh_patch_bufffer(int shading_mode,
                                           struct viewer_mesh *mesh_s,
                                           struct mesh_menu_val *mesh_m,
                                           struct gl_strided_buffer *mesh_buf){
	mesh_s->ntot_solid_patch = count_solid_mesh_patches(mesh_s, mesh_m);
    set_buffer_address_4_patch(ITHREE*mesh_s->ntot_solid_patch, mesh_buf);
	if(mesh_buf->num_nod_buf <= 0) return;
	
    resize_strided_buffer(mesh_buf);
    set_mesh_patch_colors(mesh_m, mesh_s);
	set_solid_mesh_patches_to_buf(mesh_m, mesh_s, mesh_s->iele_solid_patch);
    add_solid_mesh_patch_to_buf(shading_mode, mesh_m->polygon_mode, mesh_s, mesh_buf);
	return;
}

void sort_transparent_mesh_patches(struct viewer_mesh *mesh_s,
                                   struct mesh_sorting_work *mesh_sort){
    long i;
    float rmax = max_Float_Array_pthreads(NTHREADS, mesh_s->ntot_trans_patch, mesh_sort->z_trans_patch);
    for(i=mesh_s->ntot_trans_patch;i<mesh_sort->ntotP2_trans_patch;i++){
        mesh_sort->z_trans_patch[i] = rmax + 1.0;
        mesh_sort->index_trans_patch[i] = -1;
    }
    bitonicsort_Float_Pthread(NTHREADS, mesh_sort->ntotP2_trans_patch,
                              mesh_sort->z_trans_patch, mesh_sort->index_trans_patch);
    
    for(i=0;i<mesh_s->ntot_trans_patch;i++){
        mesh_s->iele_trans_patch[i] = mesh_sort->index_trans_patch[i];
    }
    return;
}


void const_trans_mesh_buffer(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
                             struct view_element *view_s, struct gl_strided_buffer *mesh_trns_buf){
    mesh_trns_buf->num_nod_buf = 0;
    if(mesh_m->iflag_draw_mesh == 0) return;

    
    mesh_s->ntot_trans_patch = count_transparent_mesh_patches(mesh_s, mesh_m);
    set_buffer_address_4_patch(ITHREE*mesh_s->ntot_trans_patch, mesh_trns_buf);

    if(mesh_trns_buf->num_nod_buf > 0){
        resize_strided_buffer(mesh_trns_buf);

        struct mesh_sorting_work * mesh_sort = alloc_mesh_sorting_work(mesh_s->ntot_trans_patch, mesh_s);
        set_mesh_patch_colors(mesh_m, mesh_s);
                
        long num = mesh_s->nsurf_each_tri * mesh_s->nsurf_viewer;
        set_distance_in_model(view_s, num, mesh_s->surf_center_view, mesh_sort->z_ele_view);
        set_transparent_mesh_patches_to_buf(mesh_m, mesh_s,
                                            mesh_sort->z_ele_view,
                                            mesh_s->iele_trans_patch,
                                            mesh_sort->z_trans_patch,
                                            mesh_sort->index_trans_patch);
        sort_transparent_mesh_patches(mesh_s, mesh_sort);
        dealloc_mesh_sorting_work(mesh_sort);

        add_trans_mesh_patch_to_buf(view_s->shading_mode, mesh_m->polygon_mode,
                                    mesh_s, mesh_trns_buf);
    };
    return;
};


static void const_mesh_grids_buffer(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
                                    struct gl_strided_buffer *mesh_buf){
	long num_edge = count_mesh_grid_to_buf(mesh_s, mesh_m);
    set_buffer_address_4_patch(ITWO*num_edge, mesh_buf);
	if(mesh_buf->num_nod_buf <= 0) return;
	
	resize_strided_buffer(mesh_buf);
	set_mesh_grid_to_buf(mesh_s, mesh_m, mesh_buf);
	return;
}


static void const_mesh_nodes_ico_buffer(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
                                        struct gl_strided_buffer *mesh_buf){
	long num_patch = count_mesh_node_to_buf(mesh_s, mesh_m);
    set_buffer_address_4_patch((ITHREE*num_patch), mesh_buf);
	if(mesh_buf->num_nod_buf <= 0) return;
	
	resize_strided_buffer(mesh_buf);
	set_mesh_node_to_buf(mesh_s, mesh_m, mesh_buf);
    return;
}

void const_solid_mesh_buffer(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m, struct view_element *view_s, 
                             struct gl_strided_buffer *mesh_solid_buf, struct gl_strided_buffer *mesh_grid_buf,
                             struct gl_strided_buffer *mesh_node_buf){
    mesh_solid_buf->num_nod_buf = 0;
    mesh_grid_buf->num_nod_buf = 0;
    mesh_node_buf->num_nod_buf = 0;
    if(mesh_m->iflag_draw_mesh == 0) return;
        
    const_mesh_grids_buffer(mesh_s, mesh_m, mesh_grid_buf);
    const_mesh_nodes_ico_buffer(mesh_s, mesh_m, mesh_node_buf);
    const_solid_mesh_patch_bufffer(view_s->shading_mode, mesh_s, mesh_m, mesh_solid_buf);
    return;
};
