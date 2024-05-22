/*
//  m_kemoview_mesh_buffers.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include "m_kemoview_mesh_buffers.h"

struct MESH_buffers * init_MESH_buffers(void)
{
    struct MESH_buffers *MESH_bufs = (struct MESH_buffers *) malloc(sizeof(struct MESH_buffers));
    if(MESH_bufs == NULL){
        printf("malloc error in MESH_buffers \n");
        exit(0);
    };
    
    long n_point = 1024;
    MESH_bufs->mesh_solid_buf =  init_strided_buffer(n_point);
    MESH_bufs->mesh_grid_buf =   init_strided_buffer(n_point);
    MESH_bufs->mesh_node_buf =   init_strided_buffer(n_point);
    return MESH_bufs;
}

void dealloc_MESH_buffers(struct MESH_buffers *MESH_bufs)
{
    dealloc_strided_buffer(MESH_bufs->mesh_solid_buf);
    dealloc_strided_buffer(MESH_bufs->mesh_grid_buf);
    dealloc_strided_buffer(MESH_bufs->mesh_node_buf);
    free(MESH_bufs);
};

void const_solid_mesh_buffer(int nthreads,
                             struct viewer_mesh *mesh_s, 
                             struct mesh_menu_val *mesh_m,
                             struct view_element *view_s,
                             struct MESH_buffers *MESH_bufs){
    MESH_bufs->mesh_solid_buf->num_nod_buf = 0;
    MESH_bufs->mesh_grid_buf->num_nod_buf = 0;
    MESH_bufs->mesh_node_buf->num_nod_buf = 0;
    if(mesh_m->iflag_draw_mesh == 0) return;
        
    const_mesh_grids_buffer(nthreads, mesh_s, mesh_m, MESH_bufs->mesh_grid_buf);
    const_mesh_nodes_ico_buffer(nthreads, view_s, mesh_s, mesh_m, MESH_bufs->mesh_node_buf);
    const_solid_mesh_patch_bufffer(nthreads, view_s->shading_mode, mesh_s, mesh_m,
                                   MESH_bufs->mesh_solid_buf);
    return;
};

