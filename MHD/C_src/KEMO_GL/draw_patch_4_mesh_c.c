
/* draw_patch_4_mesh_c.c */

#include "draw_patch_4_mesh_c.h"


static void const_solid_mesh_patch_bufffer(int shading_mode,
                                           struct viewer_mesh *mesh_s,
                                           struct mesh_menu_val *mesh_m,
                                           struct gl_strided_buffer *mesh_buf){
	int num_patch = count_solid_mesh_patches(mesh_s, mesh_m);
    set_buffer_address_4_patch(ITHREE*num_patch, mesh_buf);
	if(mesh_buf->num_nod_buf <= 0) return;
	
	resize_strided_buffer(mesh_buf);
	set_solid_mesh_patches_to_buf(shading_mode, mesh_s, mesh_m, mesh_buf);
	return;
}


void const_trans_mesh_buffer(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
                             struct view_element *view_s, struct gl_strided_buffer *mesh_trns_buf){
    int num_patch;

    mesh_trns_buf->num_nod_buf = 0;
    if(mesh_m->iflag_draw_mesh == 0) return;

    if(mesh_m->domain_opacity < 1.0
                || mesh_m->ele_grp_opacity < 1.0
                || mesh_m->surf_grp_opacity < 1.0){
        sort_by_patch_distance_mesh(mesh_s, view_s);
    } else {
        copy_patch_distance_mesh(mesh_s);
    }
    
    num_patch = count_transparent_mesh_patches(mesh_s, mesh_m);
    set_buffer_address_4_patch(ITHREE*num_patch, mesh_trns_buf);

    if(mesh_trns_buf->num_nod_buf > 0){
        resize_strided_buffer(mesh_trns_buf);
        set_transparent_mesh_patches_to_buf(view_s->shading_mode, mesh_s, mesh_m, mesh_trns_buf);
    };
    return;
};

void set_trans_mesh_VAO(struct gl_strided_buffer *mesh_trns_buf, struct VAO_ids *mesh_trans_VAO){
    mesh_trans_VAO->npoint_draw = mesh_trns_buf->num_nod_buf;
    if(mesh_trans_VAO->npoint_draw > 0){Const_VAO_4_Phong(mesh_trans_VAO, mesh_trns_buf);};
	return;
};


static void const_mesh_grids_buffer(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
                                    struct gl_strided_buffer *mesh_buf){
	int num_edge = count_mesh_grid_to_buf(mesh_s, mesh_m);
    set_buffer_address_4_patch(ITWO*num_edge, mesh_buf);
	if(mesh_buf->num_nod_buf <= 0) return;
	
	resize_strided_buffer(mesh_buf);
	set_mesh_grid_to_buf(mesh_s, mesh_m, mesh_buf);
	return;
}


static void const_mesh_nodes_ico_buffer(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
                                        struct gl_strided_buffer *mesh_buf){
	int num_patch = count_mesh_node_to_buf(mesh_s, mesh_m);
    set_buffer_address_4_patch((ITHREE*num_patch), mesh_buf);
	if(mesh_buf->num_nod_buf <= 0) return;
	
	resize_strided_buffer(mesh_buf);
	set_mesh_node_to_buf(mesh_s, mesh_m, mesh_buf);
    return;
}

void const_solid_mesh_buffer(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m, struct view_element *view_s, 
                             struct gl_strided_buffer *mesh_solid_buf, struct gl_strided_buffer *mesh_grid_buf,
                             struct gl_strided_buffer *mesh_node_buf){
    int i;
    mesh_solid_buf->num_nod_buf = 0;
    mesh_grid_buf->num_nod_buf = 0;
    mesh_node_buf->num_nod_buf = 0;
    if(mesh_m->iflag_draw_mesh == 0) return;
    
    for(i=0; i < mesh_s->num_pe_sf;i++){mesh_s->ip_domain_far[i] = i+1;};
    
    const_mesh_grids_buffer(mesh_s, mesh_m, mesh_grid_buf);
    const_mesh_nodes_ico_buffer(mesh_s, mesh_m, mesh_node_buf);
    copy_patch_distance_mesh(mesh_s);
    const_solid_mesh_patch_bufffer(view_s->shading_mode, mesh_s, mesh_m, mesh_solid_buf);
    return;
};

void set_solid_mesh_VAO(struct gl_strided_buffer *mesh_solid_buf, struct gl_strided_buffer *mesh_grid_buf,
                        struct gl_strided_buffer *mesh_node_buf, struct VAO_ids **mesh_VAO){
    mesh_VAO[0]->npoint_draw = mesh_solid_buf->num_nod_buf;
    if(mesh_VAO[0]->npoint_draw > 0){Const_VAO_4_Phong(mesh_VAO[0], mesh_solid_buf);};
    mesh_VAO[1]->npoint_draw = mesh_grid_buf->num_nod_buf;
    if(mesh_VAO[1]->npoint_draw > 0){Const_VAO_4_Simple(mesh_VAO[1], mesh_grid_buf);};
    mesh_VAO[2]->npoint_draw = mesh_node_buf->num_nod_buf;
    if(mesh_VAO[2]->npoint_draw > 0){Const_VAO_4_Phong(mesh_VAO[2], mesh_node_buf);};
	return;
};
