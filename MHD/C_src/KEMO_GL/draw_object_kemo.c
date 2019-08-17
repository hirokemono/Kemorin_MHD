
/* draw_object_kemo.c */

#include <OpenGL/gl3.h>
#include "draw_object_kemo.h"


void draw_mesh_edges_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids *mesh_VAO, 
			struct kemoview_shaders *kemo_shaders, struct gl_strided_buffer *mesh_buf){
	int i, ip_st;
	
	set_buffer_address_4_patch(3*128, mesh_buf);
	alloc_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	for(i=0; i < mesh_s->num_pe_sf;i++){mesh_s->ip_domain_far[i] = i+1;};
	
	if(mesh_m->draw_surface_grid != 0){
		mesh_edge_VBO(view_s, mesh_m->domain_grid_color, mesh_m->mesh_color_mode,
					   mesh_m->num_of_color_loop, mesh_m->domain_grid_color_code,
					   mesh_s->num_pe_sf, mesh_s->edge_stack_domain_sf, 
					   mesh_s->edge_item_domain_sf, IZERO, mesh_m->draw_domains_grid,
					   mesh_s, mesh_VAO, kemo_shaders, mesh_buf);
	};
	
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_elegrp_grid[i] ){
			mesh_edge_VBO(view_s, mesh_m->ele_grid_color, mesh_m->mesh_color_mode, 
						   mesh_m->num_of_color_loop, mesh_m->ele_grid_color_code,
						   mesh_s->ngrp_ele_sf, &mesh_s->ele_edge_stack_sf[ip_st],
						   mesh_s->ele_edge_item_sf, i, mesh_m->always_draw_domains,
						   mesh_s, mesh_VAO, kemo_shaders, mesh_buf);
		};
	};
	
	/* ! draw surface group */
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_surfgrp_grid[i] ){
			mesh_edge_VBO(view_s, mesh_m->surf_grid_color, mesh_m->mesh_color_mode,
						   mesh_m->num_of_color_loop, mesh_m->surf_grid_color_code,
						   mesh_s->ngrp_surf_sf, &mesh_s->surf_edge_stack_sf[ip_st], 
						   mesh_s->surf_edge_item_sf, i, mesh_m->always_draw_domains, 
						   mesh_s, mesh_VAO, kemo_shaders, mesh_buf);
		};
	};
	
	free(mesh_buf->v_buf);
	return;
}

