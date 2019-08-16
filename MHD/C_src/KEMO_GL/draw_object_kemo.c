
/* draw_object_kemo.c */

#include <OpenGL/gl3.h>
#include "draw_object_kemo.h"


int draw_objects_4_map(struct psf_data **psf_s, struct mesh_menu_val *mesh_m,
                       struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                       struct view_element *view_s, struct buffer_for_gl *gl_buf){
    int i;
    int iflag_map = 0;
	GLdouble xwin, ywin;
	    
	/* set shading mode */
	glShadeModel(GL_SMOOTH);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glDisable(GL_CULL_FACE);
	
	if(view_s->ny_window > view_s->nx_window) {
		xwin = 2.05;
		ywin = 2.05 * (GLdouble)view_s->ny_window / (GLdouble)view_s->nx_window;
	} else{
		xwin = 1.7 * (GLdouble)view_s->nx_window / (GLdouble)view_s->ny_window;
		ywin = 1.7;
	}
	
	orthogonalGL(-xwin, xwin, -ywin, ywin, -1.0, 1.0);
	set_view_by_identity();
	
	load_projection_matrix(view_s);
	modify_view_by_struct(view_s);
	return iflag_map;
}


void draw_nodes_ico_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids *mesh_VAO, 
			struct kemoview_shaders *kemo_shaders, struct gl_strided_buffer *mesh_buf){
	int i, ip_st;
	
	set_buffer_address_4_patch(3*128, mesh_buf);
	alloc_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	node_ico_VBO(view_s, mesh_s->num_pe_sf, IZERO, mesh_s->nod_stack_domain_sf,
				mesh_s->nod_item_domain_sf, mesh_s,
				mesh_m->node_diam, mesh_m->domain_node_color,
				mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
				mesh_m->domain_node_color_code, mesh_m->draw_domains_nod, 
				mesh_VAO, kemo_shaders, mesh_buf);
	
	/* ! draw node group */
	
	for (i = 0; i < mesh_s->ngrp_nod_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_nodgrp_nod[i] ){
	
			node_ico_VBO(view_s, mesh_s->ngrp_nod_sf, i, &mesh_s->nod_stack_sf[ip_st],
						mesh_s->nod_item_sf, mesh_s,
						mesh_m->node_diam, mesh_m->node_node_color,
						mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
						mesh_m->node_node_color_code, mesh_m->always_draw_domains, 
						mesh_VAO, kemo_shaders, mesh_buf);
	
		};
	};
	
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_elegrp_nod[i] ){
			node_ico_VBO(view_s, mesh_s->ngrp_ele_sf, i, &mesh_s->ele_nod_stack_sf[ip_st],
						mesh_s->ele_nod_item_sf, mesh_s,
						mesh_m->node_diam, mesh_m->ele_node_color,
						mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
						mesh_m->ele_node_color_code, mesh_m->always_draw_domains, 
						mesh_VAO, kemo_shaders, mesh_buf);
	
		};
	};
	
	/* ! draw surface group */
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_surfgrp_nod[i] ){
			node_ico_VBO(view_s, mesh_s->ngrp_surf_sf, i, &mesh_s->surf_nod_stack_sf[ip_st],
						mesh_s->surf_nod_item_sf, mesh_s,
						mesh_m->node_diam, mesh_m->surf_node_color,
						mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
						mesh_m->surf_node_color_code, mesh_m->always_draw_domains, 
						mesh_VAO, kemo_shaders, mesh_buf);
	
		};
	};
	
	free(mesh_buf->v_buf);
	return;
}


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

void draw_mesh_patches_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids *mesh_VAO, 
			struct kemoview_shaders *kemo_shaders, struct gl_strided_buffer *mesh_buf){
	int i, ip_st;
	
	copy_patch_distance_mesh(mesh_s);
	
/*
	for(i=0;i<mesh_s->nsurf_domain_sf * mesh_s->nsurf_each_tri;i++){
		printf("%d, %f %f %f \n", i, mesh_s->normal_domain[i][0],
		mesh_s->normal_domain[i][1], mesh_s->normal_domain[i][2]);
	}
*/
	
	set_buffer_address_4_patch(3*128, mesh_buf);
	alloc_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	if(mesh_m->draw_surface_solid != 0 && mesh_m->domain_opacity >= 1.0){
		mesh_patch_VBO(view_s, mesh_m->shading_mode, mesh_m->polygon_mode, 
				mesh_m->domain_surface_color, mesh_m->mesh_color_mode,
				mesh_m->num_of_color_loop, mesh_m->domain_opacity,
				mesh_m->domain_surface_color_code, 
				mesh_s->num_pe_sf, mesh_s->isurf_stack_domain_sf, 
				mesh_s->isurf_domain_sf, mesh_s->normal_domain, mesh_s->norm_nod_domain,
				mesh_s->iele_domain_far, mesh_s->ip_domain_far,
				IZERO, mesh_s, mesh_m->draw_domains_solid, mesh_VAO, kemo_shaders, mesh_buf);
	};
	
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_elegrp_solid[i] != 0 && mesh_m->ele_grp_opacity >= 1.0){
			mesh_patch_VBO(view_s, mesh_m->shading_mode, mesh_m->polygon_mode, 
					mesh_m->ele_surface_color, mesh_m->mesh_color_mode,
					mesh_m->num_of_color_loop, mesh_m->ele_grp_opacity,
					mesh_m->ele_surface_color_code, 
					mesh_s->ngrp_ele_sf, &mesh_s->ele_stack_sf[ip_st],
					mesh_s->ele_item_sf, mesh_s->normal_ele_grp, mesh_s->norm_nod_ele_grp,
					mesh_s->iele_grp_far, mesh_s->ip_domain_far,
					i, mesh_s, mesh_m->always_draw_domains, mesh_VAO, kemo_shaders, mesh_buf);
		};
	};
	
	/* ! draw surface group */
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_surfgrp_solid[i] != 0 && mesh_m->surf_grp_opacity >= 1.0){
			mesh_patch_VBO(view_s, mesh_m->shading_mode, mesh_m->polygon_mode, 
					mesh_m->surf_surface_color, mesh_m->mesh_color_mode,
					mesh_m->num_of_color_loop, mesh_m->surf_grp_opacity,
					mesh_m->surf_surface_color_code,
					mesh_s->ngrp_surf_sf, &mesh_s->surf_stack_sf[ip_st],
					mesh_s->surf_item_sf, mesh_s->normal_surf_grp, mesh_s->norm_nod_surf_grp, 
					mesh_s->isurf_grp_far, mesh_s->ip_domain_far,
					i, mesh_s, mesh_m->always_draw_domains, mesh_VAO, kemo_shaders, mesh_buf);
		};
	};
	
	free(mesh_buf->v_buf);
	return;
}


void draw_transparent_mesh_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids *mesh_VAO, 
			struct kemoview_shaders *kemo_shaders, struct gl_strided_buffer *mesh_buf){
	int i, ip_st;
	
	if(mesh_m->domain_opacity < 1.0
				|| mesh_m->ele_grp_opacity < 1.0
				|| mesh_m->surf_grp_opacity < 1.0){
		sort_by_patch_distance_mesh(mesh_s, view_s);
	} else {
		copy_patch_distance_mesh(mesh_s);
	}
	
	set_buffer_address_4_patch(3*128, mesh_buf);
	alloc_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	
	if(mesh_m->draw_surface_solid != 0 && mesh_m->domain_opacity < 1.0){
		mesh_patch_VBO(view_s, mesh_m->shading_mode, mesh_m->polygon_mode, 
				mesh_m->domain_surface_color, mesh_m->mesh_color_mode,
				mesh_m->num_of_color_loop, mesh_m->domain_opacity,
				mesh_m->domain_surface_color_code, 
				mesh_s->num_pe_sf, mesh_s->isurf_stack_domain_sf, 
				mesh_s->isurf_domain_sf, mesh_s->normal_domain, mesh_s->norm_nod_domain,
				mesh_s->iele_domain_far, mesh_s->ip_domain_far,
				IZERO, mesh_s, mesh_m->draw_domains_solid, mesh_VAO, kemo_shaders, mesh_buf);
	};
	
	/* ! draw element group */
	
	if(mesh_m->ele_grp_opacity < 1.0){
		for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
			ip_st = i * mesh_s->num_pe_sf;
			
			if( mesh_m->draw_elegrp_solid[i] != 0){
			mesh_patch_VBO(view_s, mesh_m->shading_mode, mesh_m->polygon_mode, 
					mesh_m->ele_surface_color, mesh_m->mesh_color_mode,
					mesh_m->num_of_color_loop, mesh_m->ele_grp_opacity,
					mesh_m->ele_surface_color_code, 
					mesh_s->ngrp_ele_sf, &mesh_s->ele_stack_sf[ip_st],
					mesh_s->ele_item_sf, mesh_s->normal_ele_grp, mesh_s->norm_nod_ele_grp,
					mesh_s->iele_grp_far, mesh_s->ip_domain_far,
					i, mesh_s, mesh_m->always_draw_domains, mesh_VAO, kemo_shaders, mesh_buf);
			};
		};
	}
	
	/* ! draw surface group */
	if(mesh_m->surf_grp_opacity < 1.0){
		
		for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
			ip_st = i * mesh_s->num_pe_sf;
		
			if( mesh_m->draw_surfgrp_solid[i] != 0){
			mesh_patch_VBO(view_s, mesh_m->shading_mode, mesh_m->polygon_mode, 
					mesh_m->surf_surface_color, mesh_m->mesh_color_mode,
					mesh_m->num_of_color_loop, mesh_m->surf_grp_opacity,
					mesh_m->surf_surface_color_code,
					mesh_s->ngrp_surf_sf, &mesh_s->surf_stack_sf[ip_st],
					mesh_s->surf_item_sf, mesh_s->normal_surf_grp, mesh_s->norm_nod_surf_grp, 
					mesh_s->isurf_grp_far, mesh_s->ip_domain_far,
					i, mesh_s, mesh_m->always_draw_domains, mesh_VAO, kemo_shaders, mesh_buf);
			};
		};
	};
	
	free(mesh_buf->v_buf);
	return;
}

