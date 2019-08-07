
/* draw_object_kemo.c */

#include "draw_object_kemo.h"


int draw_objects_4_map(struct psf_data **psf_s, struct mesh_menu_val *mesh_m,
                       struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                       struct view_element *view_s, struct buffer_for_gl *gl_buf){
    int i;
    int iflag_map = 0;
	GLdouble xwin, ywin;
	    
	/* set shading mode */
	glShadeModel(GL_SMOOTH);
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	
	if(view_s->ny_window > view_s->nx_window) {
		xwin = 2.05;
		ywin = 2.05 * (GLdouble)view_s->ny_window / (GLdouble)view_s->nx_window;
	} else{
		xwin = 1.7 * (GLdouble)view_s->nx_window / (GLdouble)view_s->ny_window;
		ywin = 1.7;
	}
	
	orthogonalGL(-xwin, xwin, -ywin, ywin, -1.0, 1.0);
	set_view_by_identity();
    draw_patches_4_map(mesh_m->shading_mode, IZERO, psf_a->istack_solid_psf_patch,
                       psf_s, psf_a, gl_buf);

    for(i=0; i<psf_a->nmax_loaded; i++){
        iflag_map = iflag_map + psf_a->iflag_loaded[i];
        if(psf_a->iflag_loaded[i] != 0){
            if( (psf_m[i]->draw_psf_grid+psf_m[i]->draw_psf_zero) != 0){
                draw_map_PSF_isoline(psf_s[i], psf_m[i], gl_buf, view_s->iflag_retina,
                                     view_s->iflag_write_ps);
            };
        };
    };
	

	if(mesh_m->iflag_draw_coast != 0)   {draw_map_coast(gl_buf);}
	if(mesh_m->iflag_draw_sph_grid != 0){draw_flame_4_map(gl_buf, view_s->iflag_write_ps);};
	
	load_projection_matrix(view_s);
	modify_view_by_struct(view_s);
	return iflag_map;
}

void draw_nodes_4_domain(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
						 struct buffer_for_gl *gl_buf){
	int i, ip_st;
	
	glShadeModel(GL_SMOOTH);
	glPolygonMode(GL_FRONT, GL_FILL);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glDisable(GL_CULL_FACE);
	
	draw_node_by_ico(mesh_s->num_pe_sf, IZERO, mesh_s->nod_stack_domain_sf,
			mesh_s->nod_item_domain_sf, mesh_s,
			mesh_m->node_diam, mesh_m->domain_node_color,
			mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
			mesh_m->domain_node_color_code, mesh_m->draw_domains_nod, gl_buf);
	
	/* ! draw node group */
	
	for (i = 0; i < mesh_s->ngrp_nod_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_nodgrp_nod[i] ){
	
			draw_node_by_ico(mesh_s->ngrp_nod_sf, i, &mesh_s->nod_stack_sf[ip_st],
					mesh_s->nod_item_sf, mesh_s,
					mesh_m->node_diam, mesh_m->node_node_color,
					mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
					mesh_m->node_node_color_code, mesh_m->always_draw_domains, gl_buf);
	
		};
	};
	
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_elegrp_nod[i] ){
			draw_node_by_ico(mesh_s->ngrp_ele_sf, i, &mesh_s->ele_nod_stack_sf[ip_st],
					mesh_s->ele_nod_item_sf, mesh_s,
					mesh_m->node_diam, mesh_m->ele_node_color,
					mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
					mesh_m->ele_node_color_code, mesh_m->always_draw_domains, gl_buf);
	
		};
	};
	
	/* ! draw surface group */
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_surfgrp_nod[i] ){
			draw_node_by_ico(mesh_s->ngrp_surf_sf, i, &mesh_s->surf_nod_stack_sf[ip_st],
					mesh_s->surf_nod_item_sf, mesh_s,
					mesh_m->node_diam, mesh_m->surf_node_color,
					mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
					mesh_m->surf_node_color_code, mesh_m->always_draw_domains, gl_buf);
	
		};
	};
	glEnable(GL_CULL_FACE);
	return;
}


void draw_grids_4_domain(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
						 struct buffer_for_gl *gl_buf){
	int i, ip_st;
	
	for(i=0; i < mesh_s->num_pe_sf;i++){mesh_s->ip_domain_far[i] = i+1;};
	
	glDisable(GL_CULL_FACE);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	
	if(mesh_m->draw_surface_grid != 0){
		draw_mesh_grid(mesh_m->domain_grid_color, mesh_m->mesh_color_mode,
					   mesh_m->num_of_color_loop, mesh_m->domain_grid_color_code,
					   mesh_s->num_pe_sf, mesh_s->edge_stack_domain_sf, 
					   mesh_s->edge_item_domain_sf, IZERO, mesh_m->draw_domains_grid,
					   mesh_s, gl_buf);
	};
	
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_elegrp_grid[i] ){
			draw_mesh_grid(mesh_m->ele_grid_color, mesh_m->mesh_color_mode, 
						   mesh_m->num_of_color_loop, mesh_m->ele_grid_color_code,
						   mesh_s->ngrp_ele_sf, &mesh_s->ele_edge_stack_sf[ip_st],
						   mesh_s->ele_edge_item_sf, i, mesh_m->always_draw_domains,
						   mesh_s, gl_buf);
		};
	};
	
	/* ! draw surface group */
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_surfgrp_grid[i] ){
			draw_mesh_grid(mesh_m->surf_grid_color, mesh_m->mesh_color_mode,
						   mesh_m->num_of_color_loop, mesh_m->surf_grid_color_code,
						   mesh_s->ngrp_surf_sf, &mesh_s->surf_edge_stack_sf[ip_st], 
						   mesh_s->surf_edge_item_sf, i, mesh_m->always_draw_domains, 
						   mesh_s, gl_buf);
		};
	};
	glEnable(GL_CULL_FACE);
	
	return;
}



void draw_patches_4_domain(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
						   struct buffer_for_gl *gl_buf){
	int i, ip_st;
	
	copy_patch_distance_mesh(mesh_s);
	
	glShadeModel(GL_SMOOTH);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	
	glEnable( GL_CULL_FACE );
	if (mesh_m->polygon_mode == NORMAL_POLYGON) { 
		glPolygonMode(GL_FRONT, GL_FILL);
		glCullFace(GL_BACK);
		}
	else if(mesh_m->polygon_mode == REVERSE_POLYGON) { 
		glPolygonMode(GL_BACK, GL_FILL);
		glCullFace(GL_FRONT);
	};
	
	if(mesh_m->draw_surface_solid != 0 && mesh_m->domain_opacity >= 1.0){
		draw_mesh_patch(mesh_m->shading_mode, mesh_m->polygon_mode, 
				mesh_m->domain_surface_color, mesh_m->mesh_color_mode,
				mesh_m->num_of_color_loop, mesh_m->domain_opacity,
				mesh_m->domain_surface_color_code, 
				mesh_s->num_pe_sf, mesh_s->isurf_stack_domain_sf, 
				mesh_s->isurf_domain_sf, mesh_s->normal_domain, mesh_s->norm_nod_domain,
				mesh_s->iele_domain_far, mesh_s->ip_domain_far,
				IZERO, mesh_s, mesh_m->draw_domains_solid, gl_buf);
	};
	
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_elegrp_solid[i] != 0 && mesh_m->ele_grp_opacity >= 1.0){
			draw_mesh_patch(mesh_m->shading_mode, mesh_m->polygon_mode, 
					mesh_m->ele_surface_color, mesh_m->mesh_color_mode,
					mesh_m->num_of_color_loop, mesh_m->ele_grp_opacity,
					mesh_m->ele_surface_color_code, 
					mesh_s->ngrp_ele_sf, &mesh_s->ele_stack_sf[ip_st],
					mesh_s->ele_item_sf, mesh_s->normal_ele_grp, mesh_s->norm_nod_ele_grp,
					mesh_s->iele_grp_far, mesh_s->ip_domain_far,
					i, mesh_s, mesh_m->always_draw_domains, gl_buf);
		};
	};
	
	/* ! draw surface group */
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_surfgrp_solid[i] != 0 && mesh_m->surf_grp_opacity >= 1.0){
			draw_mesh_patch(mesh_m->shading_mode, mesh_m->polygon_mode, 
					mesh_m->surf_surface_color, mesh_m->mesh_color_mode,
					mesh_m->num_of_color_loop, mesh_m->surf_grp_opacity,
					mesh_m->surf_surface_color_code,
					mesh_s->ngrp_surf_sf, &mesh_s->surf_stack_sf[ip_st],
					mesh_s->surf_item_sf, mesh_s->normal_surf_grp, mesh_s->norm_nod_surf_grp, 
					mesh_s->isurf_grp_far, mesh_s->ip_domain_far,
					i, mesh_s, mesh_m->always_draw_domains, gl_buf);
		};
	};
	
	return;
}


void draw_transparent_4_domain(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct buffer_for_gl *gl_buf){
	int i, ip_st;
	
	if(mesh_m->domain_opacity < 1.0
				|| mesh_m->ele_grp_opacity < 1.0
				|| mesh_m->surf_grp_opacity < 1.0){
		sort_by_patch_distance_mesh(mesh_s, view_s);
	} else {
		copy_patch_distance_mesh(mesh_s);
	}
	
	glShadeModel(GL_SMOOTH);
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	
	glEnable( GL_CULL_FACE );
	if (mesh_m->polygon_mode == NORMAL_POLYGON) { 
		glPolygonMode(GL_FRONT, GL_FILL);
		glCullFace(GL_BACK);
		}
	else if(mesh_m->polygon_mode == REVERSE_POLYGON) { 
		glPolygonMode(GL_BACK, GL_FILL);
		glCullFace(GL_FRONT);
	};
	
	glEnable(GL_MULTISAMPLE);
	glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glDepthMask(GL_FALSE);
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	/*glBlendFunc(GL_SRC_ALPHA, GL_ONE);*/
	
	
	if(mesh_m->draw_surface_solid != 0 && mesh_m->domain_opacity < 1.0){
		draw_mesh_patch(mesh_m->shading_mode, mesh_m->polygon_mode, 
				mesh_m->domain_surface_color, mesh_m->mesh_color_mode,
				mesh_m->num_of_color_loop, mesh_m->domain_opacity,
				mesh_m->domain_surface_color_code, 
				mesh_s->num_pe_sf, mesh_s->isurf_stack_domain_sf, 
				mesh_s->isurf_domain_sf, mesh_s->normal_domain, mesh_s->norm_nod_domain,
				mesh_s->iele_domain_far, mesh_s->ip_domain_far, 
				IZERO, mesh_s, mesh_m->draw_domains_solid, gl_buf);
	};
	
	/* ! draw element group */
	
	if(mesh_m->ele_grp_opacity < 1.0){
		for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
			ip_st = i * mesh_s->num_pe_sf;
			
			if( mesh_m->draw_elegrp_solid[i] != 0){
				draw_mesh_patch(mesh_m->shading_mode, mesh_m->polygon_mode, 
						mesh_m->ele_surface_color, mesh_m->mesh_color_mode,
						mesh_m->num_of_color_loop, mesh_m->ele_grp_opacity,
						mesh_m->ele_surface_color_code, 
						mesh_s->ngrp_ele_sf, &mesh_s->ele_stack_sf[ip_st],
						mesh_s->ele_item_sf, mesh_s->normal_ele_grp, mesh_s->norm_nod_ele_grp,
						mesh_s->iele_grp_far, mesh_s->ip_domain_far,
						i, mesh_s, mesh_m->always_draw_domains, gl_buf);
			};
		};
	}
	
	/* ! draw surface group */
	if(mesh_m->surf_grp_opacity < 1.0){
		
		for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
			ip_st = i * mesh_s->num_pe_sf;
		
			if( mesh_m->draw_surfgrp_solid[i] != 0){
				draw_mesh_patch(mesh_m->shading_mode, mesh_m->polygon_mode, 
					mesh_m->surf_surface_color, mesh_m->mesh_color_mode,
					mesh_m->num_of_color_loop, mesh_m->surf_grp_opacity,
					mesh_m->surf_surface_color_code,
					mesh_s->ngrp_surf_sf, &mesh_s->surf_stack_sf[ip_st],
					mesh_s->surf_item_sf, mesh_s->normal_surf_grp, mesh_s->norm_nod_surf_grp, 
					mesh_s->isurf_grp_far, mesh_s->ip_domain_far,
					i, mesh_s, mesh_m->always_draw_domains, gl_buf);
			};
		};
	};
	glDisable(GL_BLEND);
	glDepthMask(GL_TRUE);
	glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glDisable(GL_MULTISAMPLE);
	
	return;
}

