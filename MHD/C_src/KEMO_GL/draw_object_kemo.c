
/* draw_object_kemo.c */

#include "draw_object_kemo.h"


void draw_objects_4_map(int shading_mode, struct psf_data *psf_s, 
						struct psf_menu_val *psf_m, struct view_element *view_s, 
						struct buffer_for_gl *gl_buf, int iflag_coast, int iflag_grid){ 
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
	
	glMatrixMode(GL_PROJECTION);
	glPushMatrix();
	glLoadIdentity();
	gluOrtho2D(-xwin, xwin, -ywin, ywin);
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	glLoadIdentity();
	
	
	if(psf_m->draw_psf_solid != 0){
		copy_patch_distance_psf(psf_s);
        draw_patches_4_map(shading_mode, psf_s, psf_m, gl_buf);
    }
	if( (psf_m->draw_psf_grid+psf_m->draw_psf_zero) != 0){
		draw_map_PSF_isoline(psf_s, psf_m, gl_buf);
	};

	if(iflag_coast != 0)   {draw_map_coast(gl_buf);}
	if(iflag_grid != 0){draw_flame_4_map(gl_buf);};
	
	glPopMatrix();
	glMatrixMode(GL_PROJECTION);
	glPopMatrix();
	glMatrixMode(GL_MODELVIEW);
	return;
}

void draw_solid_objects_4_psf(struct psf_data *psf_s, struct psf_menu_val *psf_m,
							  struct buffer_for_gl *gl_buf){

	if(psf_m->draw_psf_vect  != 0) draw_arrow_4_PSF(psf_s, psf_m);
	if( (psf_m->draw_psf_grid+psf_m->draw_psf_zero) != 0){
		draw_PSF_isoline(psf_s, psf_m, gl_buf);
	};
	return;
}

void draw_solid_patch_4_psf(int shading_mode, struct psf_data *psf_s,
							struct psf_menu_val *psf_m, struct buffer_for_gl *gl_buf){
	
	if(psf_m->draw_psf_solid != 0) {
		copy_patch_distance_psf(psf_s);
		
		if (psf_m->psf_patch_color == TEXTURED_SURFACE){
			draw_texure_4_PSF(shading_mode, psf_s, psf_m, gl_buf);
		} else {
			draw_patch_4_PSF(shading_mode, psf_s, psf_m, gl_buf);
		};
	};
	
	return;
}

void draw_transparent_objects_4_psf(int shading_mode, struct psf_data *psf_s, struct psf_menu_val *psf_m, 
									struct view_element *view_s, struct buffer_for_gl *gl_buf){
	
	
	if(psf_m->draw_psf_solid != 0) {
		sort_by_patch_distance_psf(psf_s, view_s);
		
		glEnable(GL_MULTISAMPLE);
		glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glDepthMask(GL_FALSE);
		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		
		
		if (psf_m->psf_patch_color == TEXTURED_SURFACE){
			draw_texure_4_PSF(shading_mode, psf_s, psf_m, gl_buf);
		} else {
			draw_patch_4_PSF(shading_mode, psf_s, psf_m, gl_buf);
		};
		
		glDisable(GL_BLEND);
		glDepthMask(GL_TRUE);
		glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glDisable(GL_MULTISAMPLE);
	};
	return;
}

void draw_nodes_4_domain(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
						 struct buffer_for_gl *gl_buf){
	int i, ip_st;
	
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
	return;
}


void draw_grids_4_domain(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
						 struct buffer_for_gl *gl_buf){
	int i, ip_st;
	
	for(i=0; i < mesh_s->num_pe_sf;i++){mesh_s->ip_domain_far[i] = i+1;};

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
	
	return;
}



void draw_patches_4_domain(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
						   struct buffer_for_gl *gl_buf){
	int i, ip_st;
	
	copy_patch_distance_mesh(mesh_s);
	
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
	
	if(mesh_m->draw_surface_solid != 0 && mesh_m->domain_opacity < 1.0){
		glEnable(GL_MULTISAMPLE);
		glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glDepthMask(GL_FALSE);
		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		/*glBlendFunc(GL_SRC_ALPHA, GL_ONE);*/
		
		draw_mesh_patch(mesh_m->shading_mode, mesh_m->polygon_mode, 
				mesh_m->domain_surface_color, mesh_m->mesh_color_mode,
				mesh_m->num_of_color_loop, mesh_m->domain_opacity,
				mesh_m->domain_surface_color_code, 
				mesh_s->num_pe_sf, mesh_s->isurf_stack_domain_sf, 
				mesh_s->isurf_domain_sf, mesh_s->normal_domain, mesh_s->norm_nod_domain,
				mesh_s->iele_domain_far, mesh_s->ip_domain_far, 
				IZERO, mesh_s, mesh_m->draw_domains_solid, gl_buf);
		
		glDisable(GL_BLEND);
		glDepthMask(GL_TRUE);
		glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glDisable(GL_MULTISAMPLE);
	};
	
	/* ! draw element group */
	
	if(mesh_m->ele_grp_opacity < 1.0){
		glEnable(GL_MULTISAMPLE);
		glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glDepthMask(GL_FALSE);
		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		/*glBlendFunc(GL_SRC_ALPHA, GL_ONE);*/
		
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
		
		glDisable(GL_BLEND);
		glDepthMask(GL_TRUE);
		glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glDisable(GL_MULTISAMPLE);
	}
	
	/* ! draw surface group */
	
	if(mesh_m->surf_grp_opacity < 1.0){
		glEnable(GL_MULTISAMPLE);
		glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glDepthMask(GL_FALSE);
		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		/*glBlendFunc(GL_SRC_ALPHA, GL_ONE);*/
		
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
		
		glDisable(GL_BLEND);
		glDepthMask(GL_TRUE);
		glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glDisable(GL_MULTISAMPLE);
	};
	
	return;
}

