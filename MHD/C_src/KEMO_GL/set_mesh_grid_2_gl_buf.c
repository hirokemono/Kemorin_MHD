
/* set_mesh_grid_2_gl_buf.c */

#include "set_mesh_grid_2_gl_buf.h"

static int set_each_mesh_grid(int iedge, struct viewer_mesh *mesh_s, double f_color[4],
						  int inum_buf, struct gl_strided_buffer *strided_buf){
	int i1, i2, k1, nd;
	
	for(k1=0;k1<(mesh_s->nnod_4_edge-1);k1++){
		i1 = mesh_s->ie_edge_viewer[iedge][k1  ]-1;
        set_node_stride_buffer((ITWO*inum_buf), strided_buf);
		for(nd=0;nd<3;nd++) {strided_buf->x_draw[nd] =  mesh_s->xx_draw[i1][nd];};
		for(nd=0;nd<3;nd++) {strided_buf->n_draw[nd] =  0.0;};
		for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = f_color[nd];};
		
		i2 = mesh_s->ie_edge_viewer[iedge][k1+1]-1;
        set_node_stride_buffer((ITWO*inum_buf+1), strided_buf);
		for(nd=0;nd<3;nd++) {strided_buf->x_draw[nd] =  mesh_s->xx_draw[i2][nd];};
		for(nd=0;nd<3;nd++) {strided_buf->n_draw[nd] =  0.0;};
		for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = f_color[nd];};
		
		inum_buf = inum_buf + 1;
	};
	
	return inum_buf;
}

int count_mesh_edge_buf(int *iflag_domain, int *istack_grp, struct viewer_mesh *mesh_s){
	int ip;
	int num_edge = 0;
	for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
		if(iflag_domain[ip] != 0){
			num_edge = num_edge + (mesh_s->nnod_4_edge-1)*(istack_grp[ip+1] -istack_grp[ip]);
		};
	};
	return num_edge;
}

int add_mesh_edge_to_buf(int ist_edge, int line_color, int color_mode, int color_loop, float single_color[4],
                         int num_grp, int *istack_grp, int *item_grp, int igrp, int *iflag_domain,
                         struct viewer_mesh *mesh_s, struct gl_strided_buffer *mesh_buf){
	int ip, inum, iedge, ist, ied;
	int inum_buf;
	double f_color[4];
	
	inum_buf = ist_edge;
	for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
		if(iflag_domain[ip] != 0){
			set_grid_color_mode_c(line_color, color_mode, color_loop, ip, mesh_s->num_pe_sf, 
								  igrp, num_grp, single_color, f_color);
			
			ist = istack_grp[ip];
			ied = istack_grp[ip+1];
			for(inum = ist; inum < ied; inum++){
				iedge = abs( item_grp[inum] ) - 1;
				inum_buf = set_each_mesh_grid(iedge, mesh_s, f_color, inum_buf, mesh_buf);
			};
		};
	};
	
	return inum_buf;
}


int count_mesh_grid_to_buf(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m){
	int i, ip_st;
	int num_edge = 0;
	if(mesh_m->draw_surface_grid != 0){
		num_edge = count_mesh_edge_buf(mesh_m->draw_domains_grid, 
					mesh_s->edge_stack_domain_sf, mesh_s);
	};
	
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_elegrp_grid[i] ){
			num_edge = num_edge + count_mesh_edge_buf(mesh_m->always_draw_domains, 
						&mesh_s->ele_edge_stack_sf[ip_st], mesh_s);
		};
	};
	
	/* ! draw surface group */
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_surfgrp_grid[i] ){
			num_edge = num_edge + count_mesh_edge_buf(mesh_m->always_draw_domains, 
						&mesh_s->surf_edge_stack_sf[ip_st], mesh_s);
		};
	};
	return num_edge;
}

int set_mesh_grid_to_buf(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct gl_strided_buffer *mesh_buf){
	int i, ip_st;
	int ist_edge = 0;
	if(mesh_m->draw_surface_grid != 0){
		ist_edge = add_mesh_edge_to_buf(ist_edge, mesh_m->domain_grid_color, mesh_m->mesh_color_mode,
					   mesh_m->num_of_color_loop, mesh_m->domain_grid_color_code,
					   mesh_s->num_pe_sf, mesh_s->edge_stack_domain_sf, 
					   mesh_s->edge_item_domain_sf, IZERO, mesh_m->draw_domains_grid,
					   mesh_s, mesh_buf);
	};
	
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_elegrp_grid[i] ){
			ist_edge = add_mesh_edge_to_buf(ist_edge, mesh_m->ele_grid_color, mesh_m->mesh_color_mode, 
						   mesh_m->num_of_color_loop, mesh_m->ele_grid_color_code,
						   mesh_s->ngrp_ele_sf, &mesh_s->ele_edge_stack_sf[ip_st],
						   mesh_s->ele_edge_item_sf, i, mesh_m->always_draw_domains,
						   mesh_s, mesh_buf);
		};
	};
	
	/* ! draw surface group */
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_surfgrp_grid[i] ){
			ist_edge = add_mesh_edge_to_buf(ist_edge, mesh_m->surf_grid_color, mesh_m->mesh_color_mode,
						   mesh_m->num_of_color_loop, mesh_m->surf_grid_color_code,
						   mesh_s->ngrp_surf_sf, &mesh_s->surf_edge_stack_sf[ip_st], 
						   mesh_s->surf_edge_item_sf, i, mesh_m->always_draw_domains, 
						   mesh_s, mesh_buf);
		};
	};
	return ist_edge;
}

