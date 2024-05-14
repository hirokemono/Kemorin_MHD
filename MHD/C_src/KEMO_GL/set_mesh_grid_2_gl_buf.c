
/* set_mesh_grid_2_gl_buf.c */

#include "set_mesh_grid_2_gl_buf.h"


long add_mesh_edge_to_buf(int nthread, long *istack_edge_pe,
                          int line_color, int color_mode,
                          int color_loop, float single_color[4],
                          int num_grp, int *istack_grp, int *item_grp, int igrp,
                          int *iflag_domain, struct viewer_mesh *mesh_s,
                          struct gl_strided_buffer *mesh_buf){
	int ip;
	double f_color[4];
	
	long num_edge = istack_edge_pe[0];
	for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
		if(iflag_domain[ip] != 0){
			set_grid_color_mode_c(line_color, color_mode, color_loop, ip, mesh_s->num_pe_sf,
								  igrp, num_grp, single_color, f_color);
			
            num_edge = sel_each_mesh_grid_to_buf_pthread(istack_edge_pe[ip], nthread,
                                                         istack_grp[ip], istack_grp[ip+1], item_grp,
                                                         mesh_s, f_color, mesh_buf);
		};
	};
	
	return num_edge;
}


long count_mesh_grid_to_buf(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
                            long *istack_edge_domain_edge,
                            long *istack_ele_grp_edge,
                            long *istack_surf_grp_edge){
	int i, ip_st;
	long num_edge = 0;
	if(mesh_m->draw_surface_grid != 0){
		num_edge = count_mesh_edge_buf(0, mesh_m->draw_domains_grid,
                                       mesh_s->edge_stack_domain_sf, mesh_s,
                                       istack_edge_domain_edge);
	};
	
	/* ! draw element group */
	
    istack_ele_grp_edge[0] = num_edge;
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_elegrp_grid[i] ){
			num_edge = count_mesh_edge_buf(num_edge, mesh_m->always_draw_domains,
                                           &mesh_s->ele_edge_stack_sf[ip_st], mesh_s,
                                           &istack_ele_grp_edge[ip_st]);
		};
	};
	
	/* ! draw surface group */
	
    istack_surf_grp_edge[0] = num_edge;
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_surfgrp_grid[i] ){
			num_edge = count_mesh_edge_buf(num_edge, mesh_m->always_draw_domains,
                                           &mesh_s->surf_edge_stack_sf[ip_st], mesh_s,
                                           &istack_surf_grp_edge[ip_st]);
		};
	};
	return num_edge;
}

long set_mesh_grid_to_buf(int nthread, long *istack_edge_domain_edge,
                          long *istack_ele_grp_edge, long *istack_surf_grp_edge,
                          struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
                          struct gl_strided_buffer *mesh_buf){
	int i, ip_st;
	long num_edge = 0;
	if(mesh_m->draw_surface_grid != 0){
        num_edge = add_mesh_edge_to_buf(nthread, istack_edge_domain_edge,
                                        mesh_m->domain_grid_color, mesh_m->mesh_color_mode,
                                        mesh_m->num_of_color_loop, mesh_m->domain_grid_color_code,
                                        mesh_s->num_pe_sf, mesh_s->edge_stack_domain_sf,
                                        mesh_s->edge_item_domain_sf, IZERO, mesh_m->draw_domains_grid,
                                        mesh_s, mesh_buf);
	};
	
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_elegrp_grid[i] ){
            num_edge = add_mesh_edge_to_buf(nthread, &istack_ele_grp_edge[ip_st],
                                            mesh_m->ele_grid_color, mesh_m->mesh_color_mode,
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
            num_edge = add_mesh_edge_to_buf(nthread, &istack_surf_grp_edge[ip_st],
                                            mesh_m->surf_grid_color, mesh_m->mesh_color_mode,
                                            mesh_m->num_of_color_loop, mesh_m->surf_grid_color_code,
                                            mesh_s->ngrp_surf_sf, &mesh_s->surf_edge_stack_sf[ip_st],
                                            mesh_s->surf_edge_item_sf, i, mesh_m->always_draw_domains,
                                            mesh_s, mesh_buf);
		};
	};
	return num_edge;
}

