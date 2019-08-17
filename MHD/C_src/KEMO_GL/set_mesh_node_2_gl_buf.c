
/* set_mesh_node_2_gl_buf.c */

#include "set_mesh_node_2_gl_buf.h"

const int count_mesh_node_ico_to_buf(int *istack_grp, struct viewer_mesh *mesh_s, int *iflag_domain){
	int ip;
	int num_patch = 0;
	for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
		if(iflag_domain[ip] != 0){
			num_patch = num_patch + 20 * (istack_grp[ip+1] - istack_grp[ip]);
		}
	};
	
	return num_patch;
}

const int set_mesh_node_ico_to_buf(int ist_tri, int num_grp, int igrp, int *istack_grp, int *item_grp,
			struct viewer_mesh *mesh_s, double node_diam,
			int node_color, int color_mode, int color_loop, GLfloat single_color[4], 
			int *iflag_domain, struct gl_strided_buffer *mesh_buf){
	double f_color[4];
	double xyz_patch[180], norm_patch[180];
	int inum_tri, num_ico;
	int i, nd, ip, inod, inum, ist, ied;
	
	set_node_color_mode_c(node_color, color_mode, color_loop, igrp, num_grp, single_color);
	
	inum_tri = ist_tri;
	for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
		if(iflag_domain[ip] != 0){
			ist = istack_grp[ip];
			ied = istack_grp[ip+1];
			set_patch_color_mode_c(node_color, color_mode, color_loop, (ip+1),
				mesh_s->num_pe_sf, igrp, num_grp, ONE, single_color, f_color);
			
			for(inum = ist; inum < ied; inum++){
				inod = item_grp[inum]-1;
				num_ico = set_icosahedron_patch(node_diam, &mesh_s->xx_draw[inod][0],
												 xyz_patch, norm_patch);
				for (i=0; i<num_ico; i++) {
					set_node_stride_VBO((inum_tri+i), mesh_buf);
					for(nd=0;nd<3;nd++){mesh_buf->x_draw[nd] =  xyz_patch[3*i+nd];};
					for(nd=0;nd<3;nd++){mesh_buf->n_draw[nd] = norm_patch[3*i+nd];};
					for(nd=0;nd<4;nd++){mesh_buf->c_draw[nd] = f_color[nd];};
				};
				
				inum_tri = inum_tri + num_ico;
			};
		}
	};
	
	return inum_tri;
}


int count_mesh_node_to_buf(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m){
	int i, ip_st;
	int num_patch = 0;
	
	num_patch = num_patch + count_mesh_node_ico_to_buf(mesh_s->nod_stack_domain_sf,
				mesh_s, mesh_m->draw_domains_nod);
	
	/* ! draw node group */
	
	for (i = 0; i < mesh_s->ngrp_nod_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		if( mesh_m->draw_nodgrp_nod[i] ){
			num_patch = num_patch + count_mesh_node_ico_to_buf(&mesh_s->nod_stack_sf[ip_st],
						mesh_s, mesh_m->always_draw_domains);
		};
	};
	
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		if( mesh_m->draw_elegrp_nod[i] ){
			num_patch = num_patch + count_mesh_node_ico_to_buf(&mesh_s->ele_nod_stack_sf[ip_st],
						mesh_s, mesh_m->always_draw_domains);
		};
	};
	
	/* ! draw surface group */
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_surfgrp_nod[i] ){
			num_patch = num_patch + count_mesh_node_ico_to_buf(&mesh_s->surf_nod_stack_sf[ip_st],
						mesh_s, mesh_m->always_draw_domains);
		};
	};
	return num_patch;
}


int set_mesh_node_to_buf(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct gl_strided_buffer *mesh_buf){
	int i, ip_st;
	int ist_tri = 0;
	
	ist_tri = set_mesh_node_ico_to_buf(ist_tri, mesh_s->num_pe_sf, IZERO, mesh_s->nod_stack_domain_sf,
				mesh_s->nod_item_domain_sf, mesh_s,
				mesh_m->node_diam, mesh_m->domain_node_color,
				mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
				mesh_m->domain_node_color_code, mesh_m->draw_domains_nod, 
				mesh_buf);
	
	/* ! draw node group */
	
	for (i = 0; i < mesh_s->ngrp_nod_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		if( mesh_m->draw_nodgrp_nod[i] ){
			ist_tri = set_mesh_node_ico_to_buf(ist_tri, mesh_s->ngrp_nod_sf, i, &mesh_s->nod_stack_sf[ip_st],
						mesh_s->nod_item_sf, mesh_s,
						mesh_m->node_diam, mesh_m->node_node_color,
						mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
						mesh_m->node_node_color_code, mesh_m->always_draw_domains, 
						mesh_buf);
	
		};
	};
	
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		if( mesh_m->draw_elegrp_nod[i] ){
			ist_tri = set_mesh_node_ico_to_buf(ist_tri, mesh_s->ngrp_ele_sf, i, &mesh_s->ele_nod_stack_sf[ip_st],
						mesh_s->ele_nod_item_sf, mesh_s,
						mesh_m->node_diam, mesh_m->ele_node_color,
						mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
						mesh_m->ele_node_color_code, mesh_m->always_draw_domains, 
						mesh_buf);
	
		};
	};
	
	/* ! draw surface group */
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		if( mesh_m->draw_surfgrp_nod[i] ){
			ist_tri = set_mesh_node_ico_to_buf(ist_tri, mesh_s->ngrp_surf_sf, i, &mesh_s->surf_nod_stack_sf[ip_st],
						mesh_s->surf_nod_item_sf, mesh_s,
						mesh_m->node_diam, mesh_m->surf_node_color,
						mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
						mesh_m->surf_node_color_code, mesh_m->always_draw_domains, 
						mesh_buf);
	
		};
	};
	return ist_tri;
}
