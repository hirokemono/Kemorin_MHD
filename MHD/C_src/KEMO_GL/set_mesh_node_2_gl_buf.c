
/* set_mesh_node_2_gl_buf.c */

#include "set_mesh_node_2_gl_buf.h"

const long count_each_grp_node_ico_to_buf(int iflag_domain, int *istack_grp){
    if(iflag_domain == 0) return 0;
    long num_patch = 20 * (istack_grp[1] - istack_grp[0]);
    return num_patch;
}

const long add_mesh_node_ico_to_buf(long ist_patch, int *istack_grp, int num_pe_sf,
                                    int *iflag_domain, long *istack_patch_pe){
	int ip;
    istack_patch_pe[0] = ist_patch;
	for(ip = 0; ip < num_pe_sf; ip++){
        istack_patch_pe[ip+1] = istack_patch_pe[ip]
            + count_each_grp_node_ico_to_buf(iflag_domain[ip], &istack_grp[ip]);
	};
	return istack_patch_pe[num_pe_sf];
}

static long set_each_group_node_ico_to_buf(const long ist_tri, 
                                           long ist_grp, long ied_grp, int *item_grp,
                                           struct viewer_mesh *mesh_s, double node_diam,
                                           double f_color[4], 
                                           struct gl_strided_buffer *mesh_buf,
                                           struct gl_local_buffer_address *point_buf){
    double xyzw_patch[240], norm_patch[240];
    int inod;
    long inum, icou, nd;
    long inum_tri, num_ico;
    
    inum_tri = ist_tri;
    for(inum = ist_grp; inum < ied_grp; inum++){
        inod = item_grp[inum]-1;
        num_ico = set_icosahedron_patch(node_diam, &mesh_s->xyzw_draw[4*inod  ],
                                        xyzw_patch, norm_patch);

        for (icou=0; icou<num_ico; icou++) {
            set_node_stride_buffer((inum_tri+icou), mesh_buf, point_buf);
            for(nd=0;nd<4;nd++){
                mesh_buf->v_buf[nd+point_buf->igl_xyzw] = xyzw_patch[4*icou+nd];
                mesh_buf->v_buf[nd+point_buf->igl_norm] = norm_patch[4*icou+nd];
                mesh_buf->v_buf[nd+point_buf->igl_color] = f_color[nd];
            };
        };
        inum_tri = inum_tri + num_ico;
    };
    return inum_tri;
}

static long set_mesh_node_ico_to_buf(long *istack_patch_pe,
                                     int num_grp, int igrp,
                                     int *istack_grp, int *item_grp,
                                     struct viewer_mesh *mesh_s, double node_diam,
                                     int node_color, int color_mode,
                                     int color_loop, float single_color[4],
                                     int *iflag_domain,
                                     struct gl_strided_buffer *mesh_buf,
                                     struct gl_local_buffer_address *point_buf){
	double f_color[4];
    int ip;
    long inum_tri;
	
	set_node_color_mode_c(node_color, color_mode, color_loop,
                          igrp, num_grp, single_color);
	
	inum_tri = istack_patch_pe[0];
    for(ip = 0; ip<mesh_s->num_pe_sf; ip++){
		if(iflag_domain[ip] != 0){
			set_patch_color_mode_c(node_color, color_mode, color_loop,
                                   ip, mesh_s->num_pe_sf,
                                   igrp, num_grp, ONE,
                                   single_color, f_color);
			
            inum_tri = set_each_group_node_ico_to_buf(inum_tri,
                                                      istack_grp[ip], istack_grp[ip+1], item_grp,
                                                      mesh_s, node_diam, f_color, mesh_buf, point_buf);
		}
	};
	
	return inum_tri;
}


long count_mesh_node_to_buf(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
                            long *istack_node_domain_patch,  long *istack_node_nod_grp_patch,
                            long *istack_node_ele_grp_patch, long *istack_node_surf_grp_patch){
	int i, ip_st;
    long num_patch = 0;
    num_patch = add_mesh_node_ico_to_buf(num_patch, mesh_s->nod_stack_domain_sf,
                                                            mesh_s->num_pe_sf,
                                                            mesh_m->draw_domains_nod,
                                                            &istack_node_domain_patch[0]);
	
	/* ! draw node group */
	
    istack_node_nod_grp_patch[0] = num_patch;
	for(i = 0; i < mesh_s->ngrp_nod_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		if(mesh_m->draw_nodgrp_nod[i]){
			num_patch = add_mesh_node_ico_to_buf(num_patch, &mesh_s->nod_stack_sf[ip_st],
                                                 mesh_s->num_pe_sf,
                                                 mesh_m->always_draw_domains,
                                                 &istack_node_nod_grp_patch[ip_st]);
		};
	};

	/* ! draw element group */
    istack_node_ele_grp_patch[0] = num_patch;
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		if( mesh_m->draw_elegrp_nod[i] ){
			num_patch = add_mesh_node_ico_to_buf(num_patch, &mesh_s->ele_nod_stack_sf[ip_st],
                                                 mesh_s->num_pe_sf,
                                                 mesh_m->always_draw_domains,
                                                 &istack_node_ele_grp_patch[ip_st]);
		};
	};

	/* ! draw surface group */
    istack_node_surf_grp_patch[0] = num_patch;
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_surfgrp_nod[i] ){
			num_patch = add_mesh_node_ico_to_buf(num_patch, &mesh_s->surf_nod_stack_sf[ip_st],
                                                 mesh_s->num_pe_sf,
                                                 mesh_m->always_draw_domains,
                                                 &istack_node_surf_grp_patch[ip_st]);
		};
	};
	return num_patch;
}


long set_mesh_node_to_buf(long *istack_node_domain_patch,  long *istack_node_nod_grp_patch,
                          long *istack_node_ele_grp_patch, long *istack_node_surf_grp_patch,
                          struct viewer_mesh *mesh_s,
                          struct mesh_menu_val *mesh_m,
                          struct gl_strided_buffer *mesh_buf,
                          struct gl_local_buffer_address *point_buf){
	int i, ip_st;
	long num_tri = 0;
	
    num_tri = set_mesh_node_ico_to_buf(istack_node_domain_patch,
                                       mesh_s->num_pe_sf, IZERO,
                                       mesh_s->nod_stack_domain_sf,
                                       mesh_s->nod_item_domain_sf, mesh_s,
                                       mesh_m->node_diam, mesh_m->domain_node_color,
                                       mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
                                       mesh_m->domain_node_color_code, mesh_m->draw_domains_nod,
                                       mesh_buf, point_buf);
	
	/* ! draw node group */
	
	for (i = 0; i < mesh_s->ngrp_nod_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		if( mesh_m->draw_nodgrp_nod[i] ){
            num_tri = set_mesh_node_ico_to_buf(&istack_node_nod_grp_patch[ip_st],
                                               mesh_s->ngrp_nod_sf, i, &mesh_s->nod_stack_sf[ip_st],
                                               mesh_s->nod_item_sf, mesh_s,
                                               mesh_m->node_diam, mesh_m->node_node_color,
                                               mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
                                               mesh_m->node_node_color_code, mesh_m->always_draw_domains,
                                               mesh_buf, point_buf);
	
		};
	};
	
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		if( mesh_m->draw_elegrp_nod[i] ){
            num_tri = set_mesh_node_ico_to_buf(&istack_node_ele_grp_patch[ip_st],
                                               mesh_s->ngrp_ele_sf, i,
                                               &mesh_s->ele_nod_stack_sf[ip_st],
                                               mesh_s->ele_nod_item_sf, mesh_s,
                                               mesh_m->node_diam, mesh_m->ele_node_color,
                                               mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
                                               mesh_m->ele_node_color_code,
                                               mesh_m->always_draw_domains,
                                               mesh_buf, point_buf);
	
		};
	};
	
	/* ! draw surface group */
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		if( mesh_m->draw_surfgrp_nod[i] ){
            num_tri = set_mesh_node_ico_to_buf(&istack_node_surf_grp_patch[ip_st],
                                               mesh_s->ngrp_surf_sf, i,
                                               &mesh_s->surf_nod_stack_sf[ip_st],
                                               mesh_s->surf_nod_item_sf, mesh_s,
                                               mesh_m->node_diam, mesh_m->surf_node_color,
                                               mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
                                               mesh_m->surf_node_color_code,
                                               mesh_m->always_draw_domains,
                                               mesh_buf, point_buf);
	
		};
	};
	return num_tri;
}
