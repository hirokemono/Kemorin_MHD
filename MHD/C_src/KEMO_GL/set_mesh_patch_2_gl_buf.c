/*
 *  set_mesh_patch_2_gl_buf.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include "set_mesh_patch_2_gl_buf.h"

const int ntri_ico = 20;

long num_icosahedron_patch(void){
    return ntri_ico;
}

static long count_each_grp_node_ico_to_buf(int iflag_domain, int *istack_grp){
    if(iflag_domain == 0) return 0;
    long num_patch = istack_grp[1] - istack_grp[0];
    return num_patch;
}

long add_mesh_node_ico_to_buf(long ist_patch, int *istack_grp, int num_pe_sf,
                              int *iflag_domain, long *istack_patch_pe){
	int ip;
    istack_patch_pe[0] = ist_patch;
	for(ip = 0; ip < num_pe_sf; ip++){
        istack_patch_pe[ip+1] = istack_patch_pe[ip]
            + count_each_grp_node_ico_to_buf(iflag_domain[ip], &istack_grp[ip]);
	};
	return istack_patch_pe[num_pe_sf];
}

long set_each_group_node_ico_to_buf(const long ist_tri, 
                                    long ist_grp, long ied_grp, int *item_grp,
                                    struct viewer_mesh *mesh_s, double node_diam,
                                    double f_color[4], 
                                    struct gl_strided_buffer *mesh_buf){
    long inod, inum;
    
    long inum_tri = ist_tri;
    for(inum = ist_grp; inum < ied_grp; inum++){
        inod = item_grp[inum]-1;
        inum_tri = set_icosahedron_strided_buffer(inum_tri, node_diam,
                                                  &mesh_s->xyzw_draw[4*inod  ],
                                                  f_color, mesh_buf);
    };
    return inum_tri;
}



const long count_each_grp_edge_to_buf(int iflag_domain, int nnod_4_edge, int *istack_grp){
    if(iflag_domain == 0) return 0;
    long num_edge = (nnod_4_edge-1)*(istack_grp[1] - istack_grp[0]);
    return num_edge;
}

long count_mesh_edge_buf(long ist_edge, int *iflag_domain, int *istack_grp,
                         struct viewer_mesh *mesh_s, long *istack_edge_pe){
	int ip;
	istack_edge_pe[0] = ist_edge;
	for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
        istack_edge_pe[ip+1] = istack_edge_pe[ip]
            + count_each_grp_edge_to_buf(iflag_domain[ip], mesh_s->nnod_4_edge, &istack_grp[ip]);
	};
	return istack_edge_pe[mesh_s->num_pe_sf];
}

long set_each_mesh_grid_to_buf(int ist, int ied, int *item_grp,
                               struct viewer_mesh *mesh_s,
                               double f_color[4], long ist_edge,
                               struct gl_strided_buffer *strided_buf){
    double xyzw_line[8], color_line[8];
	int i1, i2, k1, nd, inum;
    int icou, iedge;
    long inum_edge = ist_edge;

    for(icou = ist; icou < ied; icou++){
        iedge = abs( item_grp[icou] ) - 1;
        for(k1=0;k1<(mesh_s->nnod_4_edge-1);k1++){
            inum = k1 + mesh_s->nnod_4_edge * iedge;
            i1 = mesh_s->ie_edge_viewer[inum  ] - 1;
            i2 = mesh_s->ie_edge_viewer[inum+1] - 1;
            
            for(nd=0;nd<4;nd++) {
                xyzw_line[nd  ] = mesh_s->xyzw_draw[4*i1+nd];
                xyzw_line[4+nd] = mesh_s->xyzw_draw[4*i2+nd];
                color_line[nd  ] =  f_color[nd];
                color_line[4+nd] = f_color[nd];
            };
            inum_edge = set_line_strided_buffer(inum_edge,
                                                xyzw_line, color_line,
                                                strided_buf);
        };
    };
	return inum_edge;
}

static void add_each_mesh_tri_patch(int ie_local, int iele, int shading_mode, int polygon_mode,
                                    int nnod_4_sf, double *xyzw_draw, int *ie_sf_viewer,
                                    int *node_quad_2_linear_tri,
                                    double normal_ele[4], double normal_nod[12], double f_color[4],
                                    double xyzw_tri[12], double norm_tri[12],
                                    double color_tri[12]){
    int inod, inum, k1, nd;
    long k, kr;
    
	for (k=0; k<ITHREE; k++) {
		if (iele < 0) {kr = ITHREE - k - 1;}
		else {kr = k;};
		k1 = node_quad_2_linear_tri[3*ie_local+kr];
        inum = k1-1 + nnod_4_sf * (abs(iele)-1);
		inod = ie_sf_viewer[inum]-1;
		
		for(nd=0;nd<4;nd++){
            xyzw_tri[4*k+nd] =  xyzw_draw[4*inod+nd];
            color_tri[4*k+nd] = f_color[nd];
        };
		
		if (shading_mode == SMOOTH_SHADE) {
			for(nd = 0; nd < 4; nd++){norm_tri[4*k+nd] =  normal_nod[4*kr+nd];};
		} else {
			for (nd = 0; nd < 4; nd++){norm_tri[4*k+nd] = normal_ele[nd];};
		};
		
		if(polygon_mode == REVERSE_POLYGON){
			for(nd = 0; nd < 4; nd++){norm_tri[4*k+nd] = - norm_tri[4*k+nd];};
		};
	};
	return;
};


long set_mesh_patch_to_buffer(int shading_mode, int polygon_mode,
                              struct viewer_mesh *mesh_s, long ist_tri,
                              long ist_ele, long ied_ele, long *iele_patch,
                              struct gl_strided_buffer *mesh_buf){
    double xyzw_tri[12];
    double norm_tri[12], color_tri[12];
    int j, icolor;
    long inum, icou, jnum, item;
	
	long inum_tri = ist_tri;
	
    for (inum =ist_ele;inum<ied_ele; inum++) {
        icou = iele_patch[inum] / mesh_s->nsurf_each_tri;
        j =    iele_patch[inum] % mesh_s->nsurf_each_tri;
        
        item =  mesh_s->item_mesh_patch[icou];
        jnum = j + (icou) * mesh_s->nsurf_each_tri;
        icolor = mesh_s->igroup_mesh_patch[jnum];
        add_each_mesh_tri_patch(j, (int) item, shading_mode, polygon_mode,
                                mesh_s->nnod_4_surf, mesh_s->xyzw_draw,
                                mesh_s->ie_sf_viewer, mesh_s->node_quad_2_linear_tri,
                                &mesh_s->normal_mesh_patch[4*jnum],
                                &mesh_s->normal_nod_mesh_patch[12*jnum],
                                &mesh_s->mesh_color[4*icolor],
                                xyzw_tri, norm_tri, color_tri);
        inum_tri = set_patch_strided_buffer(inum_tri, xyzw_tri,
                                            norm_tri, color_tri,
                                            mesh_buf);
    };
	return inum_tri;
}

