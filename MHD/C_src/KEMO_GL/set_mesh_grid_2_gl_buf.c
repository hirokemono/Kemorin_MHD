
/* set_mesh_grid_2_gl_buf.c */

#include <pthread.h>

#include "set_mesh_grid_2_gl_buf.h"


static long set_each_mesh_grid_to_buf(int ist, int ied, int *item_grp,
                                      struct viewer_mesh *mesh_s,
                                      double f_color[4], long ist_edge,
                                      struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
    const float z_norm[4] = {0.0, 0.0, 1.0, 1.0};
	int i1, i2, k1, nd, inum;
    int icou, iedge;
    long inum_edge = ist_edge;

    for(icou = ist; icou < ied; icou++){
        iedge = abs( item_grp[icou] ) - 1;
        for(k1=0;k1<(mesh_s->nnod_4_edge-1);k1++){
            inum = k1 + mesh_s->nnod_4_edge * iedge;
            i1 = mesh_s->ie_edge_viewer[inum  ] - 1;
            i2 = mesh_s->ie_edge_viewer[inum+1] - 1;
            
            set_node_stride_buffer((ITWO*inum_edge), strided_buf, &point_buf);
            for(nd=0;nd<4;nd++) {
                strided_buf->v_buf[nd+point_buf.igl_xyzw] =  mesh_s->xyzw_draw[4*i1+nd];
                strided_buf->v_buf[nd+point_buf.igl_norm] =  z_norm[nd];
                strided_buf->v_buf[nd+point_buf.igl_color] = f_color[nd];
            };
            
            set_node_stride_buffer((ITWO*inum_edge+1), strided_buf, &point_buf);
            for(nd=0;nd<4;nd++) {
                strided_buf->v_buf[nd+point_buf.igl_xyzw] =  mesh_s->xyzw_draw[4*i2+nd];
                strided_buf->v_buf[nd+point_buf.igl_norm] =  z_norm[nd];
                strided_buf->v_buf[nd+point_buf.igl_color] = f_color[nd];
            };
            
            inum_edge = inum_edge + 1;
        };
    };
	return inum_edge;
}

typedef struct{
    int id;
    int nthread;
    
    struct gl_strided_buffer        *strided_buf;

    struct viewer_mesh *mesh_s;
    int ist_grp;
    int ied_grp;
    int  *item_grp;
    double *f_color;
    
    long istack_patch;
    long *num_patch;
} args_pthread_mesh_edge;

static void * set_each_mesh_grid_to_buf_1thread(void *arg){
    args_pthread_mesh_edge * p = (args_pthread_mesh_edge *) arg;
    int id =       p->id;
    int nthread = p->nthread;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    
    struct viewer_mesh *mesh_s = p->mesh_s;
    int ist_grp =  p->ist_grp;
    int ied_grp =  p->ied_grp;
    int *item_grp =     p->item_grp;;
    double *f_color =   p->f_color;

    long istack_patch = p->istack_patch;
    long *num_patch =  p->num_patch;
    
    int lo = (ied_grp - ist_grp) * id /     nthread;
    int hi = (ied_grp - ist_grp) * (id+1) / nthread;
    long ist_edge = (mesh_s->nnod_4_edge-1) * lo + istack_patch;
        
    num_patch[id] = set_each_mesh_grid_to_buf((lo+ist_grp), (hi+ist_grp), item_grp, mesh_s,
                                              f_color, ist_edge, strided_buf);
    return 0;
}

long set_each_mesh_grid_to_buf_pthread(long ist_patch, const int nthread,
                                       int ist_grp, int ied_grp, int *item_grp,
                                       struct viewer_mesh *mesh_s, double f_color[4],
                                       struct gl_strided_buffer *mesh_buf){
/* Allocate thread arguments. */
    args_pthread_mesh_edge *args
                = (args_pthread_mesh_edge *) malloc (nthread * sizeof(args_pthread_mesh_edge));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_mesh_edge.\n"); exit(1);}
/* Initialize thread handles and barrier. */
    pthread_t* thread_handles = malloc (nthread * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
        
    int ip;
    long *num_each = (long *) malloc (nthread * sizeof(long));
    for(ip=0;ip<nthread;ip++) {
        args[ip].id = ip;
        args[ip].nthread = nthread;

        args[ip].strided_buf = mesh_buf;
        
        args[ip].mesh_s =    mesh_s;
        args[ip].ist_grp =  ist_grp;
        args[ip].ied_grp =  ied_grp;
        args[ip].item_grp =  item_grp;
        args[ip].f_color =   f_color;

        args[ip].istack_patch = ist_patch;
        args[ip].num_patch = num_each;
            
        pthread_create(&thread_handles[ip], NULL, set_each_mesh_grid_to_buf_1thread, &args[ip]);
    }
    for(ip=0;ip<nthread;ip++){pthread_join(thread_handles[ip], NULL);}
    long num_patch = num_each[ip];
    free(num_each);
    free(thread_handles);
    free(args);
    return num_patch;
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
			
            if(nthread > 0){
                num_edge = set_each_mesh_grid_to_buf_pthread(istack_edge_pe[ip], nthread,
                                                             istack_grp[ip], istack_grp[ip+1], item_grp,
                                                             mesh_s, f_color, mesh_buf);
            }else{
                num_edge = set_each_mesh_grid_to_buf(istack_grp[ip], istack_grp[ip+1], item_grp,
                                                     mesh_s, f_color, istack_edge_pe[ip], mesh_buf);
            }
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

