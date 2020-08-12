/*
 *  t_surf_edge_hash_c.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/11/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#ifndef T_SURF_EDGE_HASH_C__
#define T_SURF_EDGE_HASH_C__

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

struct sum_hash_tbl_c{
	int node_on_edge_tri[3][2];
	
	long ntot_id;
	long iend_hash;
	long *istack_hash;
	
	long ntot_list;
	long **id_hash;
	long *iflag_hash;
};

/*  prototypes */

struct sum_hash_tbl_c * init_sum_hash(const long numnod, const long nele, 
									  const int num_4_ele, const int nnod_4_edge);
void dealloc_sum_hash(struct sum_hash_tbl_c *h_tbl);

void clear_sum_hash(struct sum_hash_tbl_c *h_tbl);
void set_edge_hash_4_triangle(const int nedge_4_surf, const long numsurf, 
							  const int nnod_4_surf, long **ie_viz, 
							  struct sum_hash_tbl_c *h_tbl);
void mark_all_edges_on_triangle(const int nnod_4_surf, long **ie_viz, 
								struct sum_hash_tbl_c *h_tbl);


/* T_SURF_EDGE_HASH_C__*/
#endif
