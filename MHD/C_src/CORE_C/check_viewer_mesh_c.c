
/* check_viewer_mesh_c.c */

#include "check_viewer_mesh_c.h"

/*void check_viewer_kemo(struct viewer_mesh *mesh_s)*/


void check_viewer_kemo(struct viewer_mesh *mesh_s){
	int i, j;
	/* int k, jst, jed; */
	
	printf("num_pe_sf: %d \n", mesh_s->num_pe_sf);
	printf("nnod_viewer: %d \n", mesh_s->nnod_viewer);
	printf("nsurf_viewer: %d \n", mesh_s->nsurf_viewer);
	printf("nedge_viewer: %d \n", mesh_s->nedge_viewer);
	
	printf("inod_sf_stack");
	for (i = 0; i < mesh_s->num_pe_sf+1; i++) {
		printf(" %d", mesh_s->inod_sf_stack[i]);
	};
	printf("\n");
	printf("isurf_sf_stack");
	for (i = 0; i < mesh_s->num_pe_sf+1; i++) {
		printf(" %d", mesh_s->isurf_sf_stack[i]);
	};
	printf("\n");
	printf("iedge_sf_stack");
	for (i = 0; i < mesh_s->num_pe_sf+1; i++) {
		printf(" %d", mesh_s->iedge_sf_stack[i]);
	};
	printf("\n");
	
	
	/* printf("node \n");
	for (i = 0; i < mesh_s->nnod_viewer; i++) {
		printf(" %d %.12e %.12e %.12e \n", i,
                mesh_s->xx_view[4*i], mesh_s->xx_view[4*i+1], mesh_s->xx_view[4*i+2]);
	};
	*/
	
	/* printf("element type \n");
	for (i = 0; i < mesh_s->nsurf_viewer; i++) {
		printf(" %d %d \n", i, mesh_s->surftyp_viewer[i]);
	};
	*/
	
	printf("nnod_4_surf: %d \n", mesh_s->nnod_4_surf);
	printf("nnod_4_edge: %d \n", mesh_s->nnod_4_edge);
	printf("nedge_4_surf: %d \n", mesh_s->nedge_4_surf);
	printf("nedge_4_surf: %d \n", mesh_s->nsurf_each_tri);
	
	/*
	printf("connectivity \n");
	if( mesh_s->nnod_4_surf == 9 ){
		for (i = 0; i < mesh_s->nsurf_viewer; i++) {
			printf(" %d %d %d %d %d %d %d %d %d %d \n", i, 
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i    ],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 1],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 2],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 3],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 4],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 5],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 6],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 7],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 8]);
		}
	}
	else if( mesh_s->nnod_4_surf == 8 ){
		for (i = 0; i < mesh_s->nsurf_viewer; i++) {
			printf(" %d %d %d %d %d %d %d %d %d \n", i, 
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i    ],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 1],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 2],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 3],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 4],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 5],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 6],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 7]);
		}
	}
	else{
		for (i = 0; i < mesh_s->nsurf_viewer; i++) {
			printf(" %d %d %d %d %d \n", i, 
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i    ],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 1],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 2],
                    &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 3]);
		}
	}
	*/
	/*
	printf("edge connectivity \n");
	if( mesh_s->nnod_4_edge == 3 ){
		for (i = 0; i < mesh_s->nedge_viewer; i++) {
			printf(" %d %d %d %d \n", i, 
					mesh_s->ie_edge_viewer[mesh_s->nnod_4_edge * i    ],
					mesh_s->ie_edge_viewer[mesh_s->nnod_4_edge * i + 1],
					mesh_s->ie_edge_viewer[mesh_s->nnod_4_edge * i + 2]);
		}
	}
	else{
		for (i = 0; i < mesh_s->nedge_viewer; i++) {
			printf(" %d %d %d \n", i, 
					mesh_s->ie_edge_viewer[mesh_s->nnod_4_edge * i    ],
					mesh_s->ie_edge_viewer[mesh_s->nnod_4_edge * i + 1]);
		}
	}
	*/
	/*
	printf("edge connection for surface \n");
	for (i = 0; i < mesh_s->nsurf_viewer; i++) {
		printf(" %d %d %d %d %d \n", i, 
				mesh_s->iedge_sf_viewer[mesh_s->nedge_4_surf * i   0],
				mesh_s->iedge_sf_viewer[mesh_s->nedge_4_surf * i + 1],
				mesh_s->iedge_sf_viewer[mesh_s->nedge_4_surf * i + 2],
				mesh_s->iedge_sf_viewer[mesh_s->nedge_4_surf * i + 3]);
	}
	*/
	
	printf("nnod_domain_sf: %d \n", mesh_s->nnod_domain_sf);
	printf("nod_stack_domain_sf");
	for (i = 0; i < mesh_s->num_pe_sf+1; i++) {
		printf(" %d", mesh_s->nod_stack_domain_sf[i]);
	};
	printf("\n");
	
	/*
	printf("nod_item_domain_sf");
	for (i = 0; i < mesh_s->nnod_domain_sf; i++) {
		printf(" %d", mesh_s->nod_item_domain_sf[i]);
	};
	printf("\n");
	*/
	
	
	printf("nsurf_domain_sf: %d \n", mesh_s->nsurf_domain_sf);
	printf("isurf_stack_domain_sf");
	for (i = 0; i < mesh_s->num_pe_sf+1; i++) {
		printf(" %d", mesh_s->isurf_stack_domain_sf[i]);
	};
	printf("\n");
	
	/*
	printf("isurf_domain_sf");
	for (i = 0; i < mesh_s->nsurf_domain_sf; i++) {
		printf(" %d", mesh_s->isurf_domain_sf[i]);
	};
	printf("\n");
	*/
	
	
	printf("nedge_domain_sf: %d \n", mesh_s->nedge_domain_sf);
	printf("edge_stack_domain_sf");
	for (i = 0; i < mesh_s->num_pe_sf+1; i++) {
		printf(" %d", mesh_s->edge_stack_domain_sf[i]);
	};
	printf("\n");
	
	/*
	printf("edge_item_domain_sf");
	for (i = 0; i < mesh_s->nedge_domain_sf; i++) {
		printf(" %d", mesh_s->edge_item_domain_sf[i]);
	};
	printf("\n");
	*/
	
	printf("ngrp_nod_sf: %d \n", mesh_s->ngrp_nod_sf);
	
	printf("nod_stack_sf");
	printf(" %d ", mesh_s->nod_stack_sf[0]);
	printf("\n");
	for (j = 0; j < mesh_s->ngrp_nod_sf; j++) {
		printf("%s ", mesh_s->nod_gp_name_sf[j]);
		for (i = j*mesh_s->num_pe_sf; i < (j+1)*mesh_s->num_pe_sf; i++) {
			printf(" %d ", mesh_s->nod_stack_sf[i+1]);
		};
		printf("\n");
		/*
		jst = mesh_s->nod_stack_sf[mesh_s->num_pe_sf*j    ];
		jed = mesh_s->nod_stack_sf[mesh_s->num_pe_sf*(j+1)];
		for (i = jst; i < jed; i++) {
			printf(" %d ", mesh_s->nod_item_sf[i]);
		};
		printf("\n");
		*/
	};
	
	printf("ngrp_ele_sf: %d %d \n", mesh_s->ngrp_ele_sf, mesh_s->nele_ele_sf);
	
	printf("ele_stack_sf");
	printf(" %d ", mesh_s->ele_stack_sf[0]);
	printf("\n");
	for (j = 0; j < mesh_s->ngrp_ele_sf; j++) {
		printf("%s ", mesh_s->ele_gp_name_sf[j]);
		for (i = j*mesh_s->num_pe_sf; i < (j+1)*mesh_s->num_pe_sf; i++) {
			printf(" %d ", mesh_s->ele_stack_sf[i+1]);
		};
		printf("\n");
		/*
		jst = mesh_s->ele_stack_sf[mesh_s->num_pe_sf*j    ];
		jed = mesh_s->ele_stack_sf[mesh_s->num_pe_sf*(j+1)];
		for (i = jst; i < jed; i++) {
			printf(" %d ", mesh_s->ele_item_sf[i]);
		};
		printf("\n");
		*/
	};
	
	printf("ele_nod_stack_sf");
	printf(" %d ", mesh_s->ele_nod_stack_sf[0]);
	printf("\n");
	for (j = 0; j < mesh_s->ngrp_ele_sf; j++) {
		printf("%s ", mesh_s->ele_gp_name_sf[j]);
		for (i = j*mesh_s->num_pe_sf; i < (j+1)*mesh_s->num_pe_sf; i++) {
			printf(" %d ", mesh_s->ele_nod_stack_sf[i+1]);
		};
		printf("\n");
		/*
		jst = mesh_s->ele_nod_stack_sf[mesh_s->num_pe_sf*j    ];
		jed = mesh_s->ele_nod_stack_sf[mesh_s->num_pe_sf*(j+1)];
		for (i = jst; i < jed; i++) {
			printf(" %d ", mesh_s->ele_nod_item_sf[i]);
		};
		printf("\n");
		*/
	};
	
	printf("ele_edge_stack_sf");
	printf(" %d ", mesh_s->ele_edge_stack_sf[0]);
	printf("\n");
	for (j = 0; j < mesh_s->ngrp_ele_sf; j++) {
		printf("%s ", mesh_s->ele_gp_name_sf[j]);
		for (i = j*mesh_s->num_pe_sf; i < (j+1)*mesh_s->num_pe_sf; i++) {
			printf(" %d ", mesh_s->ele_edge_stack_sf[i+1]);
		};
		printf("\n");
		/*
		jst = mesh_s->ele_edge_stack_sf[mesh_s->num_pe_sf*j    ];
		jed = mesh_s->ele_edge_stack_sf[mesh_s->num_pe_sf*(j+1)];
		for (i = jst; i < jed; i++) {
			printf(" %d ", mesh_s->ele_edge_item_sf[i]);
		};
		printf("\n");
		*/
	};
	
	
	printf("ngrp_surf_sf: %d %d \n", mesh_s->ngrp_surf_sf, mesh_s->nsurf_surf_sf);
	
	printf("surf_stack_sf");
	printf(" %d ", mesh_s->surf_stack_sf[0]);
	printf("\n");
	for (j = 0; j < mesh_s->ngrp_surf_sf; j++) {
		printf("%s ", mesh_s->surf_gp_name_sf[j]);
		for (i = j*mesh_s->num_pe_sf; i < (j+1)*mesh_s->num_pe_sf; i++) {
			printf(" %d ", mesh_s->surf_stack_sf[i+1]);
		};
		printf("\n");
		/*
		jst = mesh_s->surf_stack_sf[mesh_s->num_pe_sf*j    ];
		jed = mesh_s->surf_stack_sf[mesh_s->num_pe_sf*(j+1)];
		for (i = jst; i < jed; i++) {
			printf(" %d ", mesh_s->surf_item_sf[i]);
		};
		printf("\n");
		*/
	};
	
	printf("surf_nod_stack_sf");
	printf(" %d ", mesh_s->surf_nod_stack_sf[0]);
	printf("\n");
	for (j = 0; j < mesh_s->ngrp_surf_sf; j++) {
		printf("%s ", mesh_s->surf_gp_name_sf[j]);
		for (i = j*mesh_s->num_pe_sf; i < (j+1)*mesh_s->num_pe_sf; i++) {
			printf(" %d ", mesh_s->surf_nod_stack_sf[i+1]);
		};
		printf("\n");
		/*
		jst = mesh_s->surf_nod_stack_sf[mesh_s->num_pe_sf*j    ];
		jed = mesh_s->surf_nod_stack_sf[mesh_s->num_pe_sf*(j+1)];
		for (i = jst; i < jed; i++) {
			printf(" %d ", mesh_s->surf_nod_item_sf[i]);
		};
		printf("\n");
		*/
	};
	
	printf("surf_edge_stack_sf");
	printf(" %d ", mesh_s->surf_edge_stack_sf[0]);
	printf("\n");
	for (j = 0; j < mesh_s->ngrp_surf_sf; j++) {
		printf("%s ", mesh_s->surf_gp_name_sf[j]);
		for (i = j*mesh_s->num_pe_sf; i < (j+1)*mesh_s->num_pe_sf; i++) {
			printf(" %d ", mesh_s->surf_edge_stack_sf[i+1]);
		};
		printf("\n");
		/*
		jst = mesh_s->surf_edge_stack_sf[mesh_s->num_pe_sf*j    ];
		jed = mesh_s->surf_edge_stack_sf[mesh_s->num_pe_sf*(j+1)];
		for (i = jst; i < jed; i++) {
			printf(" %d ", mesh_s->surf_edge_item_sf[i]);
		};
		printf("\n");
		*/
	};
	/*
	printf("surf_norm_view surf_size_view \n");
	for (i = 0; i < mesh_s->nsurf_viewer; i++) {
		for (j = 0; j < mesh_s->nsurf_each_tri; j++) {
			k = j + i*mesh_s->nsurf_each_tri;
			printf("%d %d %.12e %.12e %.12e %.12e \n", i, j, 
					mesh_s->surf_norm_view[4*k  ], mesh_s->surf_norm_view[4*k+1],
					mesh_s->surf_norm_view[4*k+2], mesh_s->surf_size_view[4*k+3] );
		};
	};
	*/
	/*
	printf("surf_center_view \n");
	for (i = 0; i < mesh_s->nsurf_viewer; i++) {
		for (j = 0; j < mesh_s->nsurf_each_tri; j++) {
			k = j + i*mesh_s->nsurf_each_tri;
			printf("%d %d %.12e %.12e %.12e \n", i, j, 
					mesh_s->surf_center_view[4*k  ], mesh_s->surf_center_view[4*k+1],
					mesh_s->surf_center_view[4*k+2]);
		};
	};
	*/
	return;
};

void check_viewer_mesh_minmax(struct viewer_mesh *mesh_s){
	int i;
	printf("domain_center \n");
	for (i = 0; i < mesh_s->num_pe_sf; i++) {
		printf("%d %.12e %.12e %.12e \n", i, 
			   mesh_s->domain_center[4*i  ],
               mesh_s->domain_center[4*i+1],
			   mesh_s->domain_center[4*i+2]);
	};
	
	
	
	printf("domain_min \n");
	for (i = 0; i < mesh_s->num_pe_sf; i++) {
		printf("%d %.12e %.12e %.12e \n", i, 
			   mesh_s->domain_min[4*i+0], mesh_s->domain_min[4*i+1],
			   mesh_s->domain_min[4*i+2] );
	};
	
	
	
	printf("domain_max \n");
	for (i = 0; i < mesh_s->num_pe_sf; i++) {
		printf("%d %.12e %.12e %.12e \n", i, 
			   mesh_s->domain_max[4*i+0], mesh_s->domain_max[i+1],
			   mesh_s->domain_max[4*i+2] );
	};
	
	return;
};
