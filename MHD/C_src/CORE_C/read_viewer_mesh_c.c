
/* read_viewer_mesh_c.c */


#include "read_viewer_mesh_c.h"

FILE *fp_mesh;

static long read_listed_item_viewer(int num, int *item_sf){
	long offset;
	int i;
	offset = skip_comment_c(fp_mesh);
	for (i = 0; i < num; i++) {
		fscanf(fp_mesh, "%d", &item_sf[i]);
	}
	
	return offset;
};

static long read_group_stack_4_viewer(int ngrp, int *stack_sf){
	return read_listed_item_viewer(ngrp, &stack_sf[1]);
};

static long read_group_item_4_viewer(long sum_o, int npe, int ngrp, int *stack_sf, int *istack_pe,
									char **name, int *item){
	int j,ip, ist, num;
    long offset;
	char buf[LENGTHBUF];            /* character buffer for reading line */
	
	offset = sum_o;
	for (j = 0; j < ngrp; j++) {
		offset = offset + skip_comment_c(fp_mesh);
		
		fgets(buf, LENGTHBUF, fp_mesh);
		sscanf(buf, "%s", name[j]);
		
		offset = read_group_stack_4_viewer(npe, istack_pe);
		for (ip=0;ip<npe;ip++){
			stack_sf[npe*j+ip+1] = istack_pe[ip+1] + stack_sf[npe*j];
		};
		offset = read_group_stack_4_viewer(npe, istack_pe);
		for (ip=0;ip<npe;ip++){
			ist = stack_sf[npe*j+ip];
			num = stack_sf[npe*j+ip+1] - stack_sf[npe*j+ip];
			if(num > 0){
				offset = read_listed_item_viewer(num, &item[ist]);
			};
		}
	};
	return offset;
};

int read_viewer_mesh(const char *file_name, struct viewer_mesh *mesh_s){
	char buf[LENGTHBUF];    /* array for reading line */
	char name_tmp[4096];
	char **tmp_name_sf;
	int *istack_pe, *istack_grp;
	int itmp;
	long offset = 0;
	long sum_offset = 0;
	int i;
	
	printf("kemoviewer mesh file name: %s \n",file_name);
	
	/* Error for failed file*/ 	
	if ((fp_mesh = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		return 1;                    /* terminate with error message */
	}
	/* Skip comment lines*/
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	
	fgets(buf, LENGTHBUF, fp_mesh);
 	sscanf(buf, "%d", &mesh_s->num_pe_sf);
	
	alloc_nummesh_viewer_s(mesh_s);
	alloc_domain_stack_viewer_s(mesh_s);
	istack_pe =  (int *)calloc((mesh_s->num_pe_sf)+1,sizeof(int));
	
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	
	fgets(buf, LENGTHBUF, fp_mesh);
	sscanf(buf, "%d", &mesh_s->nnod_viewer);

	offset = read_group_stack_4_viewer(mesh_s->num_pe_sf, mesh_s->inod_sf_stack);
	offset = read_group_stack_4_viewer(mesh_s->num_pe_sf, istack_pe);
	
	alloc_node_viewer_s(mesh_s);
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	for (i= 0; i < mesh_s->nnod_viewer; i++) {
		fgets(buf, LENGTHBUF, fp_mesh);
		sscanf(buf, "%d %lf %lf %lf", &itmp, 
				&mesh_s->xx_view[4*i ],
				&mesh_s->xx_view[4*i+1],
				&mesh_s->xx_view[4*i+2]);
	};
	/*
	printf("mesh_s->xx_view %e %e %e\n",
			mesh_s->xx_view[4*mesh_s->nnod_viewer-3],
			mesh_s->xx_view[4*mesh_s->nnod_viewer-2],
			mesh_s->xx_view[4*mesh_s->nnod_viewer-1]);
	*/
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	
	fgets(buf, LENGTHBUF, fp_mesh);
	sscanf(buf, "%d", &mesh_s->nsurf_viewer);
	offset = read_group_stack_4_viewer(mesh_s->num_pe_sf, mesh_s->isurf_sf_stack);
	offset = read_group_stack_4_viewer(mesh_s->num_pe_sf, istack_pe);
	
	alloc_sf_type_viewer_s(mesh_s);
	offset = read_listed_item_viewer(mesh_s->nsurf_viewer, mesh_s->surftyp_viewer);
	
	offset = read_group_stack_4_viewer(mesh_s->num_pe_sf, istack_pe);
	offset = read_group_stack_4_viewer(mesh_s->num_pe_sf, istack_pe);

	alloc_surface_params_s(mesh_s);
	alloc_surf_connect_viewer_s(mesh_s);
	
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	if( mesh_s->nnod_4_surf == 9 ){
		for (i = 0; i < mesh_s->nsurf_viewer; i++) {
			fgets(buf, LENGTHBUF, fp_mesh);
			sscanf(buf, "%d %d %d %d %d %d %d %d %d %d", &itmp, 
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
			fgets(buf, LENGTHBUF, fp_mesh);
			sscanf(buf, "%d %d %d %d %d %d %d %d %d", &itmp, 
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
			fgets(buf, LENGTHBUF, fp_mesh);
			sscanf(buf, "%d %d %d %d %d", &itmp, 
                   &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i    ],
                   &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 1],
                   &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 2],
                   &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * i + 3]);
		}
	}
	
/*	printf("mesh_s->ie_sf_viewer %d %d %d %d\n", 
			mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * (mesh_s->nsurf_viewer-1)   ],
            mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * (mesh_s->nsurf_viewer-1) + 1],
            mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * (mesh_s->nsurf_viewer-1) + 2],
            mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf * (mesh_s->nsurf_viewer-1) + 3]);
	*/
	
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp_mesh);
	sscanf(buf, "%d", &mesh_s->nedge_viewer);
	offset = read_group_stack_4_viewer(mesh_s->num_pe_sf, mesh_s->iedge_sf_stack);
	offset = read_group_stack_4_viewer(mesh_s->num_pe_sf, istack_pe);
	
	alloc_edge_4_sf_viewer_s(mesh_s);
	
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	if( mesh_s->nnod_4_edge == 3 ){
		for (i = 0; i < mesh_s->nedge_viewer; i++) {
			fgets(buf, LENGTHBUF, fp_mesh);
			sscanf(buf, "%d %d %d %d", &itmp, 
				&mesh_s->ie_edge_viewer[mesh_s->nnod_4_edge * i    ],
				&mesh_s->ie_edge_viewer[mesh_s->nnod_4_edge * i + 1],
				&mesh_s->ie_edge_viewer[mesh_s->nnod_4_edge * i + 2]);
		}
	}
	else{
		for (i = 0; i < mesh_s->nedge_viewer; i++) {
			fgets(buf, LENGTHBUF, fp_mesh);
			sscanf(buf, "%d %d %d", &itmp, 
				&mesh_s->ie_edge_viewer[mesh_s->nnod_4_edge * i    ],
				&mesh_s->ie_edge_viewer[mesh_s->nnod_4_edge * i + 1]);
		}
	}
	
/*
     printf("mesh_s->ie_edge_viewer %d %d\n",
             mesh_s->ie_edge_viewer[mesh_s->nnod_4_edge * (mesh_s->nedge_viewer-1)    ],
             mesh_s->ie_edge_viewer[mesh_s->nnod_4_edge * (mesh_s->nedge_viewer-1) + 1]);
*/
	
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp_mesh);
	sscanf(buf, "%d", &itmp);
	
	offset = read_group_stack_4_viewer(mesh_s->num_pe_sf, istack_pe);
	offset = read_group_stack_4_viewer(mesh_s->num_pe_sf, istack_pe);

	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	for (i = 0; i < mesh_s->nsurf_viewer; i++) {
		fgets(buf, LENGTHBUF, fp_mesh);
		sscanf(buf, "%d %d %d %d %d", &itmp, 
				&mesh_s->iedge_sf_viewer[mesh_s->nedge_4_surf * i    ],
				&mesh_s->iedge_sf_viewer[mesh_s->nedge_4_surf * i + 1],
				&mesh_s->iedge_sf_viewer[mesh_s->nedge_4_surf * i + 2],
				&mesh_s->iedge_sf_viewer[mesh_s->nedge_4_surf * i + 3]);
	}
	
	/*  node ID for domain boundary */
	
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp_mesh);
	sscanf(buf, "%s", name_tmp);
	offset = read_group_stack_4_viewer(mesh_s->num_pe_sf, mesh_s->nod_stack_domain_sf);
	offset = read_group_stack_4_viewer(mesh_s->num_pe_sf, istack_pe);
	
	mesh_s->nnod_domain_sf = mesh_s->nod_stack_domain_sf[mesh_s->num_pe_sf];
	alloc_domain_nod_item_viewer_s(mesh_s);

	offset = read_listed_item_viewer(mesh_s->nnod_domain_sf, mesh_s->nod_item_domain_sf);
	
	/*  surface ID for domain boundary */
	
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp_mesh);
	sscanf(buf, "%s", name_tmp);
	offset = read_group_stack_4_viewer(mesh_s->num_pe_sf, mesh_s->isurf_stack_domain_sf);
	offset = read_group_stack_4_viewer(mesh_s->num_pe_sf, istack_pe);
	
	mesh_s->nsurf_domain_sf = mesh_s->isurf_stack_domain_sf[mesh_s->num_pe_sf];
	alloc_domain_surf_item_viewer_s(mesh_s);
	
	offset = read_listed_item_viewer(mesh_s->nsurf_domain_sf, mesh_s->isurf_domain_sf);
	
	/*  edge ID for domain boundary */
	
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp_mesh);
	sscanf(buf, "%s", name_tmp);
	offset = read_group_stack_4_viewer(mesh_s->num_pe_sf, mesh_s->edge_stack_domain_sf);
	offset = read_group_stack_4_viewer(mesh_s->num_pe_sf, istack_pe);
	
	mesh_s->nedge_domain_sf = mesh_s->edge_stack_domain_sf[mesh_s->num_pe_sf];
	alloc_domain_edge_item_viewer_s(mesh_s);
	
	offset = read_listed_item_viewer(mesh_s->nedge_domain_sf, mesh_s->edge_item_domain_sf);
	
	/* node group */
	
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp_mesh);
	sscanf(buf, "%d", &mesh_s->ngrp_nod_sf);
	istack_grp =  (int *)calloc((mesh_s->ngrp_nod_sf)+1,sizeof(int));
	alloc_nod_grp_stack_viewer_s(mesh_s);
	
	offset = read_group_stack_4_viewer(mesh_s->ngrp_nod_sf, istack_grp);
	mesh_s->nnod_nod_sf = istack_grp[mesh_s->ngrp_nod_sf];
	alloc_nod_grp_item_viewer_s(mesh_s);
	
	sum_offset = read_group_item_4_viewer(sum_offset, 
			mesh_s->num_pe_sf, mesh_s->ngrp_nod_sf, mesh_s->nod_stack_sf, istack_pe, 
			mesh_s->nod_gp_name_sf, mesh_s->nod_item_sf);
	free(istack_grp);
	
	/* element group */
	
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp_mesh);
	sscanf(buf, "%d", &mesh_s->ngrp_ele_sf);

	istack_grp =  (int *)calloc((mesh_s->ngrp_ele_sf)+1,sizeof(int));
	alloc_ele_grp_stack_viewer_s(mesh_s);
	
	tmp_name_sf = (char **)calloc(mesh_s->ngrp_ele_sf, sizeof(char *));
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++) {
		tmp_name_sf[i] = (char *)calloc(KCHARA_C, sizeof(char));
	};
	
	offset = read_group_stack_4_viewer(mesh_s->ngrp_ele_sf, istack_grp);
	mesh_s->nele_ele_sf = istack_grp[mesh_s->ngrp_ele_sf];
	alloc_ele_grp_item_viewer_s(mesh_s);
	
	sum_offset = read_group_item_4_viewer(sum_offset, 
			mesh_s->num_pe_sf, mesh_s->ngrp_ele_sf, mesh_s->ele_stack_sf, istack_pe, 
			mesh_s->ele_gp_name_sf, mesh_s->ele_item_sf);
	
	
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp_mesh);
	sscanf(buf, "%d", &itmp);
	
	offset = read_group_stack_4_viewer(mesh_s->ngrp_ele_sf, istack_grp);
	mesh_s->nnod_ele_sf = istack_grp[mesh_s->ngrp_ele_sf];
	alloc_ele_grp_nod_item_viewer_s(mesh_s);
	
	sum_offset = read_group_item_4_viewer(sum_offset, 
			mesh_s->num_pe_sf, mesh_s->ngrp_ele_sf, mesh_s->ele_nod_stack_sf, istack_pe, 
			tmp_name_sf, mesh_s->ele_nod_item_sf);
	
	
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp_mesh);
	sscanf(buf, "%d", &itmp);
	
	offset = read_group_stack_4_viewer(mesh_s->ngrp_ele_sf, istack_grp);
	mesh_s->nedge_ele_sf = istack_grp[mesh_s->ngrp_ele_sf];
	alloc_ele_grp_edge_item_viewer_s(mesh_s);
	
	sum_offset = read_group_item_4_viewer(sum_offset, 
			mesh_s->num_pe_sf, mesh_s->ngrp_ele_sf, mesh_s->ele_edge_stack_sf, istack_pe, 
			tmp_name_sf, mesh_s->ele_edge_item_sf);
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++) free(tmp_name_sf[i]);
	free(tmp_name_sf);
	free(istack_grp);
	
	/* surface group */
	
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp_mesh);
	sscanf(buf, "%d", &mesh_s->ngrp_surf_sf);
	istack_grp =  (int *)calloc((mesh_s->ngrp_surf_sf)+1,sizeof(int));
	alloc_surf_grp_stack_viewer_s(mesh_s);
	
	tmp_name_sf = (char **)calloc(mesh_s->ngrp_surf_sf, sizeof(char *));
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++) {
		tmp_name_sf[i] = (char *)calloc(KCHARA_C, sizeof(char));
	};
	
	offset = read_group_stack_4_viewer(mesh_s->ngrp_surf_sf, istack_grp);
	mesh_s->nsurf_surf_sf = istack_grp[mesh_s->ngrp_surf_sf];
	alloc_surf_grp_item_viewer_s(mesh_s);
	
	sum_offset = read_group_item_4_viewer(sum_offset, 
			mesh_s->num_pe_sf, mesh_s->ngrp_surf_sf, mesh_s->surf_stack_sf, istack_pe, 
			mesh_s->surf_gp_name_sf, mesh_s->surf_item_sf);
	
	
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp_mesh);
	sscanf(buf, "%d", &itmp);
	
	offset = read_group_stack_4_viewer(mesh_s->ngrp_surf_sf, istack_grp);
	mesh_s->nnod_surf_sf = istack_grp[mesh_s->ngrp_surf_sf];
	alloc_surf_grp_nod_item_viewer_s(mesh_s);
	
	sum_offset = read_group_item_4_viewer(sum_offset, 
			mesh_s->num_pe_sf, mesh_s->ngrp_surf_sf, mesh_s->surf_nod_stack_sf, istack_pe, 
			tmp_name_sf, mesh_s->surf_nod_item_sf);
	
	
	offset = skip_comment_c(fp_mesh);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp_mesh);
	sscanf(buf, "%d", &itmp);
	
	offset = read_group_stack_4_viewer(mesh_s->ngrp_surf_sf, istack_grp);
	mesh_s->nedge_surf_sf = istack_grp[mesh_s->ngrp_surf_sf];
	alloc_surf_grp_edge_item_viewer_s(mesh_s);
	
	sum_offset = read_group_item_4_viewer(sum_offset, 
			mesh_s->num_pe_sf, mesh_s->ngrp_surf_sf, mesh_s->surf_edge_stack_sf, istack_pe, 
			tmp_name_sf, mesh_s->surf_edge_item_sf);
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++) free(tmp_name_sf[i]);
	free(tmp_name_sf);
	free(istack_grp);
	free(istack_pe);

	fclose(fp_mesh);                                /* close file */
	
	return 0;
};
