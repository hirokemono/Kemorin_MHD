
/* read_viewer_mesh_c.c */


#include "read_viewer_mesh_c.h"

FILE *fp;

static void read_listed_item_viewer(int num, int *item_sf){
	int i;
	for (i = 0; i < num; i++) {
		fscanf(fp, "%d", &item_sf[i]);
	}
	
	return;
};

static void read_group_stack_4_viewer(int npe, int ngrp, int *stack_sf){
	int i, num;
	
	num = npe*ngrp;
	for (i = 0; i < num; i++) {
		fscanf(fp, "%d", &stack_sf[i+1]);
	};
	return;
};

static long read_group_item_4_viewer(int sum_o, int npe, int ngrp, int *stack_sf,
									char **name, int *item){
	int i, j, jst, jed;
	long sum_offset=0;
    long offset;
	char buf[LENGTHBUF];            /* character buffer for reading line */
	
	for (j = 0; j < ngrp; j++) {
		offset = skip_comment_c(fp);
		sum_offset = offset + sum_o;
		
		fgets(buf, LENGTHBUF, fp);
		sscanf(buf, "%s", name[j]);
		
		jst = stack_sf[npe*j    ];
		jed = stack_sf[npe*(j+1)];
		for (i = jst; i < jed; i++) {
			fscanf(fp, "%d", &item[i]);
		};
	};
	return sum_offset;
};

int read_viewer_mesh(const char *file_name, struct viewer_mesh *mesh_s){
	char buf[LENGTHBUF];    /* array for reading line */
	char **tmp_name_sf;
	int itmp;
	long offset = 0;
	long sum_offset = 0;
	int i;
	
	printf("kemoviewer mesh file name: %s \n",file_name);
	
	/* Error for failed file*/ 	
	if ((fp = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		return 1;                    /* terminate with error message */
	}
	/* Skip comment lines*/
	offset = skip_comment_c(fp);
	sum_offset = offset + sum_offset;
	
	fgets(buf, LENGTHBUF, fp);
 	sscanf(buf, "%d", &mesh_s->num_pe_sf);
	
	alloc_nummesh_viewer_s(mesh_s);
	alloc_domain_stack_viewer_s(mesh_s);
	
	read_group_stack_4_viewer(mesh_s->num_pe_sf, 1, mesh_s->inod_sf_stack);
	read_group_stack_4_viewer(mesh_s->num_pe_sf, 1, mesh_s->isurf_sf_stack);
	read_group_stack_4_viewer(mesh_s->num_pe_sf, 1, mesh_s->iedge_sf_stack);
	
	mesh_s->nnod_viewer =  mesh_s->inod_sf_stack[mesh_s->num_pe_sf];
	mesh_s->nsurf_viewer = mesh_s->isurf_sf_stack[mesh_s->num_pe_sf];
	mesh_s->nedge_viewer = mesh_s->iedge_sf_stack[mesh_s->num_pe_sf];
	
	offset = skip_comment_c(fp);
	sum_offset = offset + sum_offset;
	
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%d", &itmp);
	
	alloc_node_viewer_s(mesh_s);
	for (i= 0; i < mesh_s->nnod_viewer; i++) {
		fgets(buf, LENGTHBUF, fp);
		sscanf(buf, "%d %lf %lf %lf", &itmp, 
				&mesh_s->xx_view[i][0], 
				&mesh_s->xx_view[i][1], 
				&mesh_s->xx_view[i][2]);
	};
	/*
	printf("mesh_s->xx_view %e %e %e\n",
			mesh_s->xx_view[mesh_s->nnod_viewer-1][0],
			mesh_s->xx_view[mesh_s->nnod_viewer-1][1],
			mesh_s->xx_view[mesh_s->nnod_viewer-1][2]);
	*/
	offset = skip_comment_c(fp);
	sum_offset = offset + sum_offset;
	
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%d", &itmp);
	
	alloc_sf_type_viewer_s(mesh_s);
	read_listed_item_viewer(mesh_s->nsurf_viewer, mesh_s->surftyp_viewer);
	
	alloc_surface_params_s(mesh_s);
	alloc_surf_connect_viewer_s(mesh_s);
	
	offset = skip_comment_c(fp);
	sum_offset = offset + sum_offset;
	if( mesh_s->nnod_4_surf == 9 ){
		for (i = 0; i < mesh_s->nsurf_viewer; i++) {
			fgets(buf, LENGTHBUF, fp);
			sscanf(buf, "%d %d %d %d %d %d %d %d %d %d", &itmp, 
				&mesh_s->ie_sf_viewer[i][0], 
				&mesh_s->ie_sf_viewer[i][1], 
				&mesh_s->ie_sf_viewer[i][2], 
				&mesh_s->ie_sf_viewer[i][3], 
				&mesh_s->ie_sf_viewer[i][4], 
				&mesh_s->ie_sf_viewer[i][5], 
				&mesh_s->ie_sf_viewer[i][6], 
				&mesh_s->ie_sf_viewer[i][7], 
				&mesh_s->ie_sf_viewer[i][8]);
		}
	}
	else if( mesh_s->nnod_4_surf == 8 ){
		for (i = 0; i < mesh_s->nsurf_viewer; i++) {
			fgets(buf, LENGTHBUF, fp);
			sscanf(buf, "%d %d %d %d %d %d %d %d %d", &itmp, 
				&mesh_s->ie_sf_viewer[i][0], 
				&mesh_s->ie_sf_viewer[i][1], 
				&mesh_s->ie_sf_viewer[i][2], 
				&mesh_s->ie_sf_viewer[i][3], 
				&mesh_s->ie_sf_viewer[i][4], 
				&mesh_s->ie_sf_viewer[i][5], 
				&mesh_s->ie_sf_viewer[i][6], 
					&mesh_s->ie_sf_viewer[i][7]);
		}
	}
	else{
		for (i = 0; i < mesh_s->nsurf_viewer; i++) {
			fgets(buf, LENGTHBUF, fp);
			sscanf(buf, "%d %d %d %d %d", &itmp, 
				&mesh_s->ie_sf_viewer[i][0], 
				&mesh_s->ie_sf_viewer[i][1], 
				&mesh_s->ie_sf_viewer[i][2], 
				&mesh_s->ie_sf_viewer[i][3]);
		}
	}
	
/*	printf("mesh_s->ie_sf_viewer %d %d %d %d\n", 
			mesh_s->ie_sf_viewer[mesh_s->nsurf_viewer-1][0],
			mesh_s->ie_sf_viewer[mesh_s->nsurf_viewer-1][1],
			mesh_s->ie_sf_viewer[mesh_s->nsurf_viewer-1][2],
			mesh_s->ie_sf_viewer[mesh_s->nsurf_viewer-1][3]);
	*/
	
	offset = skip_comment_c(fp);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%d", &itmp);
	alloc_edge_4_sf_viewer_s(mesh_s);
	
	if( mesh_s->nnod_4_edge == 3 ){
		for (i = 0; i < mesh_s->nedge_viewer; i++) {
			fgets(buf, LENGTHBUF, fp);
			sscanf(buf, "%d %d %d %d", &itmp, 
				&mesh_s->ie_edge_viewer[i][0], 
				&mesh_s->ie_edge_viewer[i][1], 
				&mesh_s->ie_edge_viewer[i][2]);
		}
	}
	else{
		for (i = 0; i < mesh_s->nedge_viewer; i++) {
			fgets(buf, LENGTHBUF, fp);
			sscanf(buf, "%d %d %d", &itmp, 
				&mesh_s->ie_edge_viewer[i][0], 
				&mesh_s->ie_edge_viewer[i][1]);
		}
	}
	
	/*printf("mesh_s->ie_edge_viewer %d %d\n", 
	mesh_s->ie_edge_viewer[mesh_s->nedge_viewer-1][0],
			mesh_s->ie_edge_viewer[mesh_s->nedge_viewer-1][1]);*/
	
	offset = skip_comment_c(fp);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%d", &itmp);
	
	for (i = 0; i < mesh_s->nsurf_viewer; i++) {
		fgets(buf, LENGTHBUF, fp);
		sscanf(buf, "%d %d %d %d %d", &itmp, 
				&mesh_s->iedge_sf_viewer[i][0], 
				&mesh_s->iedge_sf_viewer[i][1], 
				&mesh_s->iedge_sf_viewer[i][2], 
				&mesh_s->iedge_sf_viewer[i][3]);
	}
	
	/*  node ID for domain boundary */
	
	offset = skip_comment_c(fp);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%d", &mesh_s->nnod_domain_sf);
	read_group_stack_4_viewer(mesh_s->num_pe_sf, 1, mesh_s->nod_stack_domain_sf);
	
	alloc_domain_nod_item_viewer_s(mesh_s);
	read_listed_item_viewer(mesh_s->nnod_domain_sf, mesh_s->nod_item_domain_sf);
	
	/*  surface ID for domain boundary */
	
	offset = skip_comment_c(fp);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%d", &mesh_s->nsurf_domain_sf);
	read_group_stack_4_viewer(mesh_s->num_pe_sf, 1, mesh_s->isurf_stack_domain_sf);
	
	alloc_domain_surf_item_viewer_s(mesh_s);
	read_listed_item_viewer(mesh_s->nsurf_domain_sf, mesh_s->isurf_domain_sf);
	
	/*  edge ID for domain boundary */
	
	offset = skip_comment_c(fp);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%d", &mesh_s->nedge_domain_sf);
	read_group_stack_4_viewer(mesh_s->num_pe_sf, 1, mesh_s->edge_stack_domain_sf);
	
	alloc_domain_edge_item_viewer_s(mesh_s);
	read_listed_item_viewer(mesh_s->nedge_domain_sf, mesh_s->edge_item_domain_sf);
	
	/* node group */
	
	offset = skip_comment_c(fp);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%d", &mesh_s->ngrp_nod_sf);
	alloc_nod_grp_stack_viewer_s(mesh_s);
	
	read_group_stack_4_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_nod_sf, mesh_s->nod_stack_sf);
	mesh_s->nnod_nod_sf = mesh_s->nod_stack_sf[mesh_s->num_pe_sf*mesh_s->ngrp_nod_sf];
	alloc_nod_grp_item_viewer_s(mesh_s);
	
	sum_offset = read_group_item_4_viewer(sum_offset, 
			mesh_s->num_pe_sf, mesh_s->ngrp_nod_sf, mesh_s->nod_stack_sf, 
			mesh_s->nod_gp_name_sf, mesh_s->nod_item_sf);
	
	/* element group */
	
	offset = skip_comment_c(fp);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%d", &mesh_s->ngrp_ele_sf);
	alloc_ele_grp_stack_viewer_s(mesh_s);
	
	tmp_name_sf = (char **)calloc(mesh_s->ngrp_ele_sf, sizeof(char *));
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++) {
		tmp_name_sf[i] = (char *)calloc(KCHARA_C, sizeof(char));
	};
	
	read_group_stack_4_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_ele_sf, mesh_s->ele_stack_sf);
	mesh_s->nele_ele_sf = mesh_s->ele_stack_sf[mesh_s->num_pe_sf*mesh_s->ngrp_ele_sf];
	alloc_ele_grp_item_viewer_s(mesh_s);
	
	sum_offset = read_group_item_4_viewer(sum_offset, 
			mesh_s->num_pe_sf, mesh_s->ngrp_ele_sf, mesh_s->ele_stack_sf, 
			mesh_s->ele_gp_name_sf, mesh_s->ele_item_sf);
	
	
	offset = skip_comment_c(fp);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%d", &itmp);
	
	read_group_stack_4_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_ele_sf, mesh_s->ele_nod_stack_sf);
	mesh_s->nnod_ele_sf = mesh_s->ele_nod_stack_sf[mesh_s->num_pe_sf*mesh_s->ngrp_ele_sf];
	alloc_ele_grp_nod_item_viewer_s(mesh_s);
	
	sum_offset = read_group_item_4_viewer(sum_offset, 
			mesh_s->num_pe_sf, mesh_s->ngrp_ele_sf, mesh_s->ele_nod_stack_sf, 
			tmp_name_sf, mesh_s->ele_nod_item_sf);
	
	
	offset = skip_comment_c(fp);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%d", &itmp);
	
	read_group_stack_4_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_ele_sf, mesh_s->ele_edge_stack_sf);
	mesh_s->nedge_ele_sf = mesh_s->ele_edge_stack_sf[mesh_s->num_pe_sf*mesh_s->ngrp_ele_sf];
	alloc_ele_grp_edge_item_viewer_s(mesh_s);
	
	sum_offset = read_group_item_4_viewer(sum_offset, 
			mesh_s->num_pe_sf, mesh_s->ngrp_ele_sf, mesh_s->ele_edge_stack_sf, 
			tmp_name_sf, mesh_s->ele_edge_item_sf);
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++) free(tmp_name_sf[i]);
	free(tmp_name_sf);
	
	/* surface group */
	
	offset = skip_comment_c(fp);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%d", &mesh_s->ngrp_surf_sf);
	alloc_surf_grp_stack_viewer_s(mesh_s);
	
	tmp_name_sf = (char **)calloc(mesh_s->ngrp_surf_sf, sizeof(char *));
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++) {
		tmp_name_sf[i] = (char *)calloc(KCHARA_C, sizeof(char));
	};
	
	read_group_stack_4_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_surf_sf, mesh_s->surf_stack_sf);
	mesh_s->nsurf_surf_sf = mesh_s->surf_stack_sf[mesh_s->num_pe_sf*mesh_s->ngrp_surf_sf];
	alloc_surf_grp_item_viewer_s(mesh_s);
	
	sum_offset = read_group_item_4_viewer(sum_offset, 
			mesh_s->num_pe_sf, mesh_s->ngrp_surf_sf, mesh_s->surf_stack_sf, 
			mesh_s->surf_gp_name_sf, mesh_s->surf_item_sf);
	
	
	offset = skip_comment_c(fp);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%d", &itmp);
	
	read_group_stack_4_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_surf_sf, mesh_s->surf_nod_stack_sf);
	mesh_s->nnod_surf_sf = mesh_s->surf_nod_stack_sf[mesh_s->num_pe_sf*mesh_s->ngrp_surf_sf];
	alloc_surf_grp_nod_item_viewer_s(mesh_s);
	
	sum_offset = read_group_item_4_viewer(sum_offset, 
			mesh_s->num_pe_sf, mesh_s->ngrp_surf_sf, mesh_s->surf_nod_stack_sf, 
			tmp_name_sf, mesh_s->surf_nod_item_sf);
	
	
	offset = skip_comment_c(fp);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%d", &itmp);
	
	read_group_stack_4_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_surf_sf, mesh_s->surf_edge_stack_sf);
	mesh_s->nedge_surf_sf = mesh_s->surf_edge_stack_sf[mesh_s->num_pe_sf*mesh_s->ngrp_surf_sf];
	alloc_surf_grp_edge_item_viewer_s(mesh_s);
	
	sum_offset = read_group_item_4_viewer(sum_offset, 
			mesh_s->num_pe_sf, mesh_s->ngrp_surf_sf, mesh_s->surf_edge_stack_sf, 
			tmp_name_sf, mesh_s->surf_edge_item_sf);
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++) free(tmp_name_sf[i]);
	free(tmp_name_sf);

	fclose(fp);                                /* close file */
	
	return 0;
};
