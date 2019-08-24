
/* read_grouping_mesh_c.c */


#include "read_grouping_mesh_c.h"

FILE *fp_grp_mesh;


void read_grouping_mesh_c(char *file_name, struct grouping_data *mesh_g, struct viewer_mesh *mesh_s){
	char buf[LENGTHBUF];    /* array for reading line */
	int itmp;
	long offset = 0;
	long sum_offset = 0;
	int i;
	 	
	/* Error for failed file*/ 	
	if ((fp_grp_mesh = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	}
	/* Skip comment lines*/
	offset = skip_comment_c(fp_grp_mesh);
	sum_offset = offset;
	
	fgets(buf, LENGTHBUF, fp_grp_mesh);
	sscanf(buf, "%d", &itmp);
	fgets(buf, LENGTHBUF, fp_grp_mesh);
	sscanf(buf, "%d", &itmp);
	
	offset = skip_comment_c(fp_grp_mesh);
	sum_offset = offset + sum_offset;
	fscanf(fp_grp_mesh, "%d %d", &mesh_g->numnod_layer,&itmp);
	
	alloc_grouping_node(mesh_g);
	for (i= 0; i < mesh_g->numnod_layer; i++) {
		fgets(buf, LENGTHBUF, fp_grp_mesh);
		sscanf(buf, "%d %lf %lf %lf", &itmp, 
				&mesh_g->rtp_layer[i][0], 
				&mesh_g->rtp_layer[i][1], 
				&mesh_g->rtp_layer[i][2]);
	};
	
	offset = skip_comment_c(fp_grp_mesh);
	sum_offset = offset + sum_offset;
	
	fgets(buf, LENGTHBUF, fp_grp_mesh);
	sscanf(buf, "%d", &mesh_g->numsurf_layer);
	alloc_grouping_sf_type(mesh_g);
	
	for (i = 0; i < mesh_g->numsurf_layer; i++) {
		fscanf(fp_grp_mesh, "%d", &mesh_g->surftyp_layer[i]);
	};
	
	alloc_grouping_surf_param(mesh_g);
	alloc_grouping_surf_connect(mesh_g);
	
	offset = skip_comment_c(fp_grp_mesh);
	sum_offset = offset + sum_offset;
	for (i = 0; i < mesh_g->numsurf_layer; i++) {
		fgets(buf, LENGTHBUF, fp_grp_mesh);
		sscanf(buf, "%d %d %d %d %d", &itmp, 
				&mesh_g->ie_layer[i][0], 
				&mesh_g->ie_layer[i][1], 
				&mesh_g->ie_layer[i][2], 
				&mesh_g->ie_layer[i][3]);
	}
	
	offset = skip_comment_c(fp_grp_mesh);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp_grp_mesh);
	sscanf(buf, "%d", &itmp);
	alloc_grouping_edge_4_sf(mesh_g);
	
	for (i = 0; i < mesh_g->numedge_layer; i++) {
		fgets(buf, LENGTHBUF, fp_grp_mesh);
		sscanf(buf, "%d %d %d", &itmp, 
				&mesh_g->ie_edge_layer[i][0], 
				&mesh_g->ie_edge_layer[i][1]);
	}
	
	offset = skip_comment_c(fp_grp_mesh);
	sum_offset = offset + sum_offset;
	fgets(buf, LENGTHBUF, fp_grp_mesh);
	sscanf(buf, "%d", &itmp);
	
	for (i = 0; i < mesh_g->numsurf_layer; i++) {
		fgets(buf, LENGTHBUF, fp_grp_mesh);
		sscanf(buf, "%d %d %d %d %d", &itmp, 
				&mesh_g->iedge_sf_layer[i][0], 
				&mesh_g->iedge_sf_layer[i][1], 
				&mesh_g->iedge_sf_layer[i][2], 
				&mesh_g->iedge_sf_layer[i][3]);
	}
	
	
	fclose(fp_grp_mesh);                                /* close file */
	
	return;
};



void read_coef_file_for_snap_c(char *file_name, struct grouping_data *mesh_g, int istep_target){
	char buf[LENGTHBUF];    /* array for reading line */
	int istep, itmp;
	double time;
	int i, j;
	 	
	/* Error for failed file*/ 	
	if ((fp_grp_mesh = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	}
	/* count components*/
	
	mesh_g->num_comp = count_comps_by_comma_c(fp_grp_mesh);
	fclose(fp_grp_mesh);                                /* close file */
	
	alloc_group_coefs(mesh_g);
			
	/* open again*/ 
	if ((fp_grp_mesh = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	}
	
	read_multi_field_name(fp_grp_mesh, mesh_g->comp_name);
	
	istep = 0;
	while (istep == istep_target-1){
		for (i=0;i<  mesh_g->numsurf_layer;i++){
			fscanf(fp_grp_mesh, "%d %lf %d", &istep, &time, &itmp);
			for (j=0;j<mesh_g->num_comp;j++) fscanf(fp_grp_mesh, "%lf", &mesh_g->coef[j][i]);
			fgets(buf, LENGTHBUF, fp_grp_mesh);
		};
	};
	
	fclose(fp_grp_mesh);                                /* close file */
	
	return;
};


void read_tave_coef_file_c(char *file_name, struct grouping_data *mesh_g){
	char buf[LENGTHBUF];    /* array for reading line */
	double time;
	int itmp;
	int i, j, istep;
	long offset;
	 	
	/* Error for failed file*/ 	
	if ((fp_grp_mesh = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	}
	/* count components*/
	
	offset = skip_comment_c(fp_grp_mesh);
	fgets(buf, LENGTHBUF, fp_grp_mesh);
	sscanf(buf, "%d", &itmp);
	fgets(buf, LENGTHBUF, fp_grp_mesh);
	sscanf(buf, "%lf", &time);
	
	offset = skip_comment_c(fp_grp_mesh);
	fgets(buf, LENGTHBUF, fp_grp_mesh);
	sscanf(buf, "%d", &mesh_g->num_comp);
	
	alloc_group_coefs(mesh_g);
	
	read_multi_field_name(fp_grp_mesh, mesh_g->comp_name);
	
	for (i=0;i<  mesh_g->numsurf_layer;i++){
		fscanf(fp_grp_mesh, "%d %lf %d", &istep, &time, &itmp);
		for (j=0;j<mesh_g->num_comp;j++) fscanf(fp_grp_mesh, "%lf", &mesh_g->coef[j][i]);
		fgets(buf, LENGTHBUF, fp_grp_mesh);
	}
	
	fclose(fp_grp_mesh);                                /* close file */
	
	return;
};

