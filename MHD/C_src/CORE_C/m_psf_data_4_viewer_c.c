
/*  m_psf_data_4_viewer_c.c */

#include <stdlib.h>
#include "m_psf_data_4_viewer_c.h"


void alloc_viz_node_s(struct psf_data *psf_s){
	int i;
	/* allocate memory  xx_viz[node #][direction]*/
	psf_s->xx_viz = (double **)malloc(psf_s->nnod_viz*sizeof(double *));
	for (i = 0; i < psf_s->nnod_viz; i++){
		psf_s->xx_viz[i] =  (double *)calloc(3,sizeof(double));
	};
	
	psf_s->inod_viz = (long *)calloc(psf_s->nnod_viz,sizeof(long));
	return;
};

void alloc_viz_ele_s(struct psf_data *psf_s){
	int i;
	
	/* allocate memory  ie_viz[patch #][connection]*/
	psf_s->ie_viz = (long **) malloc(psf_s->nele_viz*sizeof(long *));
	for (i = 0; i < psf_s->nele_viz; i++){
		psf_s->ie_viz[i] = (long *)calloc(psf_s->nnod_4_ele_viz,sizeof(long));
	};
	
	/* allocate memory  x_ele_viz[patch #][direction]*/
	psf_s->x_ele_viz = (double **) malloc(psf_s->nele_viz*sizeof(double *));
	for (i = 0; i < psf_s->nele_viz; i++){
		psf_s->x_ele_viz[i] = (double *)calloc(3,sizeof(double));
	};
	
	return;
};

void alloc_psf_field_name_c(struct psf_data *psf_s){
	int i;
	
	psf_s->ncomp =       (long *)calloc(psf_s->nfield,sizeof(long));
	psf_s->istack_comp = (long *)calloc(psf_s->nfield+1,sizeof(long));

	psf_s->id_coord =    (int *)calloc(psf_s->nfield,sizeof(int));
	
	psf_s->data_name = (char **)malloc(psf_s->nfield*sizeof(char *));
	for (i = 0; i < psf_s->nfield; i++) {
		psf_s->data_name[i] = (char *)calloc(KCHARA_C, sizeof(char));
	};
};

void alloc_psf_field_data_c(struct psf_data *psf_s){
	int i;
	/* allocate memory  d_nod[node #][component]*/
	psf_s->d_nod = (double **)malloc(psf_s->nnod_viz*sizeof(double *));
	for (i = 0; i < psf_s->nnod_viz; i++){
		psf_s->d_nod[i] = (double *)calloc(psf_s->ncomptot,sizeof(double));
	};
};

void alloc_psf_data_s(struct psf_data *psf_s){
	int i;
	/* allocate memory  d_amp[node #][field]*/
	psf_s->d_amp = (double **)malloc(psf_s->nnod_viz*sizeof(double *));
	for (i = 0; i < psf_s->nnod_viz; i++){
		psf_s->d_amp[i] = (double *)calloc(psf_s->nfield,sizeof(double));
	};
	
	/* allocate memory  color_nod[node #][rgba code]*/
	psf_s->color_nod = (double **)malloc(psf_s->nnod_viz*sizeof(double *));
	for (i = 0; i < psf_s->nnod_viz; i++){
		psf_s->color_nod[i] = (double *)calloc(IFOUR,sizeof(double));
	};
	
	psf_s->d_min = (double *)calloc(psf_s->ncomptot,sizeof(double));
	psf_s->d_max = (double *)calloc(psf_s->ncomptot,sizeof(double));
	psf_s->d_ave = (double *)calloc(psf_s->ncomptot,sizeof(double));
	psf_s->d_rms = (double *)calloc(psf_s->ncomptot,sizeof(double));
	
	psf_s->amp_min = (double *)calloc(psf_s->nfield,sizeof(double));
	psf_s->amp_max = (double *)calloc(psf_s->nfield,sizeof(double));
	
	return;
};

void alloc_psf_norm_s(struct psf_data *psf_s){
	int i;
	
	/* allocate memory  norm_ele[patch #][component]*/
	psf_s->norm_ele = (double **)malloc(psf_s->nele_viz*sizeof(double *));
	for (i = 0; i < psf_s->nele_viz; i++){
		psf_s->norm_ele[i] = (double *)calloc(3,sizeof(double));
	};
	
	psf_s->area_viz = (double *)calloc(psf_s->nele_viz,sizeof(double));
	
	/* allocate memory  norm_nod[node #][component]*/
	psf_s->norm_nod = (double **)malloc(psf_s->nnod_viz*sizeof(double *));
	for (i = 0; i < psf_s->nnod_viz; i++){
		psf_s->norm_nod[i] = (double *)calloc(3,sizeof(double));
	};
	
	return;
};

void alloc_psf_length_s(struct psf_data *psf_s){
	int i;
	
	/* allocate memory  dir_ele[patch #][component]*/
	psf_s->dir_ele = (double **)malloc(psf_s->nele_viz*sizeof(double *));
	for (i = 0; i < psf_s->nele_viz; i++){
		psf_s->dir_ele[i] = (double *)calloc(3,sizeof(double));
	};
	
	psf_s->length_ele = (double *)calloc(psf_s->nele_viz,sizeof(double));
	
	/* allocate memory  dir_nod[node #][component]*/
	psf_s->dir_nod = (double **)malloc(psf_s->nnod_viz*sizeof(double *));
	for (i = 0; i < psf_s->nnod_viz; i++){
		psf_s->dir_nod[i] = (double *)calloc(3,sizeof(double));
	};
	
	return;
};

void alloc_psf_cutting_4_map(struct psf_data *psf_s){
	int i;
	
	/* allocate memory  dir_ele[patch #][component]*/
	psf_s->inod_org_4_map_itp = (int **)malloc(psf_s->nnod_added_4_map*sizeof(int *));
	psf_s->coef_4_map_itp = (double **)malloc(psf_s->nnod_added_4_map*sizeof(int *));
	for (i = 0; i < psf_s->nnod_added_4_map; i++){
		psf_s->inod_org_4_map_itp[i] = (int *)calloc(2,sizeof(int));
		psf_s->coef_4_map_itp[i] = (double *)calloc(2,sizeof(double));
	};
	return;
};

static void dealloc_psf_norm_s(struct psf_data *psf_s){
	int i;
	/* deallocate memory*/
	for (i = 0; i < psf_s->nnod_viz; i++) free(psf_s->norm_nod[i]);
	free(psf_s->norm_nod);
	for (i = 0; i < psf_s->nele_viz; i++) free(psf_s->norm_ele[i]);
	free(psf_s->norm_ele);
	free(psf_s->area_viz);
	
	return;
};

static void dealloc_psf_length_s(struct psf_data *psf_s){
	int i;
	/* deallocate memory*/
	for (i = 0; i < psf_s->nnod_viz; i++) free(psf_s->dir_nod[i]);
	free(psf_s->dir_nod);
	for (i = 0; i < psf_s->nele_viz; i++) free(psf_s->dir_ele[i]);
	free(psf_s->dir_ele);
	free(psf_s->length_ele);
	
	return;
};

void dealloc_psf_field_data_c(struct psf_data *psf_s){
	int i;

	for (i = 0; i < psf_s->nnod_viz; i++) free(psf_s->d_nod[i]);
	free(psf_s->d_nod);
	
	free(psf_s->ncomp);
	free(psf_s->istack_comp);
	
	for (i = 0; i < psf_s->nfield; i++) free(psf_s->data_name[i]);
	free(psf_s->data_name);
	
	return;
};

void dealloc_psf_data_s(struct psf_data *psf_s){
	int i;
	/* deallocate memory*/
	free(psf_s->d_rms);
	free(psf_s->d_ave);
	free(psf_s->d_max);
	free(psf_s->d_min);
	free(psf_s->amp_max);
	free(psf_s->amp_min);
	
	for (i = 0; i < psf_s->nnod_viz; i++) free(psf_s->color_nod[i]);
	free(psf_s->color_nod);
	
	for (i = 0; i < psf_s->nnod_viz; i++) free(psf_s->d_amp[i]);
	free(psf_s->d_amp);
	
	free(psf_s->id_coord);
	
    dealloc_psf_field_data_c(psf_s);
		
	psf_s->ncomptot = 0;
	psf_s->nfield =   0;
	return;
};

void dealloc_psf_mesh_c(struct psf_data *psf_s){
	int i;
	for (i = 0; i < psf_s->nele_viz; i++) free(psf_s->x_ele_viz[i]);
	free(psf_s->x_ele_viz);
	
	for (i = 0; i < psf_s->nele_viz; i++) free(psf_s->ie_viz[i]);
	free(psf_s->ie_viz);
	free(psf_s->inod_viz);
	
	for (i = 0; i < psf_s->nnod_viz; i++) free(psf_s->xx_viz[i]);
	free(psf_s->xx_viz);
	
	return;
}

void dealloc_psf_cutting_4_map(struct psf_data *psf_s){
	int i;
	
	/* allocate memory  dir_ele[patch #][component]*/
	for (i = 0; i < psf_s->nnod_added_4_map; i++){
		free(psf_s->inod_org_4_map_itp[i]);
		free(psf_s->coef_4_map_itp[i]);
	};
	free(psf_s->inod_org_4_map_itp);
	free(psf_s->coef_4_map_itp);
	return;
};

void deallc_all_psf_data(struct psf_data *psf_s){
	dealloc_psf_cutting_4_map(psf_s);
	dealloc_psf_norm_s(psf_s);
	dealloc_psf_data_s(psf_s);
	dealloc_psf_mesh_c(psf_s);
	return;
};

void deallc_all_fline_data(struct psf_data *psf_s){
	dealloc_psf_norm_s(psf_s);
	dealloc_psf_length_s(psf_s);
	dealloc_psf_data_s(psf_s);
	dealloc_psf_mesh_c(psf_s);
	return;
};


struct vtk_field *alloc_vtk_fields_list_c(void){
	struct vtk_field *vtk_list = (struct vtk_field *)malloc(sizeof(struct vtk_field));
	return vtk_list;
}

void alloc_vtk_field_data_c(vtk_fields_t *vtk_s){
	int i;
	/* allocate memory  d_nod[node #][component]*/
	vtk_s->d_vtk = (double **)calloc(vtk_s->nnod_fld,sizeof(double *));
	for (i = 0; i < vtk_s->nnod_fld; i++){
		vtk_s->d_vtk[i] = (double *)calloc(vtk_s->ncomp_vtk,sizeof(double));
	};
};

void dealloc_vtk_field_data_c(vtk_fields_t *vtk_s){
	int i;
    
	for (i = 0; i < vtk_s->nnod_fld; i++) free(vtk_s->d_vtk[i]);
	free(vtk_s->d_vtk);
};

void dealloc_vtk_fields_list_c(struct vtk_field *vtk_list){
    vtk_fields_t *last_fld;
    vtk_fields_t *del_fld;
    int ifld;

    del_fld = vtk_list->vtk_fields;
    for (ifld=0; ifld<vtk_list->nfld_vtk; ifld++) {
        last_fld = del_fld->next_fld;
        dealloc_vtk_field_data_c(del_fld);
        free(del_fld);
        del_fld = last_fld;
    };
}


void copy_viewer_udt_node(struct psf_data *viz_copied, struct psf_data *viz_org){
	int i, j;
	
	for (i = 0; i < viz_org->nnod_viz; i++) {
		viz_copied->inod_viz[i] = viz_org->inod_viz[i];
		for(j = 0; j < 3; j++) viz_copied->xx_viz[i][j] = viz_org->xx_viz[i][j];
	};
	return;
}

void copy_viewer_udt_connect(struct psf_data *viz_copied, struct psf_data *viz_org){
	int i, j;
    
	for (i = 0; i < viz_org->nele_viz; i++) {
		for(j=0;j<viz_copied->nnod_4_ele_viz;j++){
			viz_copied->ie_viz[i][j] = viz_org->ie_viz[i][j];
		};
	};
	return;
}

void copy_viewer_udt_field_name(struct psf_data *viz_copied, struct psf_data *viz_org){
	int i, imin_fld;
	
    imin_fld = viz_org->nfield;
    if (viz_copied->nfield < imin_fld) imin_fld = viz_copied->nfield;
    
	viz_copied->istack_comp[0] = viz_org->istack_comp[0];
	for (i = 0; i < imin_fld; i++) {
		viz_copied->ncomp[i] = viz_org->ncomp[i];
		viz_copied->istack_comp[i+1] = viz_org->istack_comp[i+1];
		viz_copied->id_coord[i] = viz_org->id_coord[i];
		strngcopy(viz_copied->data_name[i], viz_org->data_name[i]);
	};
	viz_copied->ncomptot = viz_copied->istack_comp[imin_fld];
	
	return;
}

void copy_viewer_udt_data(struct psf_data *viz_copied, struct psf_data *viz_org){
	int i, j, imin_comp, imin_nod;
	
    imin_nod = viz_org->nnod_viz;
    imin_comp = viz_org->ncomptot;
    if (viz_copied->nnod_viz < imin_nod)  imin_nod =  viz_copied->nnod_viz;
    if (viz_copied->ncomptot < imin_comp) imin_comp = viz_copied->ncomptot;
    
	for (i = 0; i < imin_nod; i++) {
		for (j = 0; j < imin_comp; j++){
			viz_copied->d_nod[i][j] = viz_org->d_nod[i][j];
		};
	};
	return;
}

void copy_vtk_list_2_udt_name(struct psf_data *viz_copied, struct vtk_field *vtk_list){
    vtk_fields_t *last_fld;
    int i;
    
    last_fld = vtk_list->vtk_fields;
    for (i=0; i<viz_copied->nfield; i++) {
        strngcopy(viz_copied->data_name[i], last_fld->field_name);
        viz_copied->id_coord[i] = set_field_coordinate_flag(viz_copied->data_name[i]);
        viz_copied->ncomp[i] = last_fld->ncomp_vtk;
        viz_copied->istack_comp[i+1] = viz_copied->istack_comp[i] + viz_copied->ncomp[i];
        last_fld = last_fld->next_fld;
    }
    viz_copied->ncomptot = viz_copied->istack_comp[viz_copied->nfield];

	return;
}


void copy_vtk_list_2_udt_data(struct psf_data *viz_copied, struct vtk_field *vtk_list){
    vtk_fields_t *last_fld;
    int i, inod, nd, ist;
    
    last_fld = vtk_list->vtk_fields;
    for (i=0; i<viz_copied->nfield; i++) {
        ist = viz_copied->istack_comp[i];
        for (inod=0; inod<viz_copied->nnod_viz; inod++) {
            for (nd=0; nd<viz_copied->ncomp[i]; nd++) {
                viz_copied->d_nod[inod][nd+ist] = last_fld->d_vtk[inod][nd];
            };
        };
        last_fld = last_fld->next_fld;
    };
	return;
}

void check_psf_read(struct psf_data *psf_s){
	int i;
	
	printf("psf_s->nnod_viz %ld \n", psf_s->nnod_viz);
	printf("xx_1 %le %le %le \n", psf_s->xx_viz[0][0], psf_s->xx_viz[0][1], psf_s->xx_viz[0][2]);
	printf("xx_2 %le %le %le \n", psf_s->xx_viz[1][0], psf_s->xx_viz[1][1], psf_s->xx_viz[1][2]);
	printf("xx_3 %le %le %le \n", psf_s->xx_viz[2][0], psf_s->xx_viz[2][1], psf_s->xx_viz[2][2]);
	
	printf("xx_3 %le %le %le \n", psf_s->xx_viz[psf_s->nnod_viz-3][0],
		   psf_s->xx_viz[psf_s->nnod_viz-3][1], psf_s->xx_viz[psf_s->nnod_viz-3][2]);
	printf("xx_2 %le %le %le \n", psf_s->xx_viz[psf_s->nnod_viz-2][0],
		   psf_s->xx_viz[psf_s->nnod_viz-2][1], psf_s->xx_viz[psf_s->nnod_viz-2][2]);
	printf("xx_1 %le %le %le \n", psf_s->xx_viz[psf_s->nnod_viz-1][0],
		   psf_s->xx_viz[psf_s->nnod_viz-1][1], psf_s->xx_viz[psf_s->nnod_viz-1][2]);
	
	printf("psf_s->nnod_4_ele_viz %ld \n", psf_s->nnod_4_ele_viz);
	printf("psf_s->nele_viz %ld \n", psf_s->nele_viz);
	printf("ie_1 %ld %ld %ld \n", psf_s->ie_viz[0][0], psf_s->ie_viz[0][1], psf_s->ie_viz[0][2]);
	printf("ie_2 %ld %ld %ld \n", psf_s->ie_viz[1][0], psf_s->ie_viz[1][1], psf_s->ie_viz[1][2]);
	printf("ie_3 %ld %ld %ld \n", psf_s->ie_viz[2][0], psf_s->ie_viz[2][1], psf_s->ie_viz[2][2]);
	
	printf("ie_3 %ld %ld %ld \n", psf_s->ie_viz[psf_s->nele_viz-3][0], 
		   psf_s->ie_viz[psf_s->nele_viz-3][1], psf_s->ie_viz[psf_s->nele_viz-3][2]);
	printf("ie_2 %ld %ld %ld \n", psf_s->ie_viz[psf_s->nele_viz-2][0], 
		   psf_s->ie_viz[psf_s->nele_viz-2][1], psf_s->ie_viz[psf_s->nele_viz-2][2]);
	printf("ie_1 %ld %ld %ld \n", psf_s->ie_viz[psf_s->nele_viz-1][0], 
		   psf_s->ie_viz[psf_s->nele_viz-1][1], psf_s->ie_viz[psf_s->nele_viz-1][2]);
	
	printf("psf_s->nfield %ld \n", psf_s->nfield);
	printf("psf_s->ncomp ");
	for(i=0;i<psf_s->nfield;i++){printf("%ld ", psf_s->ncomp[i]);};
	printf("\n");
	printf("psf_s->data_name ");
	for(i=0;i<psf_s->nfield;i++){printf("%d %s \n", i, psf_s->data_name[i]);};
	printf("\n");
	printf("d_nod_1 %le \n", psf_s->d_nod[0][0]);
	printf("d_nod_2 %le \n", psf_s->d_nod[1][0]);
	printf("d_nod_3 %le \n", psf_s->d_nod[2][0]);
	
	printf("d_nod_3 %le \n", psf_s->d_nod[psf_s->nnod_viz-3][0]);
	printf("d_nod_2 %le \n", psf_s->d_nod[psf_s->nnod_viz-2][0]);
	printf("d_nod_1 %le \n", psf_s->d_nod[psf_s->nnod_viz-1][0]);
	
}

void compare_psf_data(struct psf_data *psf_s, struct psf_data *psf_z){
	int i, j;
	printf("Error xx_viz \n");
	for(j=0;j<3;j++){
		for(i=0;i<psf_s->nnod_viz;i++){
			if(psf_s->xx_viz[i][j] != psf_z->xx_viz[i][j]){
				printf("%d %d %le %le\n", j, i, psf_s->xx_viz[i][j], psf_z->xx_viz[i][j]);
			};
		};
	};
	
	printf("Error ie_viz \n");
	for(j=0;j<psf_s->nnod_4_ele_viz;j++){
		for(i=0;i<psf_s->nele_viz;i++){
			if(psf_s->ie_viz[i][j] != psf_z->ie_viz[i][j]){
				printf("%d %d %ld %ld\n", j, i, psf_s->ie_viz[i][j], psf_z->ie_viz[i][j]);
			};
		};
	};
	
	printf("Error d_nod \n");
	for(j=0;j<psf_s->ncomptot;j++){
		for(i=0;i<psf_s->nnod_viz;i++){
			if(psf_z->d_nod[i][j] != psf_z->d_nod[i][j]){
				printf("%d %d %le %le\n", j, i, psf_z->d_nod[i][j], psf_z->d_nod[i][j]);
			};
		};
	};
	printf("\n");
	return;
};
