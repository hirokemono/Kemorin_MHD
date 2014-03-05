
/*  m_psf_data_4_viewer_c.c */

#include <stdlib.h>
#include "m_psf_data_4_viewer_c.h"


void alloc_viz_node_s(struct psf_data *viz_s){
	int i;
	/* allocate memory  xx_viz[node #][direction]*/
	viz_s->xx_viz = (double **)calloc(viz_s->nnod_viz,sizeof(double *));
	for (i = 0; i < viz_s->nnod_viz; i++){
		viz_s->xx_viz[i] =  (double *)calloc(3,sizeof(double));
	};
	
	viz_s->inod_viz = (int *)calloc(viz_s->nnod_viz,sizeof(int));
	return;
};

void alloc_viz_ele_s(struct psf_data *viz_s){
	int i;
	
	/* allocate memory  ie_viz[patch #][connection]*/
	viz_s->ie_viz = (int **)calloc(viz_s->nele_viz,sizeof(int *));
	for (i = 0; i < viz_s->nele_viz; i++){
		viz_s->ie_viz[i] = (int *)calloc(viz_s->nnod_4_ele_viz,sizeof(int));
	};
	
	/* allocate memory  x_ele_viz[patch #][direction]*/
	viz_s->x_ele_viz = (double **)calloc(viz_s->nele_viz,sizeof(double *));
	for (i = 0; i < viz_s->nele_viz; i++){
		viz_s->x_ele_viz[i] = (double *)calloc(3,sizeof(double));
	};
	/* allocate memory  z_ele_viz[patch #]*/
	viz_s->z_ele_viz = (double *)calloc(viz_s->nele_viz,sizeof(double));
	/* allocate memory  iele_viz_far[patch #]*/
	viz_s->iele_viz_far = (int *)calloc(viz_s->nele_viz,sizeof(int));
	
	return;
};

void alloc_psf_field_name_c(struct psf_data *viz_s){
	int i;
	
	viz_s->ncomp =       (int *)calloc(viz_s->nfield,sizeof(int));
	viz_s->istack_comp = (int *)calloc(viz_s->nfield+1,sizeof(int));

	viz_s->id_coord =    (int *)calloc(viz_s->nfield,sizeof(int));
	
	viz_s->data_name = (char **)calloc(viz_s->nfield, sizeof(char *));
	for (i = 0; i < viz_s->nfield; i++) {
		viz_s->data_name[i] = (char *)calloc(UCD_LABEL_LEN, sizeof(char));
	};
};

void alloc_psf_field_data_c(struct psf_data *viz_s){
	int i;
	/* allocate memory  d_nod[node #][component]*/
	viz_s->d_nod = (double **)calloc(viz_s->nnod_viz,sizeof(double *));
	for (i = 0; i < viz_s->nnod_viz; i++){
		viz_s->d_nod[i] = (double *)calloc(viz_s->ncomptot,sizeof(double));
	};
};

void alloc_psf_data_s(struct psf_data *viz_s){
	int i;
	/* allocate memory  d_amp[node #][field]*/
	viz_s->d_amp = (double **)calloc(viz_s->nnod_viz,sizeof(double *));
	for (i = 0; i < viz_s->nnod_viz; i++){
		viz_s->d_amp[i] = (double *)calloc(viz_s->nfield,sizeof(double));
	};
	
	/* allocate memory  color_nod[node #][rgba code]*/
	viz_s->color_nod = (double **)calloc(viz_s->nnod_viz,sizeof(double *));
	for (i = 0; i < viz_s->nnod_viz; i++){
		viz_s->color_nod[i] = (double *)calloc(IFOUR,sizeof(double));
	};
	
	viz_s->d_min = (double *)calloc(viz_s->ncomptot,sizeof(double));
	viz_s->d_max = (double *)calloc(viz_s->ncomptot,sizeof(double));
	viz_s->d_ave = (double *)calloc(viz_s->ncomptot,sizeof(double));
	viz_s->d_rms = (double *)calloc(viz_s->ncomptot,sizeof(double));
	
	viz_s->amp_min = (double *)calloc(viz_s->nfield,sizeof(double));
	viz_s->amp_max = (double *)calloc(viz_s->nfield,sizeof(double));
	
	return;
};

void alloc_psf_norm_s(struct psf_data *viz_s){
	int i;
	
	/* allocate memory  norm_ele[patch #][component]*/
	viz_s->norm_ele = (double **)calloc(viz_s->nele_viz,sizeof(double *));
	for (i = 0; i < viz_s->nele_viz; i++){
		viz_s->norm_ele[i] = (double *)calloc(3,sizeof(double));
	};
	
	viz_s->area_viz = (double *)calloc(viz_s->nele_viz,sizeof(double));
	
	/* allocate memory  norm_nod[node #][component]*/
	viz_s->norm_nod = (double **)calloc(viz_s->nnod_viz,sizeof(double *));
	for (i = 0; i < viz_s->nnod_viz; i++){
		viz_s->norm_nod[i] = (double *)calloc(3,sizeof(double));
	};
	
	return;
};

void alloc_psf_length_s(struct psf_data *viz_s){
	int i;
	
	/* allocate memory  dir_ele[patch #][component]*/
	viz_s->dir_ele = (double **)calloc(viz_s->nele_viz,sizeof(double *));
	for (i = 0; i < viz_s->nele_viz; i++){
		viz_s->dir_ele[i] = (double *)calloc(3,sizeof(double));
	};
	
	viz_s->length_ele = (double *)calloc(viz_s->nele_viz,sizeof(double));
	
	/* allocate memory  dir_nod[node #][component]*/
	viz_s->dir_nod = (double **)calloc(viz_s->nnod_viz,sizeof(double *));
	for (i = 0; i < viz_s->nnod_viz; i++){
		viz_s->dir_nod[i] = (double *)calloc(3,sizeof(double));
	};
	
	return;
};

void alloc_psf_cutting_4_map(struct psf_data *viz_s){
	int i;
	
	/* allocate memory  dir_ele[patch #][component]*/
	viz_s->inod_org_4_map_itp = (int **)calloc(viz_s->nnod_added_4_map,sizeof(int *));
	viz_s->coef_4_map_itp = (double **)calloc(viz_s->nnod_added_4_map,sizeof(int *));
	for (i = 0; i < viz_s->nnod_added_4_map; i++){
		viz_s->inod_org_4_map_itp[i] = (int *)calloc(2,sizeof(int));
		viz_s->coef_4_map_itp[i] = (double *)calloc(2,sizeof(double));
	};
	return;
};

static void dealloc_psf_norm_s(struct psf_data *viz_s){
	int i;
	/* deallocate memory*/
	for (i = 0; i < viz_s->nnod_viz; i++) free(viz_s->norm_nod[i]);
	free(viz_s->norm_nod);
	for (i = 0; i < viz_s->nele_viz; i++) free(viz_s->norm_ele[i]);
	free(viz_s->norm_ele);
	free(viz_s->area_viz);
	
	return;
};

static void dealloc_psf_length_s(struct psf_data *viz_s){
	int i;
	/* deallocate memory*/
	for (i = 0; i < viz_s->nnod_viz; i++) free(viz_s->dir_nod[i]);
	free(viz_s->dir_nod);
	for (i = 0; i < viz_s->nele_viz; i++) free(viz_s->dir_ele[i]);
	free(viz_s->dir_ele);
	free(viz_s->length_ele);
	
	return;
};

void dealloc_psf_field_data_c(struct psf_data *viz_s){
	int i;

	for (i = 0; i < viz_s->nnod_viz; i++) free(viz_s->d_nod[i]);
	free(viz_s->d_nod);
	
	free(viz_s->ncomp);
	free(viz_s->istack_comp);
	
	for (i = 0; i < viz_s->nfield; i++) free(viz_s->data_name[i]);
	free(viz_s->data_name);
	
	return;
};

void dealloc_psf_data_s(struct psf_data *viz_s){
	int i;
	/* deallocate memory*/
	free(viz_s->d_rms);
	free(viz_s->d_ave);
	free(viz_s->d_max);
	free(viz_s->d_min);
	free(viz_s->amp_max);
	free(viz_s->amp_min);
	
	for (i = 0; i < viz_s->nnod_viz; i++) free(viz_s->color_nod[i]);
	free(viz_s->color_nod);
	
	for (i = 0; i < viz_s->nnod_viz; i++) free(viz_s->d_amp[i]);
	free(viz_s->d_amp);
	
	free(viz_s->id_coord);
	
    dealloc_psf_field_data_c(viz_s);
		
	viz_s->ncomptot = 0;
	viz_s->nfield =   0;
	return;
};

void dealloc_psf_mesh_c(struct psf_data *viz_s){
	int i;
	free(viz_s->iele_viz_far);
	free(viz_s->z_ele_viz);
	
	for (i = 0; i < viz_s->nele_viz; i++) free(viz_s->x_ele_viz[i]);
	free(viz_s->x_ele_viz);
	
	for (i = 0; i < viz_s->nele_viz; i++) free(viz_s->ie_viz[i]);
	free(viz_s->ie_viz);
	free(viz_s->inod_viz);
	
	for (i = 0; i < viz_s->nnod_viz; i++) free(viz_s->xx_viz[i]);
	free(viz_s->xx_viz);
	
	return;
}

void dealloc_psf_cutting_4_map(struct psf_data *viz_s){
	int i;
	
	/* allocate memory  dir_ele[patch #][component]*/
	for (i = 0; i < viz_s->nnod_added_4_map; i++){
		free(viz_s->inod_org_4_map_itp[i]);
		free(viz_s->coef_4_map_itp[i]);
	};
	free(viz_s->inod_org_4_map_itp);
	free(viz_s->coef_4_map_itp);
	return;
};

void deallc_all_psf_data(struct psf_data *viz_s){
	dealloc_psf_cutting_4_map(viz_s);
	dealloc_psf_norm_s(viz_s);
	dealloc_psf_data_s(viz_s);
	dealloc_psf_mesh_c(viz_s);
	return;
};

void deallc_all_fline_data(struct psf_data *viz_s){
	dealloc_psf_norm_s(viz_s);
	dealloc_psf_length_s(viz_s);
	dealloc_psf_data_s(viz_s);
	dealloc_psf_mesh_c(viz_s);
	return;
};


void alloc_vtk_fields_list_c(struct vtk_field *vtk_list){
	vtk_list = (struct vtk_field *)malloc(sizeof(struct vtk_field));
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