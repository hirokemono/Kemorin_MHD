
/*  m_psf_data_4_viewer_c.c */

#include <stdlib.h>
#include "m_psf_data_4_viewer_c.h"


void alloc_viz_node_s(struct psf_data *psf_s){
	/* allocate memory  xyzw_viz[node #][direction]*/
	psf_s->xyzw_viz = (double *)malloc(IFOUR*psf_s->nnod_viz*sizeof(double));
    if(psf_s->xyzw_viz  == NULL){
        printf("malloc error for psf_s->xyzw_viz \n");
        exit(0);
    }
    
	psf_s->inod_viz = (long *)calloc(psf_s->nnod_viz,sizeof(long));
    if(psf_s->inod_viz  == NULL){
        printf("malloc error for psf_s->inod_viz \n");
        exit(0);
    }
	return;
};

void alloc_viz_ele_s(struct psf_data *psf_s){
	int i;
	
	/* allocate memory  ie_viz[patch #][connection]*/
	psf_s->ie_viz = (long **) malloc(psf_s->nele_viz*sizeof(long *));
    if(psf_s->ie_viz  == NULL){
        printf("malloc error for psf_s->ie_viz \n");
        exit(0);
    }

    for (i = 0; i < psf_s->nele_viz; i++){
		psf_s->ie_viz[i] = (long *)calloc(psf_s->nnod_4_ele_viz,sizeof(long));
        if(psf_s->ie_viz[i]  == NULL){
            printf("malloc error for psf_s->ie_viz[i], %d \n", i);
            exit(0);
        }
	};
	
	/* allocate memory  xyzw_ele_viz[patch #][direction]*/
	psf_s->xyzw_ele_viz = (double *) malloc(4*psf_s->nele_viz*sizeof(double));
    if(psf_s->xyzw_ele_viz  == NULL){
        printf("malloc error for psf_s->xyzw_ele_viz \n");
        exit(0);
    }
    return;
};

void alloc_psf_field_name_c(struct psf_data *psf_s){
	int i;
	
	psf_s->ncomp =       (long *)calloc(psf_s->nfield,sizeof(long));
	psf_s->istack_comp = (long *)calloc(psf_s->nfield+1,sizeof(long));

	psf_s->id_coord =    (int *)calloc(psf_s->nfield,sizeof(int));
	
	psf_s->data_name = (char **)malloc(psf_s->nfield*sizeof(char *));
    if(psf_s->data_name  == NULL){
        printf("malloc error for psf_s->data_name \n");
        exit(0);
    }

    for (i = 0; i < psf_s->nfield; i++) {
		psf_s->data_name[i] = (char *)calloc(KCHARA_C, sizeof(char));
        if(psf_s->data_name[i]  == NULL){
            printf("malloc error for psf_s->data_name[i], %d \n", i);
            exit(0);
        }
	};
};

void alloc_psf_field_data_c(struct psf_data *psf_s){
	/* allocate memory  d_nod[node #][component]*/
    long num = psf_s->ncomptot * psf_s->nnod_viz;
	psf_s->d_nod = (double *)malloc(num*sizeof(double));
    if(psf_s->d_nod  == NULL){
        printf("malloc error for psf_s->d_nod \n");
        exit(0);
    }
};

void alloc_psf_data_s(struct psf_data *psf_s){
    long num;
	/* allocate memory  d_amp[node #][field]*/
    num = psf_s->nfield * psf_s->nnod_viz;
	psf_s->d_amp = (double *)malloc(num*sizeof(double));
    if(psf_s->d_amp  == NULL){
        printf("malloc error for psf_s->d_amp \n");
        exit(0);
    }

    /* allocate memory  color_nod[node #][rgba code]*/
	psf_s->color_nod = (double *)malloc(IFOUR*psf_s->nnod_viz*sizeof(double));
    if(psf_s->color_nod  == NULL){
        printf("malloc error for psf_s->color_nod \n");
        exit(0);
    }

	psf_s->d_min = (double *)calloc(psf_s->ncomptot,sizeof(double));
    if(psf_s->d_min  == NULL){
        printf("malloc error for psf_s->d_min \n");
        exit(0);
    }

	psf_s->d_max = (double *)calloc(psf_s->ncomptot,sizeof(double));
    if(psf_s->d_max  == NULL){
        printf("malloc error for psf_s->d_max \n");
        exit(0);
    }

	psf_s->d_ave = (double *)calloc(psf_s->ncomptot,sizeof(double));
    if(psf_s->d_ave  == NULL){
        printf("malloc error for psf_s->d_ave \n");
        exit(0);
    }

	psf_s->d_rms = (double *)calloc(psf_s->ncomptot,sizeof(double));
    if(psf_s->d_rms  == NULL){
        printf("malloc error for psf_s->d_rms \n");
        exit(0);
    }


	psf_s->amp_min = (double *)calloc(psf_s->nfield,sizeof(double));
    if(psf_s->amp_min  == NULL){
        printf("malloc error for psf_s->amp_min \n");
        exit(0);
    }

	psf_s->amp_max = (double *)calloc(psf_s->nfield,sizeof(double));
    if(psf_s->amp_max  == NULL){
        printf("malloc error for psf_s->amp_max \n");
        exit(0);
    }


	return;
};

void alloc_psf_norm_s(struct psf_data *psf_s){
    psf_s->area_viz = (double *)calloc(psf_s->nele_viz,sizeof(double));
    if(psf_s->area_viz  == NULL){
        printf("malloc error for psf_s->area_viz \n");
        exit(0);
    }

	/* allocate memory  norm_ele[patch #][component]*/
	psf_s->norm_ele = (double *)malloc(4*psf_s->nele_viz*sizeof(double));
    if(psf_s->norm_ele  == NULL){
        printf("malloc error for psf_s->norm_ele \n");
        exit(0);
    }

	/* allocate memory  norm_nod[node #][component]*/
	psf_s->norm_nod = (double *)malloc(4*psf_s->nnod_viz*sizeof(double));
    if(psf_s->norm_nod  == NULL){
        printf("malloc error for psf_s->norm_nod \n");
        exit(0);
    }

	return;
};

void dealloc_psf_norm_s(struct psf_data *psf_s){
	free(psf_s->norm_nod);
	free(psf_s->norm_ele);
	free(psf_s->area_viz);
	return;
};

void dealloc_psf_field_data_c(struct psf_data *psf_s){
	free(psf_s->d_nod);
	free(psf_s->ncomp);
	free(psf_s->istack_comp);
    free(psf_s->id_coord);
	
	for(int i = 0; i < psf_s->nfield; i++) free(psf_s->data_name[i]);
	free(psf_s->data_name);
	
	return;
};

void dealloc_psf_data_s(struct psf_data *psf_s){
	/* deallocate memory*/
	free(psf_s->d_rms);
	free(psf_s->d_ave);
	free(psf_s->d_max);
	free(psf_s->d_min);
	free(psf_s->amp_max);
	free(psf_s->amp_min);
	
	free(psf_s->color_nod);
	free(psf_s->d_amp);
		
    dealloc_psf_field_data_c(psf_s);
		
	psf_s->ncomptot = 0;
	psf_s->nfield =   0;
	return;
};

void dealloc_psf_mesh_c(struct psf_data *psf_s){
	int i;
	free(psf_s->xyzw_ele_viz);
	
	for (i = 0; i < psf_s->nele_viz; i++) free(psf_s->ie_viz[i]);
	free(psf_s->ie_viz);
	free(psf_s->inod_viz);
	free(psf_s->xyzw_viz);
	return;
}

void deallc_all_psf_data(struct psf_data *psf_s){
    /*
    dealloc_edge_data_4_psf(psf_s->nele_viz, psf_s->psf_edge);
    */
	dealloc_psf_norm_s(psf_s);
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


void copy_viewer_udt_node(struct psf_data *viz_org, long *inod_copied, double *xyzw_copied){
	int i, j;
	
	for (i = 0; i < viz_org->nnod_viz; i++) {
        inod_copied[i] = viz_org->inod_viz[i];
        for(j = 0; j < 4; j++){
            xyzw_copied[i*IFOUR + j] = viz_org->xyzw_viz[i*IFOUR + j];
        };
	};
	return;
}

void copy_viewer_udt_connect(struct psf_data *viz_org, long **ie_copied){
	int i, j;
    
	for (i = 0; i < viz_org->nele_viz; i++) {
		for(j=0;j<viz_org->nnod_4_ele_viz;j++){
            ie_copied[i][j] = viz_org->ie_viz[i][j];
		};
	};
	return;
}

long copy_viewer_udt_field_name(struct psf_data *viz_org, long nfield,
                                long *ncomp, long *istack_comp,
                                int *id_coord, char **data_name){
	long i, imin_fld;
	
    imin_fld = viz_org->nfield;
    if (nfield < imin_fld) imin_fld = nfield;
    
	istack_comp[0] = viz_org->istack_comp[0];
	for (i = 0; i < imin_fld; i++) {
		ncomp[i] = viz_org->ncomp[i];
		istack_comp[i+1] = viz_org->istack_comp[i+1];
		id_coord[i] = viz_org->id_coord[i];
		strngcopy(data_name[i], viz_org->data_name[i]);
	};	
    return istack_comp[imin_fld];
}

void copy_viewer_udt_data(struct psf_data *viz_org,
                          long nnod_copied, long ncomptot_copied,
                          double *d_copied){
    long i, j, imin_comp, imin_nod;
	
    imin_nod = viz_org->nnod_viz;
    imin_comp = viz_org->ncomptot;
    if (nnod_copied < imin_nod)  imin_nod =  nnod_copied;
    if (ncomptot_copied < imin_comp) imin_comp = ncomptot_copied;
    
	for (i = 0; i < imin_nod; i++) {
		for (j = 0; j < imin_comp; j++){
            d_copied[i*ncomptot_copied + j]
                = viz_org->d_nod[i*viz_org->ncomptot + j];
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
        ist = (int) viz_copied->istack_comp[i];
        for (inod=0; inod<viz_copied->nnod_viz; inod++) {
            for (nd=0; nd<viz_copied->ncomp[i]; nd++) {
                viz_copied->d_nod[inod*viz_copied->ncomptot  + (nd+ist)] = last_fld->d_vtk[inod][nd];
            };
        };
        last_fld = last_fld->next_fld;
    };
	return;
}

void check_psf_read(struct psf_data *psf_s){
	int i;
	
	printf("psf_s->nnod_viz %ld \n", psf_s->nnod_viz);
	printf("xx_1 %le %le %le \n", psf_s->xyzw_viz[0], psf_s->xyzw_viz[1], psf_s->xyzw_viz[ 2]);
	printf("xx_2 %le %le %le \n", psf_s->xyzw_viz[4], psf_s->xyzw_viz[5], psf_s->xyzw_viz[ 6]);
	printf("xx_3 %le %le %le \n", psf_s->xyzw_viz[8], psf_s->xyzw_viz[9], psf_s->xyzw_viz[10]);
	
	printf("xx_3 %le %le %le \n",
           psf_s->xyzw_viz[(psf_s->nnod_viz-3)*IFOUR + 0],
		   psf_s->xyzw_viz[(psf_s->nnod_viz-3)*IFOUR + 1],
           psf_s->xyzw_viz[(psf_s->nnod_viz-3)*IFOUR + 2]);
	printf("xx_2 %le %le %le \n",
           psf_s->xyzw_viz[(psf_s->nnod_viz-2)*IFOUR + 0],
		   psf_s->xyzw_viz[(psf_s->nnod_viz-2)*IFOUR + 1],
           psf_s->xyzw_viz[(psf_s->nnod_viz-2)*IFOUR + 2]);
	printf("xx_1 %le %le %le \n",
           psf_s->xyzw_viz[(psf_s->nnod_viz-1)*IFOUR + 0],
		   psf_s->xyzw_viz[(psf_s->nnod_viz-1)*IFOUR + 1],
           psf_s->xyzw_viz[(psf_s->nnod_viz-1)*IFOUR + 2]);
	
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
	printf("d_nod_1 %le \n", psf_s->d_nod[0]);
	printf("d_nod_2 %le \n", psf_s->d_nod[  psf_s->ncomptot]);
	printf("d_nod_3 %le \n", psf_s->d_nod[2*psf_s->ncomptot]);
	
	printf("d_nod_3 %le \n", psf_s->d_nod[(psf_s->nnod_viz-3)*psf_s->ncomptot]);
	printf("d_nod_2 %le \n", psf_s->d_nod[(psf_s->nnod_viz-2)*psf_s->ncomptot]);
	printf("d_nod_1 %le \n", psf_s->d_nod[(psf_s->nnod_viz-1)*psf_s->ncomptot]);
	
}

void compare_psf_data(struct psf_data *psf_s, struct psf_data *psf_z){
	int i, j;
	printf("Error xyzw_viz \n");
	for(j=0;j<3;j++){
		for(i=0;i<psf_s->nnod_viz;i++){
			if(psf_s->xyzw_viz[i*IFOUR + j] != psf_z->xyzw_viz[i*IFOUR + j]){
				printf("%d %d %le %le\n", j, i,
                       psf_s->xyzw_viz[i*IFOUR + j],
                       psf_z->xyzw_viz[i*IFOUR + j]);
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
			if(psf_z->d_nod[i*psf_s->ncomptot + j]
                != psf_z->d_nod[i*psf_s->ncomptot + j]){
				printf("%d %d %le %le\n", j, i,
                       psf_z->d_nod[i*psf_s->ncomptot + j],
                       psf_z->d_nod[i*psf_s->ncomptot + j]);
			};
		};
	};
	printf("\n");
	return;
};
