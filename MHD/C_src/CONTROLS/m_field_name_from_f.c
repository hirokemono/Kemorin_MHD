/*
//  m_field_name_from_f.c
//
//  Created by Hiroaki Matsui on 05/07/20.
//
*/

#include "m_field_name_from_f.h"

int lengthchara_f();

int count_MHD_field_groups_f();
int count_MHD_sym_field_groups_f();
int count_SGS_MHD_field_groups_f();

void MHD_field_groups_f(int *nfld_group_c, char *field_group_c);
void MHD_sym_field_groups_f(int *nfld_group_c, char *field_group_c);
void SGS_MHD_field_groups_f(int *nfld_group_c, char *field_group_c);

void set_base_field_names_f(int *ncomp1, char *name1, char *math1);
void set_base_force_labels_f(int *ncomp1, char *name1, char *math1);
void set_rot_force_labels_f(int *ncomp1, char *name1, char *math1);
void set_div_force_labels_f(int *ncomp1, char *name1, char *math1);
void set_energy_flux_names_f(int *ncomp1, char *name1, char *math1);
void set_divergence_field_labels_f(int *ncomp1, char *name1, char *math1);
void set_gradient_field_labels_f(int *ncomp1, char *name1, char *math1);
void set_base_diffusion_labels_f(int *ncomp1, char *name1, char *math1);
void set_base_diffusivity_labels_f(int *ncomp1, char *name1, char *math1);
void set_field_product_labels_f(int *ncomp1, char *name1, char *math1);
void set_work_4_explicit_labels_f(int *ncomp1, char *name1, char *math1);
void set_check_fields_labels_f(int *ncomp1, char *name1, char *math1);

void set_fields_w_sym_labels_f(int *ncomp1, char *name1, char *math1);
void set_forces_w_sym_labels_f(int *ncomp1, char *name1, char *math1);
void set_ene_flux_w_sym_labels_f(int *ncomp1, char *name1, char *math1);

void set_SGS_term_labels_f(int *ncomp1, char *name1, char *math1);
void set_diff_SGS_term_labels_f(int *ncomp1, char *name1, char *math1);
void set_SGS_energy_flux_labels_f(int *ncomp1, char *name1, char *math1);
void set_SGS_model_coefs_labels_f(int *ncomp1, char *name1, char *math1);
void set_dynamic_SGS_work_labels_f(int *ncomp1, char *name1, char *math1);

void set_filter_field_labels_f(int *ncomp1, char *name1, char *math1);
void set_div_filter_field_labels_f(int *ncomp1, char *name1, char *math1);
void set_grad_filter_field_labels_f(int *ncomp1, char *name1, char *math1);
void set_filtered_force_labels_f(int *ncomp1, char *name1, char *math1);
void rot_filtered_force_labels_f(int *ncomp1, char *name1, char *math1);
void div_filtered_force_labels_f(int *ncomp1, char *name1, char *math1);
void set_filtered_ene_flax_labels_f(int *ncomp1, char *name1, char *math1);
void set_wide_filter_field_labels_f(int *ncomp1, char *name1, char *math1);
void set_dbl_filter_field_labels_f(int *ncomp1, char *name1, char *math1);
void set_differnce_vector_labels_f(int *ncomp1, char *name1, char *math1);
void set_diff_filter_vect_labels_f(int *ncomp1, char *name1, char *math1);
void set_wide_SGS_term_labels_f(int *ncomp1, char *name1, char *math1);
void set_force_with_SGS_labels_f(int *ncomp1, char *name1, char *math1);
void set_true_SGS_term_labels_f(int *ncomp1, char *name1, char *math1);


int num_flag_scalar_comp_f();
int num_flag_vector_comp_f();
int num_flag_sym_tensor_comp_f();
int num_flag_asym_tensor_comp_f();

void set_flag_scalar_comp_f(int *ncomp1, char *name1, char *math1);
void set_flag_vector_comp_f(int *ncomp1, char *name1, char *math1);
void set_flag_sym_tensor_comp_f(int *ncomp1, char *name1, char *math1);
void set_flag_asym_tensor_comp_f(int *ncomp1, char *name1, char *math1);

void set_primary_componnet_flag_f(char *name1);


static void set_field_groups_from_f(struct field_names_f *fld_list){
	int i, len;
	char *packed_name;
	
	fld_list->len_f = lengthchara_f();
	
	fld_list->num_field_groups[0] = count_MHD_field_groups_f();
	fld_list->num_field_groups[1] = count_MHD_sym_field_groups_f();
	fld_list->num_field_groups[2] = count_SGS_MHD_field_groups_f();
	
	fld_list->istack_field_groups[0] = 0;
	for(i=0;i<3;i++){
		fld_list->istack_field_groups[i+1] = fld_list->istack_field_groups[i]
											 + fld_list->num_field_groups[i];
	};
	fld_list->ntot_field_groups = fld_list->istack_field_groups[3];
	
	if((fld_list->num_fields = (int *)calloc(fld_list->ntot_field_groups, sizeof(int))) == NULL) {
		printf("malloc error for num_fields\n");
		exit(0);
	}
	if((fld_list->istack_fields = (int *)calloc(fld_list->ntot_field_groups+1, sizeof(int))) == NULL) {
		printf("malloc error for istack_fields\n");
		exit(0);
	}
	
	int ntot_chara = fld_list->len_f * fld_list->ntot_field_groups;
	if((packed_name = (char *)calloc(ntot_chara, sizeof(char))) == NULL) {
		printf("malloc error for packed_name\n");
		exit(0);
	}
	
	MHD_field_groups_f(&fld_list->num_fields[fld_list->istack_field_groups[0]], 
								&packed_name[fld_list->len_f * fld_list->istack_field_groups[0]]);
	MHD_sym_field_groups_f(&fld_list->num_fields[fld_list->istack_field_groups[1]], 
								&packed_name[fld_list->len_f * fld_list->istack_field_groups[1]]);
	SGS_MHD_field_groups_f(&fld_list->num_fields[fld_list->istack_field_groups[2]], 
								&packed_name[fld_list->len_f * fld_list->istack_field_groups[2]]);
	
	if ((fld_list->field_group_name = (char **) malloc(fld_list->ntot_field_groups*sizeof(char *))) == NULL) {
		printf("malloc error for field_group_name\n");
		exit(0);
	}
	
	fld_list->istack_fields[0] = 0;
	for(i=0;i<fld_list->ntot_field_groups;i++){
		fld_list->istack_fields[i+1] = fld_list->istack_fields[i] + fld_list->num_fields[i];
		len = strlen(&packed_name[fld_list->len_f * i])+1;
		if((fld_list->field_group_name[i] = (char *)calloc(len, sizeof(char))) == NULL){
			printf("malloc error for field_group_name[%d]\n", i);
			exit(0);
		};
		
		strcpy(fld_list->field_group_name[i], &packed_name[fld_list->len_f * i]);
	};
	fld_list->ntot_fields = fld_list->istack_fields[fld_list->ntot_field_groups];
	free(packed_name);
	return;
};

static void set_field_names_from_f(struct field_names_f *fld_list){
	int i, ist, len;
	int ntot_chara = fld_list->len_f * fld_list->ntot_fields;
	
	char *packed_name;
	char *packed_math;
	if((packed_name = (char *)calloc(ntot_chara, sizeof(char))) == NULL) {
		printf("malloc error for packed_name\n");
		exit(0);
	}
	if((packed_math = (char *)calloc(ntot_chara, sizeof(char))) == NULL) {
		printf("malloc error for packed_math\n");
		exit(0);
	}
	
	if((fld_list->num_comp = (int *)calloc(fld_list->ntot_fields, sizeof(int))) == NULL) {
		printf("malloc error for num_comp\n");
		exit(0);
	}
	
	ist = fld_list->istack_fields[0];
	set_base_field_names_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[1];
	set_base_force_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[2];
	set_energy_flux_names_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[3];
	set_base_diffusion_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[4];
	set_rot_force_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[5];
	set_div_force_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[6];
	set_field_product_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[7];
	set_gradient_field_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[8];
	set_divergence_field_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[9];
	set_base_diffusivity_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[10];
	set_work_4_explicit_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[11];
	set_check_fields_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	
	ist = fld_list->istack_fields[12];
	set_fields_w_sym_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[13];
	set_forces_w_sym_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[14];
	set_ene_flux_w_sym_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	
	ist = fld_list->istack_fields[15];
	set_SGS_term_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[16];
	set_SGS_energy_flux_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[17];
	set_diff_SGS_term_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[18];
	set_SGS_model_coefs_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	
	ist = fld_list->istack_fields[19];
	set_filter_field_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[20];
	set_filtered_force_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[21];
	set_filtered_ene_flax_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[22];
	rot_filtered_force_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[23];
	div_filtered_force_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	
	ist = fld_list->istack_fields[24];
	set_grad_filter_field_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[25];
	set_div_filter_field_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	
	ist = fld_list->istack_fields[26];
	set_wide_filter_field_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[27];
	set_dbl_filter_field_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	
	ist = fld_list->istack_fields[28];
	set_differnce_vector_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[29];
	set_diff_filter_vect_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[30];
	set_wide_SGS_term_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[31];
	set_force_with_SGS_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[32];
	set_true_SGS_term_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	ist = fld_list->istack_fields[33];
	set_dynamic_SGS_work_labels_f(&fld_list->num_comp[ist], 
		&packed_name[fld_list->len_f * ist], &packed_math[fld_list->len_f * ist]);
	
	if ((fld_list->field_name = (char **) malloc(fld_list->ntot_fields*sizeof(char *))) == NULL) {
		printf("malloc error for field_name\n");
		exit(0);
	}
	if ((fld_list->field_math = (char **) malloc(fld_list->ntot_fields*sizeof(char *))) == NULL) {
		printf("malloc error for field_math\n");
		exit(0);
	}
	
	for(i=0;i<fld_list->ntot_fields;i++){
		len = strlen(&packed_name[fld_list->len_f * i])+1;
		if((fld_list->field_name[i]
				= (char *)calloc(len, sizeof(char))) == NULL){
			printf("malloc error for field_name[%d]\n", i);
			exit(0);
		};
		
		len = strlen(&packed_math[fld_list->len_f * i])+1;
		if((fld_list->field_math[i]
				= (char *)calloc(len, sizeof(char))) == NULL){
			printf("malloc error for field_math[%d]\n", i);
			exit(0);
		};
		
		strcpy(fld_list->field_name[i], &packed_name[fld_list->len_f * i]);
		strcpy(fld_list->field_math[i], &packed_math[fld_list->len_f * i]);
	}
	
	free(packed_name);
	free(packed_math);
	return;
};

struct field_names_f * init_field_name_f(){
	struct field_names_f *fld_list;
	if((fld_list = (struct field_names_f *) malloc(sizeof(struct field_names_f))) == NULL){
		printf("malloc error for field_names_f\n");
		exit(0);
	};
	fld_list->len_f = lengthchara_f();
	
	set_field_groups_from_f(fld_list);
	set_field_names_from_f(fld_list);
	return fld_list;
};

void dealloc_field_name_f(struct field_names_f *fld_list){
	int i;
	for(i=0;i<fld_list->ntot_fields;i++){
		free(fld_list->field_name[i]);
		free(fld_list->field_math[i]);
	}
	free(fld_list->field_name);
	free(fld_list->field_math);
	free(fld_list->num_comp);
	
	for(i=0;i<fld_list->ntot_field_groups;i++){
		free(fld_list->field_group_name[i]);
	}
	free(fld_list->field_group_name);
	free(fld_list->num_fields);
	free(fld_list->istack_fields);
	return;
}

void check_field_name_f(struct field_names_f *fld_list){
    int i, j;
    
    printf("ntot_field_groups %d %d \n", 
           fld_list->len_f, fld_list->ntot_field_groups);
    for(i=0;i<fld_list->ntot_field_groups;i++){
        printf("group: %d: %d: %d: %s\n", i, fld_list->num_fields[i],
               fld_list->istack_fields[i], fld_list->field_group_name[i]);
        for(j=fld_list->istack_fields[i];j<fld_list->istack_fields[i+1];j++){
            printf("name: %d: %d: %s: %s\n", j, fld_list->num_comp[j],
                   fld_list->field_name[j], fld_list->field_math[j]);
        };
        printf("\n");
    };
    printf("ntot_fields %d \n", fld_list->ntot_fields);
    printf("\n");
    return;
};



struct flag_with_math_f * init_scalar_components_flag(){
    struct flag_with_math_f *scalar_components_flag
    = init_flag_with_math_f(num_flag_scalar_comp_f, 
                            set_flag_scalar_comp_f);
    return scalar_components_flag;
};
struct flag_with_math_f * init_vector_components_flag(){
    struct flag_with_math_f *vector_components_flag
    = init_flag_with_math_f(num_flag_vector_comp_f, 
                            set_flag_vector_comp_f);
    return vector_components_flag;
};
struct flag_with_math_f * init_sym_tensor_components_flag(){
    struct flag_with_math_f *sym_tensor_components_flag
    = init_flag_with_math_f(num_flag_sym_tensor_comp_f, 
                            set_flag_sym_tensor_comp_f);
    return sym_tensor_components_flag;
};
struct flag_with_math_f * init_asym_tensor_components_flag(){
    struct flag_with_math_f *asym_tensor_components_flag
    = init_flag_with_math_f(num_flag_asym_tensor_comp_f, 
                            set_flag_asym_tensor_comp_f);
    return asym_tensor_components_flag;
};

struct component_flags_f * init_component_flags_f(){
    struct component_flags_f *comp_flags;
    if((comp_flags = (struct component_flags_f *) malloc(sizeof(struct component_flags_f))) == NULL){
        printf("malloc error for component_flags_f\n");
        exit(0);
    };
    
    comp_flags->scalar_components_flag =      init_scalar_components_flag();
    comp_flags->vector_components_flag =      init_vector_components_flag();
    comp_flags->sym_tensor_components_flag = init_sym_tensor_components_flag();
    comp_flags->asym_tensor_components_flag = init_asym_tensor_components_flag();
    return comp_flags;
};
void dealloc_component_flags_f(struct component_flags_f *comp_flags){
    dealloc_flag_with_math_f(comp_flags->scalar_components_flag);
    dealloc_flag_with_math_f(comp_flags->vector_components_flag);
    dealloc_flag_with_math_f(comp_flags->sym_tensor_components_flag);
    dealloc_flag_with_math_f(comp_flags->asym_tensor_components_flag);
    free(comp_flags);
    return;
};
void check_component_flags_f(struct component_flags_f *comp_flags){
    check_flag_with_math_f(comp_flags->scalar_components_flag);
    check_flag_with_math_f(comp_flags->vector_components_flag);
    check_flag_with_math_f(comp_flags->sym_tensor_components_flag);
    check_flag_with_math_f(comp_flags->asym_tensor_components_flag);
    return;
};
