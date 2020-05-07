/*
//  m_field_name_from_f.c
//
//  Created by Hiroaki Matsui on 05/07/20.
//
*/

#include "m_field_name_from_f.h"

int lengthchara_f();

int num_base_fields_f();
int num_base_forces_f();
int num_rot_forces_f();
int num_div_forces_f();
int num_energy_fluxes_f();
int num_divergence_fields_f();
int num_gradient_fields_f();
int num_base_diffusions_f();
int num_base_diffusivities_f();
int num_field_products_f();
int num_work_4_explicit_f();
int num_check_fields_f();

int num_SGS_terms_f();
int num_diff_SGS_terms_f();
int num_SGS_energy_fluxes_f();
int num_SGS_model_coefs_f();
int num_dynamic_SGS_work_f();
int num_dynamic_SGS_work_f();

int num_filter_fields_f();
int num_div_filter_fields_f();
int num_grad_filter_fields_f();
int num_filtered_forces_f();
int num_rot_filtered_forces_f();
int num_div_filtered_forces_f();
int num_filtered_ene_fluxes_f();
int num_wide_filter_fields_f();
int num_double_filter_fields_f();
int num_difference_vector_f();
int num_diff_filter_vector_f();
int num_wide_SGS_terms_f();
int num_force_w_SGS_f();
int num_true_SGS_terms_f();

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

struct field_names_f * init_field_name_f(){
	int i;
	char *packed_name;
	char *packed_math;
	
	struct field_names_f *fnames;
	if((fnames = (struct field_names_f *) malloc(sizeof(struct field_names_f))) == NULL){
		printf("malloc error for field_names_f\n");
		exit(0);
	};
	
	fnames->len_f = lengthchara_f();
	
	fnames->istack_base_fields[0] = 0;
	fnames->istack_base_fields[1] = fnames->istack_base_fields[0] + num_base_fields_f();
	
	fnames->istack_base_forces[0] = fnames->istack_base_fields[1];
	fnames->istack_base_forces[1] = fnames->istack_base_forces[0] + num_base_forces_f();
	
	fnames->istack_rot_forces[0] = fnames->istack_base_forces[1];
	fnames->istack_rot_forces[1] = fnames->istack_rot_forces[0] + num_rot_forces_f();
	
	fnames->istack_div_forces[0] = fnames->istack_rot_forces[1];
	fnames->istack_div_forces[1] = fnames->istack_div_forces[0] + num_div_forces_f();
	
	fnames->istack_energy_fluxes[0] = fnames->istack_div_forces[1];
	fnames->istack_energy_fluxes[1] = fnames->istack_energy_fluxes[0] + num_energy_fluxes_f();
	
	fnames->istack_divergence_fields[0] = fnames->istack_energy_fluxes[1];
	fnames->istack_divergence_fields[1] = fnames->istack_divergence_fields[0]
			+ num_divergence_fields_f();
	
	fnames->istack_gradient_fields[0] = fnames->istack_divergence_fields[1];
	fnames->istack_gradient_fields[1] = fnames->istack_gradient_fields[0]
			+ num_gradient_fields_f();
	
	fnames->istack_base_diffusions[0] = fnames->istack_gradient_fields[1];
	fnames->istack_base_diffusions[1] = fnames->istack_base_diffusions[0]
			+ num_base_diffusions_f();
	
	fnames->istack_base_diffusivities[0] = fnames->istack_base_diffusions[1];
	fnames->istack_base_diffusivities[1] = fnames->istack_base_diffusivities[0]
			+ num_base_diffusivities_f();
	
	fnames->istack_field_products[0] = fnames->istack_base_diffusivities[1];
	fnames->istack_field_products[1] = fnames->istack_field_products[0]
			+ num_field_products_f();
	
	fnames->istack_work_4_explicit[0] = fnames->istack_field_products[1];
	fnames->istack_work_4_explicit[1] = fnames->istack_work_4_explicit[0]
			+ num_work_4_explicit_f();
	
	fnames->istack_check_fields[0] = fnames->istack_work_4_explicit[1];
	fnames->istack_check_fields[1] = fnames->istack_check_fields[0]
			+ num_check_fields_f();
	
	
	fnames->istack_filtered_forces[0] = fnames->istack_check_fields[1];
	fnames->istack_filtered_forces[1] = fnames->istack_filtered_forces[0]
			+ num_filtered_forces_f();
	
	fnames->istack_rot_filtered_forces[0] = fnames->istack_filtered_forces[1];
	fnames->istack_rot_filtered_forces[1] = fnames->istack_rot_filtered_forces[0]
			+ num_rot_filtered_forces_f();
	
	fnames->istack_div_filtered_forces[0] = fnames->istack_rot_filtered_forces[1];
	fnames->istack_div_filtered_forces[1] = fnames->istack_div_filtered_forces[0]
			+ num_div_filtered_forces_f();
	
	fnames->istack_filtered_ene_fluxes[0] = fnames->istack_div_filtered_forces[1];
	fnames->istack_filtered_ene_fluxes[1] = fnames->istack_filtered_ene_fluxes[0]
			+ num_filtered_ene_fluxes_f();
	
	
	fnames->istack_SGS_terms[0] = fnames->istack_filtered_ene_fluxes[1];
	fnames->istack_SGS_terms[1] = fnames->istack_SGS_terms[0]
			+ num_SGS_terms_f();
	
	fnames->istack_diff_SGS_terms[0] = fnames->istack_SGS_terms[1];
	fnames->istack_diff_SGS_terms[1] = fnames->istack_diff_SGS_terms[0]
			+ num_diff_SGS_terms_f();
	
	fnames->istack_SGS_energy_fluxes[0] = fnames->istack_diff_SGS_terms[1];
	fnames->istack_SGS_energy_fluxes[1] = fnames->istack_SGS_energy_fluxes[0]
			+ num_SGS_energy_fluxes_f();
	
	fnames->istack_SGS_model_coefs[0] = fnames->istack_SGS_energy_fluxes[1];
	fnames->istack_SGS_model_coefs[1] = fnames->istack_SGS_model_coefs[0]
			+ num_SGS_model_coefs_f();
	
	fnames->istack_filter_fields[0] = fnames->istack_SGS_model_coefs[1];
	fnames->istack_filter_fields[1] = fnames->istack_filter_fields[0]
			+ num_filter_fields_f();
	
	fnames->istack_div_filter_fields[0] = fnames->istack_filter_fields[1];
	fnames->istack_div_filter_fields[1] = fnames->istack_div_filter_fields[0]
			+ num_div_filter_fields_f();
	
	fnames->istack_grad_filter_fields[0] = fnames->istack_div_filter_fields[1];
	fnames->istack_grad_filter_fields[1] = fnames->istack_grad_filter_fields[0]
			+ num_grad_filter_fields_f();
	
	
	fnames->istack_wide_filter_fields[0] = fnames->istack_grad_filter_fields[1];
	fnames->istack_wide_filter_fields[1] = fnames->istack_wide_filter_fields[0]
			+ num_wide_filter_fields_f();
	
	fnames->istack_double_filter_fields[0] = fnames->istack_wide_filter_fields[1];
	fnames->istack_double_filter_fields[1] = fnames->istack_double_filter_fields[0]
			+ num_double_filter_fields_f();
	
	fnames->istack_difference_vector[0] = fnames->istack_double_filter_fields[1];
	fnames->istack_difference_vector[1] = fnames->istack_difference_vector[0]
			+ num_difference_vector_f();
	
	fnames->istack_diff_filter_vector[0] = fnames->istack_difference_vector[1];
	fnames->istack_diff_filter_vector[1] = fnames->istack_diff_filter_vector[0]
			+ num_diff_filter_vector_f();
	
	fnames->istack_wide_SGS_terms[0] = fnames->istack_diff_filter_vector[1];
	fnames->istack_wide_SGS_terms[1] = fnames->istack_wide_SGS_terms[0]
			+ num_wide_SGS_terms_f();
	
	fnames->istack_dynamic_SGS_work[0] = fnames->istack_wide_SGS_terms[1];
	fnames->istack_dynamic_SGS_work[1] = fnames->istack_dynamic_SGS_work[0]
			+ num_dynamic_SGS_work_f();
	
	fnames->istack_force_w_SGS[0] = fnames->istack_dynamic_SGS_work[1];
	fnames->istack_force_w_SGS[1] = fnames->istack_force_w_SGS[0]
			+ num_force_w_SGS_f();
	
	fnames->istack_true_SGS_terms[0] = fnames->istack_force_w_SGS[1];
	fnames->istack_true_SGS_terms[1] = fnames->istack_true_SGS_terms[0]
			+ num_true_SGS_terms_f();
	
	fnames->ntot_fields = fnames->istack_true_SGS_terms[1];
	
	if((fnames->num_comp = (int *)calloc(fnames->ntot_fields, sizeof(int))) == NULL) {
		printf("malloc error for num_comp\n");
		exit(0);
	}
	
	int ntot_chara = fnames->len_f * fnames->ntot_fields;
	if((packed_name = (char *)calloc(ntot_chara, sizeof(char))) == NULL) {
		printf("malloc error for packed_name\n");
		exit(0);
	}
	if((packed_math = (char *)calloc(ntot_chara, sizeof(char))) == NULL) {
		printf("malloc error for packed_math\n");
		exit(0);
	}
	
	set_base_field_names_f(&fnames->num_comp[fnames->istack_base_fields[0]], 
								&packed_name[fnames->len_f * fnames->istack_base_fields[0]],
								&packed_math[fnames->len_f * fnames->istack_base_fields[0]]);
	set_base_force_labels_f(&fnames->num_comp[fnames->istack_base_forces[0]], 
								&packed_name[fnames->len_f * fnames->istack_base_forces[0]],
								&packed_math[fnames->len_f * fnames->istack_base_forces[0]]);
	set_rot_force_labels_f(&fnames->num_comp[fnames->istack_rot_forces[0]], 
								&packed_name[fnames->len_f * fnames->istack_rot_forces[0]],
								&packed_math[fnames->len_f * fnames->istack_rot_forces[0]]);
	set_div_force_labels_f(&fnames->num_comp[fnames->istack_div_forces[0]], 
								&packed_name[fnames->len_f * fnames->istack_div_forces[0]],
								&packed_math[fnames->len_f * fnames->istack_div_forces[0]]);
	set_energy_flux_names_f(&fnames->num_comp[fnames->istack_energy_fluxes[0]], 
								&packed_name[fnames->len_f * fnames->istack_energy_fluxes[0]],
								&packed_math[fnames->len_f * fnames->istack_energy_fluxes[0]]);
	set_divergence_field_labels_f(&fnames->num_comp[fnames->istack_divergence_fields[0]], 
								&packed_name[fnames->len_f * fnames->istack_divergence_fields[0]],
								&packed_math[fnames->len_f * fnames->istack_divergence_fields[0]]);
	set_gradient_field_labels_f(&fnames->num_comp[fnames->istack_gradient_fields[0]], 
								&packed_name[fnames->len_f * fnames->istack_gradient_fields[0]],
								&packed_math[fnames->len_f * fnames->istack_gradient_fields[0]]);
	set_base_diffusion_labels_f(&fnames->num_comp[fnames->istack_base_diffusions[0]], 
								&packed_name[fnames->len_f * fnames->istack_base_diffusions[0]],
								&packed_math[fnames->len_f * fnames->istack_base_diffusions[0]]);
	set_base_diffusivity_labels_f(&fnames->num_comp[fnames->istack_base_diffusivities[0]], 
								&packed_name[fnames->len_f * fnames->istack_base_diffusivities[0]],
								&packed_math[fnames->len_f * fnames->istack_base_diffusivities[0]]);
	set_field_product_labels_f(&fnames->num_comp[fnames->istack_field_products[0]], 
								&packed_name[fnames->len_f * fnames->istack_field_products[0]],
								&packed_math[fnames->len_f * fnames->istack_field_products[0]]);
	set_work_4_explicit_labels_f(&fnames->num_comp[fnames->istack_work_4_explicit[0]], 
								&packed_name[fnames->len_f * fnames->istack_work_4_explicit[0]],
								&packed_math[fnames->len_f * fnames->istack_work_4_explicit[0]]);
	set_check_fields_labels_f(&fnames->num_comp[fnames->istack_check_fields[0]], 
								&packed_name[fnames->len_f * fnames->istack_check_fields[0]],
								&packed_math[fnames->len_f * fnames->istack_check_fields[0]]);
	
	set_SGS_term_labels_f(&fnames->num_comp[fnames->istack_SGS_terms[0]], 
								&packed_name[fnames->len_f * fnames->istack_SGS_terms[0]],
								&packed_math[fnames->len_f * fnames->istack_SGS_terms[0]]);
	set_diff_SGS_term_labels_f(&fnames->num_comp[fnames->istack_diff_SGS_terms[0]], 
								&packed_name[fnames->len_f * fnames->istack_diff_SGS_terms[0]],
								&packed_math[fnames->len_f * fnames->istack_diff_SGS_terms[0]]);
	set_SGS_energy_flux_labels_f(&fnames->num_comp[fnames->istack_SGS_energy_fluxes[0]], 
								&packed_name[fnames->len_f * fnames->istack_SGS_energy_fluxes[0]],
								&packed_math[fnames->len_f * fnames->istack_SGS_energy_fluxes[0]]);
	set_SGS_model_coefs_labels_f(&fnames->num_comp[fnames->istack_SGS_model_coefs[0]], 
								&packed_name[fnames->len_f * fnames->istack_SGS_model_coefs[0]],
								&packed_math[fnames->len_f * fnames->istack_SGS_model_coefs[0]]);
	
	set_filtered_force_labels_f(&fnames->num_comp[fnames->istack_filtered_forces[0]], 
								&packed_name[fnames->len_f * fnames->istack_filtered_forces[0]],
								&packed_math[fnames->len_f * fnames->istack_filtered_forces[0]]);
	rot_filtered_force_labels_f(&fnames->num_comp[fnames->istack_rot_filtered_forces[0]], 
								&packed_name[fnames->len_f * fnames->istack_rot_filtered_forces[0]],
								&packed_math[fnames->len_f * fnames->istack_rot_filtered_forces[0]]);
	div_filtered_force_labels_f(&fnames->num_comp[fnames->istack_div_filtered_forces[0]], 
								&packed_name[fnames->len_f * fnames->istack_div_filtered_forces[0]], 
								&packed_math[fnames->len_f * fnames->istack_div_filtered_forces[0]]);
	set_filtered_ene_flax_labels_f(&fnames->num_comp[fnames->istack_filtered_ene_fluxes[0]], 
								&packed_name[fnames->len_f * fnames->istack_filtered_ene_fluxes[0]], 
								&packed_math[fnames->len_f * fnames->istack_filtered_ene_fluxes[0]]);
	
	set_filter_field_labels_f(&fnames->num_comp[fnames->istack_filter_fields[0]], 
								&packed_name[fnames->len_f * fnames->istack_filter_fields[0]],
								&packed_math[fnames->len_f * fnames->istack_filter_fields[0]]);
	set_div_filter_field_labels_f(&fnames->num_comp[fnames->istack_div_filter_fields[0]], 
								&packed_name[fnames->len_f * fnames->istack_div_filter_fields[0]],
								&packed_math[fnames->len_f * fnames->istack_div_filter_fields[0]]);
	set_grad_filter_field_labels_f(&fnames->num_comp[fnames->istack_grad_filter_fields[0]], 
								&packed_name[fnames->len_f * fnames->istack_grad_filter_fields[0]],
								&packed_math[fnames->len_f * fnames->istack_grad_filter_fields[0]]);
	
	set_wide_filter_field_labels_f(&fnames->num_comp[fnames->istack_wide_filter_fields[0]], 
								&packed_name[fnames->len_f * fnames->istack_wide_filter_fields[0]], 
								&packed_math[fnames->len_f * fnames->istack_wide_filter_fields[0]]);
	set_dbl_filter_field_labels_f(&fnames->num_comp[fnames->istack_double_filter_fields[0]], 
								&packed_name[fnames->len_f * fnames->istack_double_filter_fields[0]], 
								&packed_math[fnames->len_f * fnames->istack_double_filter_fields[0]]);
	
	set_differnce_vector_labels_f(&fnames->num_comp[fnames->istack_difference_vector[0]], 
								&packed_name[fnames->len_f * fnames->istack_difference_vector[0]], 
								&packed_math[fnames->len_f * fnames->istack_difference_vector[0]]);
	set_diff_filter_vect_labels_f(&fnames->num_comp[fnames->istack_diff_filter_vector[0]], 
								&packed_name[fnames->len_f * fnames->istack_diff_filter_vector[0]], 
								&packed_math[fnames->len_f * fnames->istack_diff_filter_vector[0]]);
	set_wide_SGS_term_labels_f(&fnames->num_comp[fnames->istack_wide_SGS_terms[0]], 
								&packed_name[fnames->len_f * fnames->istack_wide_SGS_terms[0]], 
								&packed_math[fnames->len_f * fnames->istack_wide_SGS_terms[0]]);
	set_dynamic_SGS_work_labels_f(&fnames->num_comp[fnames->istack_dynamic_SGS_work[0]], 
								&packed_name[fnames->len_f * fnames->istack_dynamic_SGS_work[0]], 
								&packed_math[fnames->len_f * fnames->istack_dynamic_SGS_work[0]]);
	set_force_with_SGS_labels_f(&fnames->num_comp[fnames->istack_force_w_SGS[0]], 
								&packed_name[fnames->len_f * fnames->istack_force_w_SGS[0]], 
								&packed_math[fnames->len_f * fnames->istack_force_w_SGS[0]]);
	set_true_SGS_term_labels_f(&fnames->num_comp[fnames->istack_true_SGS_terms[0]], 
								&packed_name[fnames->len_f * fnames->istack_true_SGS_terms[0]], 
								&packed_math[fnames->len_f * fnames->istack_true_SGS_terms[0]]);
	
	
	if ((fnames->field_name = (char **) malloc(fnames->ntot_fields*sizeof(char *))) == NULL) {
		printf("malloc error for field_name\n");
		exit(0);
	}
	if ((fnames->field_math = (char **) malloc(fnames->ntot_fields*sizeof(char *))) == NULL) {
		printf("malloc error for field_math\n");
		exit(0);
	}
	
	int len;
	for(i=0;i<fnames->ntot_fields;i++){
		len = strlen(&packed_name[fnames->len_f * i])+1;
		if((fnames->field_name[i]
				= (char *)calloc(len, sizeof(char))) == NULL){
			printf("malloc error for field_name[%d]\n", i);
			exit(0);
		};
		
		len = strlen(&packed_math[fnames->len_f * i])+1;
		if((fnames->field_math[i]
				= (char *)calloc(len, sizeof(char))) == NULL){
			printf("malloc error for field_math[%d]\n", i);
			exit(0);
		};
		
		strcpy(fnames->field_name[i], &packed_name[fnames->len_f * i]);
		strcpy(fnames->field_math[i], &packed_math[fnames->len_f * i]);
	}
	printf("\n");
	
	free(packed_name);
	free(packed_math);
	
	return fnames;
};

void dealloc_field_name_f(struct field_names_f *fnames){
	int i;
	for(i=0;i<fnames->ntot_fields;i++){
		free(fnames->field_name[i]);
		free(fnames->field_math[i]);
	}
	free(fnames->field_name);
	free(fnames->field_math);
	free(fnames->num_comp);
	return;
}
