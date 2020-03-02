#include <stdlib.h>
#include <string.h>
#include <stdio.h>

int lengthchara_f();

int num_base_forces_f();
int num_divergence_fields_f();
int num_gradient_fields_f();

int num_SGS_terms_f();
int num_diff_SGS_terms_f();
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

void set_base_force_labels_f(int *ncomp1, char *name1, char *math1);
void set_divergence_field_labels_f(int *ncomp1, char *name1, char *math1);
void set_gradient_field_labels_f(int *ncomp1, char *name1, char *math1);

void set_div_filter_field_labels_f(int *ncomp1, char *name1, char *math1);
void set_grad_filter_field_labels_f(int *ncomp1, char *name1, char *math1);

void set_SGS_term_labels_f(int *ncomp1, char *name1, char *math1);
void set_diff_SGS_term_labels_f(int *ncomp1, char *name1, char *math1);

void set_filtered_force_labels_f(int *ncomp1, char *name1, char *math1);
void rot_filtered_force_labels_f(int *ncomp1, char *name1, char *math1);
void div_filtered_force_labels_f(int *ncomp1, char *name1, char *math1);
void set_filtered_ene_flax_labels_f(int *ncomp1, char *name1, char *math1);
void set_wide_filter_field_labels_f(int *ncomp1, char *name1, char *math1);
void set_dbl_filter_field_labels_f(int *ncomp1, char *name1, char *math1);
void set_differnce_vector_labels_f(int *ncomp1, char *name1, char *math1);
void set_diff_filter_vect_labels_f(int *ncomp1, char *name1, char *math1);

int main(int argc, char **argv)
{
	int i;
	char **names;
	char **maths;
	
	int len_f = lengthchara_f();
	
	int ist_base_forces = 0;
	int ist_divergence_fields = ist_base_forces
			+ num_base_forces_f();
	int ist_gradient_fields = ist_divergence_fields
			+ num_divergence_fields_f();
	int ist_filtered_forces = ist_gradient_fields
			+ num_gradient_fields_f();
	
	int ist_rot_filtered_forces = ist_filtered_forces
			+ num_filtered_forces_f();
	int ist_div_filtered_forces = ist_rot_filtered_forces
			+ num_rot_filtered_forces_f();
	int ist_filtered_ene_fluxes = ist_div_filtered_forces
			+ num_div_filtered_forces_f();
	int ist_SGS_terms = ist_filtered_ene_fluxes
			+ num_filtered_ene_fluxes_f();
	
	int ist_diff_SGS_terms = ist_SGS_terms
			+ num_SGS_terms_f();
	int ist_div_filter_fields = ist_diff_SGS_terms
			+ num_diff_SGS_terms_f();
	
	int ist_grad_filter_fields = ist_div_filter_fields
			+ num_div_filter_fields_f();
	int ist_wide_filter_fields = ist_grad_filter_fields
			+ num_grad_filter_fields_f();
	
	int ist_double_filter_fields = ist_wide_filter_fields
			+ num_wide_filter_fields_f();
	int ist_difference_vector = ist_double_filter_fields
			+ num_double_filter_fields_f();
	int ist_diff_filter_vector = ist_difference_vector
			+ num_difference_vector_f();
	
	int nword = ist_diff_filter_vector
			+ num_diff_filter_vector_f();
	
	int *ncomp = (int *)calloc(nword, sizeof(int));
	char *name1 = (char *)calloc(len_f*nword, sizeof(char));
	char *math1 = (char *)calloc(len_f*nword, sizeof(char));
	
	if ((names = (char **) malloc(nword*sizeof(char *))) == NULL) {
		printf("malloc error for names\n");
		exit(0);
	}
	if ((maths = (char **) malloc(nword*sizeof(char *))) == NULL) {
		printf("malloc error for maths\n");
		exit(0);
	}
	
	
	set_base_force_labels_f(&ncomp[ist_base_forces], 
								&name1[len_f*ist_base_forces],
								&math1[len_f*ist_base_forces]);
	set_divergence_field_labels_f(&ncomp[ist_divergence_fields], 
								&name1[len_f*ist_divergence_fields],
								&math1[len_f*ist_divergence_fields]);
	set_gradient_field_labels_f(&ncomp[ist_gradient_fields], 
								&name1[len_f*ist_gradient_fields],
								&math1[len_f*ist_gradient_fields]);
	
	set_SGS_term_labels_f(&ncomp[ist_SGS_terms], 
								&name1[len_f*ist_SGS_terms],
								&math1[len_f*ist_SGS_terms]);
	set_diff_SGS_term_labels_f(&ncomp[ist_diff_SGS_terms], 
								&name1[len_f*ist_diff_SGS_terms],
								&math1[len_f*ist_diff_SGS_terms]);
	
	set_filtered_force_labels_f(&ncomp[ist_filtered_forces], 
								&name1[len_f*ist_filtered_forces],
								&math1[len_f*ist_filtered_forces]);
	rot_filtered_force_labels_f(&ncomp[ist_rot_filtered_forces], 
								&name1[len_f*ist_rot_filtered_forces],
								&math1[len_f*ist_rot_filtered_forces]);
	div_filtered_force_labels_f(&ncomp[ist_div_filtered_forces], 
								&name1[len_f*ist_div_filtered_forces], 
								&math1[len_f*ist_div_filtered_forces]);
	set_filtered_ene_flax_labels_f(&ncomp[ist_filtered_ene_fluxes], 
								&name1[len_f*ist_filtered_ene_fluxes], 
								&math1[len_f*ist_filtered_ene_fluxes]);
	
	set_div_filter_field_labels_f(&ncomp[ist_div_filter_fields], 
								&name1[len_f*ist_div_filter_fields],
								&math1[len_f*ist_div_filter_fields]);
	set_grad_filter_field_labels_f(&ncomp[ist_grad_filter_fields], 
								&name1[len_f*ist_grad_filter_fields],
								&math1[len_f*ist_grad_filter_fields]);
	
	set_wide_filter_field_labels_f(&ncomp[ist_wide_filter_fields], 
								&name1[len_f*ist_wide_filter_fields], 
								&math1[len_f*ist_wide_filter_fields]);
	set_dbl_filter_field_labels_f(&ncomp[ist_double_filter_fields], 
								&name1[len_f*ist_double_filter_fields], 
								&math1[len_f*ist_double_filter_fields]);
	
	set_differnce_vector_labels_f(&ncomp[ist_difference_vector], 
								&name1[len_f*ist_difference_vector], 
								&math1[len_f*ist_difference_vector]);
	set_diff_filter_vect_labels_f(&ncomp[ist_diff_filter_vector], 
								&name1[len_f*ist_diff_filter_vector], 
								&math1[len_f*ist_diff_filter_vector]);
	
	printf("nword %d %d \n", len_f, nword);
	for(i=0;i<nword;i++){
		names[i] = (char *)calloc(strlen(&name1[len_f*i])+1, sizeof(char));
		maths[i] = (char *)calloc(strlen(&math1[len_f*i])+1, sizeof(char));
		strcpy(names[i], &name1[len_f*i]);
		strcpy(maths[i], &math1[len_f*i]);
		printf("name: %d: %d: %s: %s\n", i, ncomp[i], names[i], maths[i]);
	}
	
	return 0;
};