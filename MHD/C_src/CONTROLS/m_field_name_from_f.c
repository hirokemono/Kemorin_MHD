/*
//  m_field_name_from_f.c
//
//  Created by Hiroaki Matsui on 05/07/20.
//
*/

#include "m_field_name_from_f.h"

int lengthchara_f(void);

int count_MHD_field_groups_f();
int count_MHD_sym_field_groups_f();
int count_SGS_MHD_field_groups_f();

void MHD_field_groups_f(int *nfld_group_c, char *field_group_c);
void MHD_sym_field_groups_f(int *nfld_group_c, char *field_group_c);
void SGS_MHD_field_groups_f(int *nfld_group_c, char *field_group_c);

int num_flag_scalar_comp_f();
int num_flag_vector_comp_f();
int num_flag_sym_tensor_comp_f();
int num_flag_asym_tensor_comp_f();

void set_flag_scalar_comp_f(int *ncomp1, char *name1, char *math1);
void set_flag_vector_comp_f(int *ncomp1, char *name1, char *math1);
void set_flag_sym_tensor_comp_f(int *ncomp1, char *name1, char *math1);
void set_flag_asym_tensor_comp_f(int *ncomp1, char *name1, char *math1);

void set_primary_componnet_flag_f(char *name1);


extern void * c_link_base_field_names(void *fld_names_c);
extern void * c_link_gradient_field_names(void *fld_names_c);
extern void * c_link_divergence_field_names(void *fld_names_c);
extern void * c_link_base_force_names(void *fld_names_c);
extern void * c_link_rot_force_names(void *fld_names_c);
extern void * c_link_div_force_names(void *fld_names_c);
extern void * c_link_energy_flux_names(void *fld_names_c);
extern void * c_link_base_diffusion_names(void *fld_names_c);
extern void * c_link_field_product_names(void *fld_names_c);
extern void * c_link_base_diffusivity_names(void *fld_names_c);
extern void * c_link_explicit_work_names(void *fld_names_c);
extern void * c_link_check_fields_names(void *fld_names_c);

extern void * c_link_differnce_vector_names(void *fld_names_c);

extern void * c_link_field_w_symmetry_names(void *fld_names_c);
extern void * c_link_force_w_symmetry_names(void *fld_names_c);
extern void * c_link_sym_ene_flux_names(void *fld_names_c);

extern void * c_link_SGS_term_names(void *fld_names_c);
extern void * c_link_SGS_energy_flux_names(void *fld_names_c);
extern void * c_link_diff_SGS_term_names(void *fld_names_c);
extern void * c_link_SGS_model_coefs_names(void *fld_names_c);
extern void * c_link_filter_field_names(void *fld_names_c);
extern void * c_link_grad_filter_field_names(void *fld_names_c);
extern void * c_link_div_filter_field_names(void *fld_names_c);
extern void * c_link_filter_force_names(void *fld_names_c);
extern void * c_link_filter_eflux_names(void *fld_names_c);
extern void * c_link_rot_filter_force_names(void *fld_names_c);
extern void * c_link_div_filter_force_names(void *fld_names_c);

extern void * c_link_wide_filter_field_names(void *fld_names_c);
extern void * c_link_dbl_filter_field_names(void *fld_names_c);
extern void * c_link_diff_filter_vect_names(void *fld_names_c);
extern void * c_link_wide_SGS_term_names(void *fld_names_c);
extern void * c_link_force_with_SGS_names(void *fld_names_c);
extern void * c_link_true_SGS_term_names(void *fld_names_c);
extern void * c_link_dynamic_SGS_work_names(void *fld_names_c);


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
	int i, ist;
	
	ist = fld_list->istack_fields[0];
    fld_list->field_label = init_f_ctl_c2i_array(c_link_base_field_names, NULL);
    append_f_ctl_c2i_array(c_link_base_force_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_energy_flux_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_base_diffusion_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_rot_force_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_div_force_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_field_product_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_gradient_field_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_divergence_field_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_base_diffusivity_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_explicit_work_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_check_fields_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_differnce_vector_names, NULL, fld_list->field_label);

    append_f_ctl_c2i_array(c_link_field_w_symmetry_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_force_w_symmetry_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_sym_ene_flux_names, NULL, fld_list->field_label);

    append_f_ctl_c2i_array(c_link_SGS_term_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_SGS_energy_flux_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_diff_SGS_term_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_SGS_model_coefs_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_filter_field_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_filter_force_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_filter_eflux_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_rot_filter_force_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_div_filter_force_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_grad_filter_field_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_div_filter_field_names, NULL, fld_list->field_label);

    append_f_ctl_c2i_array(c_link_wide_filter_field_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_dbl_filter_field_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_diff_filter_vect_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_wide_SGS_term_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_force_with_SGS_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_true_SGS_term_names, NULL, fld_list->field_label);
    append_f_ctl_c2i_array(c_link_dynamic_SGS_work_names, NULL, fld_list->field_label);

    
    struct chara2_int_ctl_item *titem;
    printf("numbers %d %d\n", fld_list->istack_fields[8],
           count_chara2_int_clist(fld_list->field_label));
    for(i=ist;i<fld_list->istack_fields[8];i++){
        titem = chara2_int_clist_at_index(i, fld_list->field_label);
        printf("Field_neo %d %s %s %d\n", i, titem->c1_tbl, titem->c2_tbl, titem->i_data);
    }
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
    dealloc_chara2_int_clist(fld_list->field_label);
	
	for(i=0;i<fld_list->ntot_field_groups;i++){
		free(fld_list->field_group_name[i]);
	}
	free(fld_list->field_group_name);
	free(fld_list->num_fields);
	free(fld_list->istack_fields);
	return;
}

void check_field_name_f(struct field_names_f *fld_list){
    struct chara2_int_ctl_item *tmp_item;
    int i, j;
    
    printf("ntot_field_groups %d %d \n", 
           fld_list->len_f, fld_list->ntot_field_groups);
    for(i=0;i<fld_list->ntot_field_groups;i++){
        printf("group: %d: %d: %d: %s\n", i, fld_list->num_fields[i],
               fld_list->istack_fields[i], fld_list->field_group_name[i]);
        for(j=fld_list->istack_fields[i];j<fld_list->istack_fields[i+1];j++){
            tmp_item = chara2_int_clist_at_index(j, fld_list->field_label);
            printf("name: %d: %d: %s: %s\n", j, tmp_item->i_data,
                   tmp_item->c1_tbl, tmp_item->c2_tbl);
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
