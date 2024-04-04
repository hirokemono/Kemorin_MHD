/*
//  m_field_name_from_f.c
//
//  Created by Hiroaki Matsui on 05/07/20.
//
*/

#include "m_field_name_from_f.h"

int lengthchara_f(void);

void *c_link_MHD_field_groups_f(void *dummy);
void *c_link_MHD_sym_field_groups_f(void);
void *c_link_SGS_MHD_field_groups_f(void);

void set_primary_componnet_flag_f(char *name1);


extern void * c_link_base_field_names(void);
extern void * c_link_gradient_field_names(void);
extern void * c_link_divergence_field_names(void);
extern void * c_link_base_force_names(void);
extern void * c_link_rot_force_names(void);
extern void * c_link_div_force_names(void);
extern void * c_link_energy_flux_names(void);
extern void * c_link_base_diffusion_names(void);
extern void * c_link_field_product_names(void);
extern void * c_link_base_diffusivity_names(void);
extern void * c_link_explicit_work_names(void);
extern void * c_link_check_fields_names(void);

extern void * c_link_field_component_names(void);
extern void * c_link_differnce_vector_names(void);

extern void * c_link_field_w_symmetry_names(void);
extern void * c_link_force_w_symmetry_names(void);
extern void * c_link_sym_ene_flux_names(void);

extern void * c_link_SGS_term_names(void);
extern void * c_link_SGS_energy_flux_names(void);
extern void * c_link_diff_SGS_term_names(void);
extern void * c_link_SGS_model_coefs_names(void);
extern void * c_link_filter_field_names(void);
extern void * c_link_grad_filter_field_names(void);
extern void * c_link_div_filter_field_names(void);
extern void * c_link_filter_force_names(void);
extern void * c_link_filter_eflux_names(void);
extern void * c_link_rot_filter_force_names(void);
extern void * c_link_div_filter_force_names(void);

extern void * c_link_wide_filter_field_names(void);
extern void * c_link_dbl_filter_field_names(void);
extern void * c_link_diff_filter_vect_names(void);
extern void * c_link_wide_SGS_term_names(void);
extern void * c_link_force_with_SGS_names(void);
extern void * c_link_true_SGS_term_names(void);
extern void * c_link_dynamic_SGS_work_names(void);

extern void * c_link_scalar_dir_list_to_ctl(void);
extern void * c_link_vector_dir_list_to_ctl(void);
extern void * c_link_stensor_dir_list_to_ctl(void);
extern void * c_link_atensor_dir_list_to_ctl(void);


static void set_field_groups_from_f(struct field_names_f *fld_list){
	fld_list->len_f = lengthchara_f();
	
    fld_list->fld_grp_list = init_f_ctl_chara_array(c_link_MHD_field_groups_f, NULL);
    fld_list->istack_field_groups[0] = 0;
    fld_list->istack_field_groups[1] = count_chara_clist(fld_list->fld_grp_list);
    
    append_f_ctl_chara_array(c_link_MHD_sym_field_groups_f(), fld_list->fld_grp_list);
    fld_list->istack_field_groups[2] = count_chara_clist(fld_list->fld_grp_list);
    
    append_f_ctl_chara_array(c_link_SGS_MHD_field_groups_f(), fld_list->fld_grp_list);
    fld_list->istack_field_groups[3] = count_chara_clist(fld_list->fld_grp_list);
    fld_list->ntot_field_groups = fld_list->istack_field_groups[3];
	return;
};

static void set_field_names_from_f(struct field_names_f *fld_list){
	int i;
	if((fld_list->istack_fields = (int *)calloc(fld_list->ntot_field_groups+1, sizeof(int))) == NULL) {
		printf("malloc error for istack_fields\n");
		exit(0);
	}
	
    fld_list->field_label = init_field_label_array(c_link_base_field_names());
    fld_list->istack_fields[0] = 0;
    fld_list->istack_fields[1] = count_chara2_int_clist(fld_list->field_label);
    fld_list->istack_fields[2] = append_field_label_array(c_link_base_force_names(),
                                                          fld_list->field_label);
    fld_list->istack_fields[3] = append_field_label_array(c_link_energy_flux_names(),
                                                          fld_list->field_label);
    fld_list->istack_fields[4] = append_field_label_array(c_link_base_diffusion_names(),
                                                          fld_list->field_label);
    fld_list->istack_fields[5] = append_field_label_array(c_link_rot_force_names(),
                                                          fld_list->field_label);
    fld_list->istack_fields[6] = append_field_label_array(c_link_div_force_names(),
                                                          fld_list->field_label);
    fld_list->istack_fields[7] = append_field_label_array(c_link_field_product_names(),
                                                          fld_list->field_label);
    fld_list->istack_fields[8] = append_field_label_array(c_link_gradient_field_names(),
                                                          fld_list->field_label);
    fld_list->istack_fields[9] = append_field_label_array(c_link_divergence_field_names(),
                                                          fld_list->field_label);
    fld_list->istack_fields[10] = append_field_label_array(c_link_base_diffusivity_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[11] = append_field_label_array(c_link_explicit_work_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[12] = append_field_label_array(c_link_check_fields_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[13] = append_field_label_array(c_link_field_component_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[14] = append_field_label_array(c_link_differnce_vector_names(),
                                                           fld_list->field_label);

    fld_list->istack_fields[15] = append_field_label_array(c_link_field_w_symmetry_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[15] = append_field_label_array(c_link_force_w_symmetry_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[17] = append_field_label_array(c_link_sym_ene_flux_names(),
                                                           fld_list->field_label);

    fld_list->istack_fields[18] = append_field_label_array(c_link_SGS_term_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[19] = append_field_label_array(c_link_SGS_energy_flux_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[20] = append_field_label_array(c_link_diff_SGS_term_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[21] = append_field_label_array(c_link_SGS_model_coefs_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[22] = append_field_label_array(c_link_filter_field_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[23] = append_field_label_array(c_link_filter_force_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[24] = append_field_label_array(c_link_filter_eflux_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[25] = append_field_label_array(c_link_rot_filter_force_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[26] = append_field_label_array(c_link_div_filter_force_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[27] = append_field_label_array(c_link_grad_filter_field_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[28] = append_field_label_array(c_link_div_filter_field_names(),
                                                           fld_list->field_label);

    fld_list->istack_fields[29] = append_field_label_array(c_link_wide_filter_field_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[30] = append_field_label_array(c_link_dbl_filter_field_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[31] = append_field_label_array(c_link_diff_filter_vect_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[32] = append_field_label_array(c_link_wide_SGS_term_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[33] = append_field_label_array(c_link_force_with_SGS_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[34] = append_field_label_array(c_link_true_SGS_term_names(),
                                                           fld_list->field_label);
    fld_list->istack_fields[35] = append_field_label_array(c_link_dynamic_SGS_work_names(),
                                                           fld_list->field_label);
	fld_list->ntot_fields = fld_list->istack_fields[fld_list->ntot_field_groups];

    
    struct chara2_int_ctl_item *titem;
    printf("numbers %d %d\n", fld_list->istack_fields[8],
           count_chara2_int_clist(fld_list->field_label));
    for(i=0;i<fld_list->ntot_fields;i++){
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
    dealloc_chara2_int_clist(fld_list->field_label);
    dealloc_chara_clist(fld_list->fld_grp_list);

    free(fld_list->istack_fields);
	return;
}

void check_field_name_f(struct field_names_f *fld_list){
    struct chara_ctl_item *tmp_grp;
    struct chara2_int_ctl_item *tmp_item;
    int i, j;
    
    printf("ntot_field_groups %d %d \n", 
           fld_list->len_f, fld_list->ntot_field_groups);
    for(i=0;i<fld_list->ntot_field_groups;i++){
        tmp_grp = chara_clist_at_index(i, fld_list->fld_grp_list);
        printf("group: %d: %d: %s\n", i, fld_list->istack_fields[i], tmp_grp->c_tbl);
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


struct component_flags_f * init_component_flags_f(){
    struct component_flags_f *comp_flags;
    if((comp_flags = (struct component_flags_f *) malloc(sizeof(struct component_flags_f))) == NULL){
        printf("malloc error for component_flags_f\n");
        exit(0);
    };
    
    comp_flags->scalar_components_flag =      init_field_label_array(c_link_scalar_dir_list_to_ctl);
    comp_flags->vector_components_flag =      init_field_label_array(c_link_vector_dir_list_to_ctl());
    comp_flags->sym_tensor_components_flag =  init_field_label_array(c_link_stensor_dir_list_to_ctl());
    comp_flags->asym_tensor_components_list = init_field_label_array(c_link_atensor_dir_list_to_ctl());
    return comp_flags;
};
void dealloc_component_flags_f(struct component_flags_f *comp_flags){
    dealloc_chara2_int_clist(comp_flags->scalar_components_flag);
    dealloc_chara2_int_clist(comp_flags->vector_components_flag);
    dealloc_chara2_int_clist(comp_flags->sym_tensor_components_flag);
    dealloc_chara2_int_clist(comp_flags->asym_tensor_components_list);
    free(comp_flags);
    return;
};
void check_component_flags_f(struct component_flags_f *comp_flags){
    check_flag_with_math_f(comp_flags->scalar_components_flag);
    check_flag_with_math_f(comp_flags->vector_components_flag);
    check_flag_with_math_f(comp_flags->sym_tensor_components_flag);
    check_flag_with_math_f(comp_flags->asym_tensor_components_list);
    return;
};
