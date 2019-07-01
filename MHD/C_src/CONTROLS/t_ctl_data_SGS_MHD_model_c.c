/*
//  t_ctl_data_SGS_MHD_model_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/05/15.
*/

#include "t_ctl_data_SGS_MHD_model_c.h"

const char label_mhd_model_ctl[NLBL_MHD_MODEL_CTL][KCHARA_C] = {
    /*[ 0]*/    {"phys_values_ctl"},
    /*[ 1]*/    {"time_evolution_ctl"},
    /*[ 2]*/    {"layers_ctl"},
    /*[ 3]*/    {"boundary_condition"},
    /*[ 4]*/    {"bc_4_surface"},
    /*[ 5]*/    {"forces_define"},
    /*[ 6]*/    {"dimensionless_ctl"},
    /*[ 7]*/    {"coefficients_ctl"},
    /*[ 8]*/    {"gravity_define"},
    /*[ 9]*/    {"Coriolis_define"},
    /*[10]*/    {"Magneto_convection_def"},
    /*[11]*/    {"temperature_define"},
    /*[12]*/    {"composition_define"},
    /*[13]*/    {"SGS_control"}
};
	
const char label_bc_4_nod[KCHARA_C] = {
    /*[14]*/    "bc_4_node"
};

const char label_mhd_control_ctl[NLBL_MHD_CONTROL_CTL][KCHARA_C] = {
    /*[ 0]*/    {"time_step_ctl"},
    /*[ 1]*/    {"restart_file_ctl"},
    /*[ 2]*/    {"time_loop_ctl"}
};


void get_label_mhd_model_ctl(int index, char *label){
    if(index < NLBL_MHD_MODEL_CTL) strngcopy(label, label_mhd_model_ctl[index]);
    return;
};
void get_label_mhd_control_ctl(int index, char *label){
    if(index < NLBL_MHD_CONTROL_CTL) strngcopy(label, label_mhd_control_ctl[index]);
    return;
};


void alloc_mhd_model_control_c(struct mhd_model_control_c *model_ctl){
    int i;
    
    model_ctl->iflag_use = 0;
    model_ctl->maxlen = 0;
    for (i=0;i<NLBL_MHD_MODEL_CTL;i++){
        if(strlen(label_mhd_model_ctl[i]) > model_ctl->maxlen){
            model_ctl->maxlen = (int) strlen(label_mhd_model_ctl[i]);
        };
    };
    
    model_ctl->fld_ctl = (struct field_ctl_c *) malloc(sizeof(struct field_ctl_c));
	alloc_field_ctl_c(model_ctl->fld_ctl);
	model_ctl->evo_ctl = (struct mhd_evolution_ctl_c *) malloc(sizeof(struct mhd_evolution_ctl_c));
	alloc_mhd_evolution_ctl_c(model_ctl->evo_ctl);
	model_ctl->earea_ctl = (struct mhd_evo_area_ctl_c *) malloc(sizeof(struct mhd_evo_area_ctl_c));
	alloc_mhd_evo_area_ctl_c(model_ctl->earea_ctl);
	
	model_ctl->nbc_ctl = (struct MHD_boundary_ctl_c *) malloc(sizeof(struct MHD_boundary_ctl_c));
	alloc_MHD_node_bc_ctl_c(model_ctl->nbc_ctl);
	model_ctl->sbc_ctl = (struct MHD_boundary_ctl_c *) malloc(sizeof(struct MHD_boundary_ctl_c));
	alloc_MHD_surf_bc_ctl_c(model_ctl->sbc_ctl);
	
    model_ctl->frc_ctl = (struct forces_ctl_c *) malloc(sizeof(struct forces_ctl_c));
    alloc_forces_ctl_c(model_ctl->frc_ctl);
	model_ctl->dless_ctl = (struct dimless_ctl_c *) malloc(sizeof(struct dimless_ctl_c));
	alloc_dimless_ctl_c(model_ctl->dless_ctl);
	model_ctl->eqs_ctl = (struct equations_ctl_c *) malloc(sizeof(struct equations_ctl_c));
	alloc_equations_ctl_c(model_ctl->eqs_ctl);
	
	model_ctl->g_ctl = (struct gravity_ctl_c *) malloc(sizeof(struct gravity_ctl_c));
	alloc_gravity_ctl_c(model_ctl->g_ctl);
	model_ctl->cor_ctl = (struct coriolis_ctl_c *) malloc(sizeof(struct coriolis_ctl_c));
	alloc_coriolis_ctl_c(model_ctl->cor_ctl);
	model_ctl->mcv_ctl = (struct magneto_cv_ctl_c *) malloc(sizeof(struct magneto_cv_ctl_c));
	alloc_magneto_cv_ctl_c(model_ctl->mcv_ctl);
	
	model_ctl->reft_ctl = (struct reference_temperature_c *) malloc(sizeof(struct reference_temperature_c));
	alloc_ref_temperature_ctl_c(model_ctl->reft_ctl);
	model_ctl->refc_ctl = (struct reference_temperature_c *) malloc(sizeof(struct reference_temperature_c));
	alloc_ref_temperature_ctl_c(model_ctl->refc_ctl);
	
	model_ctl->sgs_ctl = (struct SGS_model_control_c *) malloc(sizeof(struct SGS_model_control_c));
	alloc_SGS_model_ctl_c(model_ctl->sgs_ctl);
	
	return;
}

void dealloc_mhd_model_control_c(struct mhd_model_control_c *model_ctl){
	dealloc_field_ctl_c(model_ctl->fld_ctl);
	free(model_ctl->fld_ctl);
	dealloc_mhd_evolution_ctl_c(model_ctl->evo_ctl);
	free(model_ctl->evo_ctl);
	dealloc_mhd_evo_area_ctl_c(model_ctl->earea_ctl);
	free(model_ctl->earea_ctl);
	
	dealloc_MHD_boundary_ctl_c(model_ctl->nbc_ctl);
	free(model_ctl->nbc_ctl);
	dealloc_MHD_boundary_ctl_c(model_ctl->sbc_ctl);
	free(model_ctl->sbc_ctl);
	
	
    dealloc_forces_ctl_c(model_ctl->frc_ctl);
    free(model_ctl->frc_ctl);
	dealloc_dimless_ctl_c(model_ctl->dless_ctl);
	free(model_ctl->dless_ctl);
	dealloc_equations_ctl_c(model_ctl->eqs_ctl);
	free(model_ctl->eqs_ctl);
	
	dealloc_gravity_ctl_c(model_ctl->g_ctl);
	free(model_ctl->g_ctl);
	dealloc_coriolis_ctl_c(model_ctl->cor_ctl);
	free(model_ctl->cor_ctl);
	dealloc_magneto_cv_ctl_c(model_ctl->mcv_ctl);
	free(model_ctl->mcv_ctl);
	
	dealloc_ref_temperature_ctl_c(model_ctl->reft_ctl);
	free(model_ctl->reft_ctl);
	dealloc_ref_temperature_ctl_c(model_ctl->refc_ctl);
	free(model_ctl->refc_ctl);
	
	dealloc_SGS_model_ctl_c(model_ctl->sgs_ctl);
	free(model_ctl->sgs_ctl);
    
    model_ctl->iflag_use = 0;
	return;
}

void read_mhd_model_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct mhd_model_control_c *model_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 0]) > 0) {
            read_field_ctl_c(fp, buf, label_mhd_model_ctl[ 0], model_ctl->fld_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 1]) > 0) {
			read_mhd_evolution_ctl_c(fp, buf, label_mhd_model_ctl[ 1], model_ctl->evo_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 2]) > 0) {
			read_mhd_evo_area_ctl_c(fp, buf, label_mhd_model_ctl[ 2], model_ctl->earea_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 3]) > 0) {
			read_MHD_node_bc_ctl_c(fp, buf, label_mhd_model_ctl[ 3], model_ctl->nbc_ctl);
		};
		if(right_begin_flag_c(buf, label_bc_4_nod) > 0) {
			read_MHD_node_bc_ctl_c(fp, buf, label_bc_4_nod, model_ctl->nbc_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 4]) > 0) {
			read_MHD_surf_bc_ctl_c(fp, buf, label_mhd_model_ctl[ 4], model_ctl->sbc_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 5]) > 0) {
			read_forces_ctl_c(fp, buf, label_mhd_model_ctl[ 5], model_ctl->frc_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 6]) > 0) {
			read_dimless_ctl_c(fp, buf, label_mhd_model_ctl[ 6], model_ctl->dless_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 7]) > 0) {
			read_equations_ctl_c(fp, buf, label_mhd_model_ctl[ 7], model_ctl->eqs_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 8]) > 0) {
			read_gravity_ctl_c(fp, buf, label_mhd_model_ctl[ 8], model_ctl->g_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 9]) > 0) {
			read_coriolis_ctl_c(fp, buf, label_mhd_model_ctl[ 9], model_ctl->cor_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[10]) > 0) {
			read_magneto_cv_ctl_c(fp, buf, label_mhd_model_ctl[10], model_ctl->mcv_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[11]) > 0) {
			read_ref_temperature_ctl_c(fp, buf, label_mhd_model_ctl[11], model_ctl->reft_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[12]) > 0) {
			read_ref_composition_ctl_c(fp, buf, label_mhd_model_ctl[12], model_ctl->refc_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[13]) > 0) {
			read_SGS_model_ctl_c(fp, buf, label_mhd_model_ctl[13], model_ctl->sgs_ctl);
		};
	};
    model_ctl->iflag_use = 1;
	return;
}

int write_mhd_model_ctl_c(FILE *fp, int level, const char *label,
                          struct mhd_model_control_c *model_ctl){
    if(model_ctl->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
    level = write_field_ctl_c(fp, level, label_mhd_model_ctl[ 0], model_ctl->fld_ctl);
    level = write_mhd_evolution_ctl_c(fp, level, label_mhd_model_ctl[ 1], model_ctl->evo_ctl);
    level = write_mhd_evo_area_ctl_c(fp, level, label_mhd_model_ctl[ 2], model_ctl->earea_ctl);
    level = write_MHD_node_bc_ctl_c(fp, level, label_mhd_model_ctl[ 3], model_ctl->nbc_ctl);
    level = write_MHD_surf_bc_ctl_c(fp, level, label_mhd_model_ctl[ 4], model_ctl->sbc_ctl);
    level = write_forces_ctl_c(fp, level, label_mhd_model_ctl[ 5], model_ctl->frc_ctl);
    level = write_dimless_ctl_c(fp, level, label_mhd_model_ctl[ 6], model_ctl->dless_ctl);
    level = write_equations_ctl_c(fp, level, label_mhd_model_ctl[ 7], model_ctl->eqs_ctl);
    level = write_gravity_ctl_c(fp, level, label_mhd_model_ctl[ 8], model_ctl->g_ctl);
    level = write_coriolis_ctl_c(fp, level, label_mhd_model_ctl[ 9], model_ctl->cor_ctl);
    level = write_magneto_cv_ctl_c(fp, level, label_mhd_model_ctl[10], model_ctl->mcv_ctl);
    level = write_ref_temperature_ctl_c(fp, level, label_mhd_model_ctl[11], model_ctl->reft_ctl);
    level = write_ref_composition_ctl_c(fp, level, label_mhd_model_ctl[12], model_ctl->refc_ctl);
    level = write_SGS_model_ctl_c(fp, level, label_mhd_model_ctl[13], model_ctl->sgs_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
}


void alloc_sph_mhd_control_control_c(struct sph_mhd_control_control_c *control_ctl){
    int i;
    
    control_ctl->iflag_use = 0;
    control_ctl->maxlen = 0;
    for (i=0;i<NLBL_MHD_CONTROL_CTL;i++){
        if(strlen(label_mhd_control_ctl[i]) > control_ctl->maxlen){
            control_ctl->maxlen = (int) strlen(label_mhd_control_ctl[i]);
        };
	};
	
	control_ctl->tctl = (struct time_data_control_c *) malloc(sizeof(struct time_data_control_c));
	alloc_time_data_control_c(control_ctl->tctl);
	control_ctl->mrst_ctl = (struct mhd_restart_control_c *) malloc(sizeof(struct mhd_restart_control_c));
	alloc_mhd_restart_control_c(control_ctl->mrst_ctl);
	control_ctl->mevo_ctl = (struct mhd_evo_scheme_control_c *) malloc(sizeof(struct mhd_evo_scheme_control_c));
	alloc_mhd_evo_scheme_control_c(control_ctl->mevo_ctl);
	return;
}

void dealloc_sph_mhd_control_control_c(struct sph_mhd_control_control_c *control_ctl){
	dealloc_time_data_control_c(control_ctl->tctl);
	free(control_ctl->tctl);
	dealloc_mhd_restart_control_c(control_ctl->mrst_ctl);
	free(control_ctl->mrst_ctl);
	dealloc_mhd_evo_scheme_control_c(control_ctl->mevo_ctl);
	free(control_ctl->mevo_ctl);
    
    control_ctl->iflag_use = 0;
	return;
}

void read_mhd_control_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct sph_mhd_control_control_c *control_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		if(right_begin_flag_c(buf, label_mhd_control_ctl[ 0]) > 0)
			read_time_data_control_c(fp, buf, label_mhd_control_ctl[ 0], control_ctl->tctl);
		if(right_begin_flag_c(buf, label_mhd_control_ctl[ 1]) > 0) 
			read_mhd_restart_control_c(fp, buf, label_mhd_control_ctl[ 1], control_ctl->mrst_ctl);
		if(right_begin_flag_c(buf, label_mhd_control_ctl[ 2]) > 0) 
			read_mhd_evo_scheme_control_c(fp, buf, label_mhd_control_ctl[ 2], control_ctl->mevo_ctl);
	};
    control_ctl->iflag_use = 1;
	return;
}

int write_mhd_control_ctl_c(FILE *fp, int level, const char *label,
                            struct sph_mhd_control_control_c *control_ctl){
    if(control_ctl->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
    level = write_time_data_control_c(fp, level, label_mhd_control_ctl[ 0], control_ctl->tctl);
    level = write_mhd_restart_control_c(fp, level, label_mhd_control_ctl[ 1], control_ctl->mrst_ctl);
    level = write_mhd_evo_scheme_control_c(fp, level, label_mhd_control_ctl[ 2], control_ctl->mevo_ctl);
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
}
