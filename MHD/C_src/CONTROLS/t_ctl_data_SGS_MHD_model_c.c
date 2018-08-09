/*
//  t_ctl_data_SGS_MHD_model_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/05/15.
*/

#include "t_ctl_data_SGS_MHD_model_c.h"

#define NLBL_MHD_MODEL_CTL  15
#define NLBL_MHD_CONTROL_CTL  15

const char label_mhd_model_ctl[NLBL_MHD_MODEL_CTL][KCHARA_C] = {
    /*[ 0]*/    {"phys_values_ctl"},
    /*[ 1]*/    {"time_evolution_ctl"},
    /*[ 2]*/    {"layers_ctl"},
    /*[ 3]*/    {"boundary_condition"},
    /*[ 4]*/    {"bc_4_surface"},
    /*[ 5]*/    {"dimensionless_ctl"},
    /*[ 6]*/    {"coefficients_ctl"},
    /*[ 7]*/    {"forces_define"},
    /*[ 8]*/    {"gravity_define"},
    /*[ 9]*/    {"Coriolis_define"},
    /*[10]*/    {"Magneto_convection_def"},
    /*[11]*/    {"temperature_define"},
    /*[12]*/    {"composition_define"},
    /*[13]*/    {"SGS_control"},
	
    /*[14]*/    {"bc_4_node"}
};

const char label_mhd_control_ctl[NLBL_MHD_CONTROL_CTL][KCHARA_C] = {
    /*[ 0]*/    {"time_step_ctl"},
    /*[ 1]*/    {"restart_file_ctl"},
    /*[ 2]*/    {"time_loop_ctl"}
};

void alloc_mhd_model_control_c(struct mhd_model_control_c *model_ctl){
    int i;
    
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
	
	model_ctl->dless_ctl = (struct dimless_ctl_c *) malloc(sizeof(struct dimless_ctl_c));
	alloc_dimless_ctl_c(model_ctl->dless_ctl);
	model_ctl->eqs_ctl = (struct equations_ctl_c *) malloc(sizeof(struct equations_ctl_c));
	alloc_equations_ctl_c(model_ctl->eqs_ctl);
	
	model_ctl->frc_ctl = (struct forces_ctl_c *) malloc(sizeof(struct forces_ctl_c));
	alloc_forces_ctl_c(model_ctl->frc_ctl);
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
	model_ctl->iflag_field_control =              0;
	model_ctl->iflag_mhd_evolution_control =      0;
	model_ctl->iflag_mhd_evo_area_control =       0;
	model_ctl->iflag_node_bc_control =            0;
	model_ctl->iflag_surf_bc_control =            0;
	model_ctl->iflag_dimless_control =            0;
	model_ctl->iflag_equations_control =          0;
	model_ctl->iflag_forces_control =             0;
	model_ctl->iflag_gravity_control =            0;
	model_ctl->iflag_coriolis_control =           0;
	model_ctl->iflag_magneto_convection_control = 0;
	model_ctl->iflag_reference_temp_control =     0;
	model_ctl->iflag_reference_comp_control =     0;
	model_ctl->iflag_sgs_model_control =          0;
	
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
	
	
	dealloc_dimless_ctl_c(model_ctl->dless_ctl);
	free(model_ctl->dless_ctl);
	dealloc_equations_ctl_c(model_ctl->eqs_ctl);
	free(model_ctl->eqs_ctl);
	
	dealloc_forces_ctl_c(model_ctl->frc_ctl);
	free(model_ctl->frc_ctl);
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
	return;
}

int read_mhd_model_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct mhd_model_control_c *model_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 0]) > 0) {
			model_ctl->iflag_field_control = read_field_ctl_c(fp, buf, 
						label_mhd_model_ctl[ 0], model_ctl->fld_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 1]) > 0) {
			model_ctl->iflag_mhd_evolution_control = read_mhd_evolution_ctl_c(fp, buf,
						label_mhd_model_ctl[ 1], model_ctl->evo_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 2]) > 0) {
			model_ctl->iflag_mhd_evo_area_control = read_mhd_evo_area_ctl_c(fp, buf,
						label_mhd_model_ctl[ 2], model_ctl->earea_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 3]) > 0) {
			model_ctl->iflag_node_bc_control = read_MHD_node_bc_ctl_c(fp, buf, 
						label_mhd_model_ctl[ 3], model_ctl->nbc_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[14]) > 0) {
			model_ctl->iflag_node_bc_control = read_MHD_node_bc_ctl_c(fp, buf, 
						label_mhd_model_ctl[14], model_ctl->nbc_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 4]) > 0) {
			model_ctl->iflag_surf_bc_control = read_MHD_surf_bc_ctl_c(fp, buf, 
						label_mhd_model_ctl[ 4], model_ctl->sbc_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 7]) > 0) {
			model_ctl->iflag_forces_control = read_forces_ctl_c(fp, buf,
						label_mhd_model_ctl[ 7], model_ctl->frc_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 5]) > 0) {
			model_ctl->iflag_dimless_control = read_dimless_ctl_c(fp, buf, 
						label_mhd_model_ctl[ 5], model_ctl->dless_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 6]) > 0) {
			model_ctl->iflag_equations_control = read_equations_ctl_c(fp, buf, 
						label_mhd_model_ctl[ 6], model_ctl->eqs_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 8]) > 0) {
			model_ctl->iflag_gravity_control = read_gravity_ctl_c(fp, buf,
						label_mhd_model_ctl[ 8], model_ctl->g_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[ 9]) > 0) {
			model_ctl->iflag_coriolis_control = read_coriolis_ctl_c(fp, buf,
						label_mhd_model_ctl[ 9], model_ctl->cor_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[10]) > 0) {
			model_ctl->iflag_magneto_convection_control = read_magneto_cv_ctl_c(fp, buf, 
						label_mhd_model_ctl[10], model_ctl->mcv_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[11]) > 0) {
			model_ctl->iflag_reference_temp_control = read_ref_temperature_ctl_c(fp, buf, 
						label_mhd_model_ctl[11], model_ctl->reft_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[12]) > 0) {
			model_ctl->iflag_reference_comp_control = read_ref_composition_ctl_c(fp, buf,
						label_mhd_model_ctl[12], model_ctl->refc_ctl);
		};
		if(right_begin_flag_c(buf, label_mhd_model_ctl[13]) > 0) {
			model_ctl->iflag_sgs_model_control = read_SGS_model_ctl_c(fp, buf, label_mhd_model_ctl[13], 
						model_ctl->sgs_ctl);
		};
	};
	return 1;
}

int write_mhd_model_ctl_c(FILE *fp, int level, const char *label,
                          struct mhd_model_control_c *model_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
    if(model_ctl->iflag_field_control > 0) {
		level = write_field_ctl_c(fp, level, label_mhd_model_ctl[ 0], model_ctl->fld_ctl);
    };
    if(model_ctl->iflag_mhd_evolution_control > 0) {
        fprintf(fp, "!\n");
		level = write_mhd_evolution_ctl_c(fp, level, label_mhd_model_ctl[ 1], model_ctl->evo_ctl);
    };
    if(model_ctl->iflag_mhd_evo_area_control > 0){ 
        fprintf(fp, "!\n");
		level = write_mhd_evo_area_ctl_c(fp, level, label_mhd_model_ctl[ 2], model_ctl->earea_ctl);
    };
    if(model_ctl->iflag_node_bc_control > 0){
        fprintf(fp, "!\n");
		level = write_MHD_node_bc_ctl_c(fp, level, label_mhd_model_ctl[ 3], model_ctl->nbc_ctl);
    };
    if(model_ctl->iflag_surf_bc_control > 0){
        fprintf(fp, "!\n");
		level = write_MHD_surf_bc_ctl_c(fp, level, label_mhd_model_ctl[ 4], model_ctl->sbc_ctl);
    };
    if(model_ctl->iflag_dimless_control > 0){
        fprintf(fp, "!\n");
		level = write_dimless_ctl_c(fp, level, label_mhd_model_ctl[ 5], model_ctl->dless_ctl);
    };
    if(model_ctl->iflag_equations_control > 0){
        fprintf(fp, "!\n");
		level = write_equations_ctl_c(fp, level, label_mhd_model_ctl[ 6], model_ctl->eqs_ctl);
    };
    if(model_ctl->iflag_forces_control > 0){
        fprintf(fp, "!\n");
		level = write_forces_ctl_c(fp, level, label_mhd_model_ctl[ 7], model_ctl->frc_ctl);
    };
    if(model_ctl->iflag_gravity_control > 0){
        fprintf(fp, "!\n");
		level = write_gravity_ctl_c(fp, level, label_mhd_model_ctl[ 8], model_ctl->g_ctl);
    };
    if(model_ctl->iflag_coriolis_control > 0){
        fprintf(fp, "!\n");
		level = write_coriolis_ctl_c(fp, level, label_mhd_model_ctl[ 9], model_ctl->cor_ctl);
    };
    if(model_ctl->iflag_magneto_convection_control > 0){
        fprintf(fp, "!\n");
		level = write_magneto_cv_ctl_c(fp, level, label_mhd_model_ctl[10], model_ctl->mcv_ctl);
    };
    if(model_ctl->iflag_reference_temp_control > 0){
        fprintf(fp, "!\n");
		level = write_ref_temperature_ctl_c(fp, level, label_mhd_model_ctl[11], model_ctl->reft_ctl);
    };
    if(model_ctl->iflag_reference_comp_control > 0){
        fprintf(fp, "!\n");
		level = write_ref_composition_ctl_c(fp, level, label_mhd_model_ctl[12], model_ctl->refc_ctl);
    };
    if(model_ctl->iflag_sgs_model_control > 0){
        fprintf(fp, "!\n");
		level = write_SGS_model_ctl_c(fp, level, label_mhd_model_ctl[13], model_ctl->sgs_ctl);
    };
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
}


void alloc_sph_mhd_control_control_c(struct sph_mhd_control_control_c *control_ctl){
    int i;
    
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
	
	control_ctl->iflag_time_data_control =      0;
	control_ctl->iflag_mhd_restart_control =    0;
	control_ctl->iflag_mhd_evo_scheme_control = 0;
	return;
}

void dealloc_sph_mhd_control_control_c(struct sph_mhd_control_control_c *control_ctl){
	control_ctl->iflag_time_data_control =      0;
	control_ctl->iflag_mhd_restart_control =    0;
	control_ctl->iflag_mhd_evo_scheme_control = 0;
	
	dealloc_time_data_control_c(control_ctl->tctl);
	free(control_ctl->tctl);
	dealloc_mhd_restart_control_c(control_ctl->mrst_ctl);
	free(control_ctl->mrst_ctl);
	dealloc_mhd_evo_scheme_control_c(control_ctl->mevo_ctl);
	free(control_ctl->mevo_ctl);
	return;
}

int read_mhd_control_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct sph_mhd_control_control_c *control_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		if(right_begin_flag_c(buf, label_mhd_control_ctl[ 0]) > 0)
			control_ctl->iflag_time_data_control = read_time_data_control_c(fp, buf, label_mhd_control_ctl[ 0], control_ctl->tctl);
		if(right_begin_flag_c(buf, label_mhd_control_ctl[ 1]) > 0) 
			control_ctl->iflag_mhd_restart_control = read_mhd_restart_control_c(fp, buf, label_mhd_control_ctl[ 1], control_ctl->mrst_ctl);
		if(right_begin_flag_c(buf, label_mhd_control_ctl[ 2]) > 0) 
			control_ctl->iflag_mhd_evo_scheme_control = read_mhd_evo_scheme_control_c(fp, buf, label_mhd_control_ctl[ 2], control_ctl->mevo_ctl);
	};
	return 1;
}

int write_mhd_control_ctl_c(FILE *fp, int level, const char *label,
                            struct sph_mhd_control_control_c *control_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
    if(control_ctl->iflag_time_data_control > 0){
        level = write_time_data_control_c(fp, level, label_mhd_control_ctl[ 0], control_ctl->tctl);
    };
    if(control_ctl->iflag_mhd_restart_control > 0){
        fprintf(fp, "!\n");
        level = write_mhd_restart_control_c(fp, level, label_mhd_control_ctl[ 1], control_ctl->mrst_ctl);
    }
    if(control_ctl->iflag_mhd_evo_scheme_control > 0){
        fprintf(fp, "!\n");
        level = write_mhd_evo_scheme_control_c(fp, level, label_mhd_control_ctl[ 2], control_ctl->mevo_ctl);
    };
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
}
