/*
//  t_SGS_MHD_control_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/05/15.
*/

#include "t_SGS_MHD_control_c.h"



void alloc_parallel_sph_shell_control_c(struct parallel_sph_shell_control_c *shell_ctl){
	shell_ctl->iflag_sph_shell = (int *)calloc(1, sizeof(int));
	shell_ctl->iflag_sph_domain = (int *)calloc(1, sizeof(int));
	
	shell_ctl->sp_ctl = (struct sphere_data_control_c *) malloc(sizeof(struct sphere_data_control_c));
	shell_ctl->sd_ctl = (struct sphere_domain_control_c *) malloc(sizeof(struct sphere_domain_control_c));
	return;
}

void dealloc_parallel_sph_shell_control_c(struct parallel_sph_shell_control_c *shell_ctl){
	free(shell_ctl->iflag_sph_domain);
	free(shell_ctl->iflag_sph_shell);
	
	free(shell_ctl->sp_ctl);
	free(shell_ctl->sd_ctl);
	return;
}

void alloc_mhd_model_control_c(struct mhd_model_control_c *model_ctl){
	model_ctl->iflag_field_control = (int *)calloc(1, sizeof(int));
	model_ctl->iflag_mhd_evolution_control = (int *)calloc(1, sizeof(int));
	model_ctl->iflag_mhd_evo_area_control = (int *)calloc(1, sizeof(int));
	model_ctl->iflag_node_bc_control = (int *)calloc(1, sizeof(int));
	model_ctl->iflag_surf_bc_control = (int *)calloc(1, sizeof(int));
	model_ctl->iflag_dimless_control = (int *)calloc(1, sizeof(int));
	model_ctl->iflag_equations_control = (int *)calloc(1, sizeof(int));
	model_ctl->iflag_forces_control = (int *)calloc(1, sizeof(int));
	model_ctl->iflag_gravity_control = (int *)calloc(1, sizeof(int));
	model_ctl->iflag_coriolis_control = (int *)calloc(1, sizeof(int));
	model_ctl->iflag_magneto_convection_control = (int *)calloc(1, sizeof(int));
	model_ctl->iflag_reference_temp_control = (int *)calloc(1, sizeof(int));
	model_ctl->iflag_reference_comp_control = (int *)calloc(1, sizeof(int));
	model_ctl->iflag_sgs_model_control = (int *)calloc(1, sizeof(int));
	
	model_ctl->fld_ctl = (struct field_control_c *) malloc(sizeof(struct field_control_c));
	model_ctl->evo_ctl = (struct mhd_evolution_control_c *) malloc(sizeof(struct mhd_evolution_control_c));
	model_ctl->earea_ctl = (struct mhd_evo_area_control_c *) malloc(sizeof(struct mhd_evo_area_control_c));
	model_ctl->nbc_ctl = (struct node_bc_control_c *) malloc(sizeof(struct node_bc_control_c));
	model_ctl->sbc_ctl = (struct surf_bc_control_c *) malloc(sizeof(struct surf_bc_control_c));
	model_ctl->dless_ctl = (struct dimless_control_c *) malloc(sizeof(struct dimless_control_c));
	model_ctl->eqs_ctl = (struct equations_control_c *) malloc(sizeof(struct equations_control_c));
	model_ctl->frc_ctl = (struct forces_control_c *) malloc(sizeof(struct forces_control_c));
	model_ctl->g_ctl = (struct gravity_control_c *) malloc(sizeof(struct gravity_control_c));
	model_ctl->cor_ctl = (struct coriolis_control_c *) malloc(sizeof(struct coriolis_control_c));
	model_ctl->mcv_ctl = (struct magneto_convection_control_c *) malloc(sizeof(struct magneto_convection_control_c));
	model_ctl->reft_ctl = (struct reference_temperature_c *) malloc(sizeof(struct reference_temperature_c));
	model_ctl->refc_ctl = (struct reference_temperature_c *) malloc(sizeof(struct reference_temperature_c));
	model_ctl->sgs_ctl = (struct SGS_model_control_c *) malloc(sizeof(struct SGS_model_control_c));
	alloc_SGS_model_ctl_c(model_ctl->sgs_ctl);
	
	return;
}

void dealloc_mhd_model_control_c(struct mhd_model_control_c *model_ctl){
	free(model_ctl->iflag_field_control);
	free(model_ctl->iflag_mhd_evolution_control);
	free(model_ctl->iflag_mhd_evo_area_control);
	free(model_ctl->iflag_node_bc_control);
	free(model_ctl->iflag_surf_bc_control);
	free(model_ctl->iflag_dimless_control);
	free(model_ctl->iflag_equations_control);
	free(model_ctl->iflag_forces_control);
	free(model_ctl->iflag_gravity_control);
	free(model_ctl->iflag_coriolis_control);
	free(model_ctl->iflag_magneto_convection_control);
	free(model_ctl->iflag_reference_temp_control);
	free(model_ctl->iflag_reference_comp_control);
	free(model_ctl->iflag_sgs_model_control);
	
	dealloc_SGS_model_ctl_c(model_ctl->sgs_ctl);
	
	free(model_ctl->fld_ctl);
	free(model_ctl->evo_ctl);
	free(model_ctl->earea_ctl);
	free(model_ctl->nbc_ctl);
	free(model_ctl->sbc_ctl);
	free(model_ctl->dless_ctl);
	free(model_ctl->eqs_ctl);
	free(model_ctl->frc_ctl);
	free(model_ctl->g_ctl);
	free(model_ctl->cor_ctl);
	free(model_ctl->mcv_ctl);
	free(model_ctl->reft_ctl);
	free(model_ctl->refc_ctl);
	free(model_ctl->sgs_ctl);
	return;
}

void alloc_sph_mhd_control_control_c(struct sph_mhd_control_control_c *control_ctl){
	control_ctl->iflag_time_data_control = (int *)calloc(1, sizeof(int));
	control_ctl->iflag_mhd_restart_control = (int *)calloc(1, sizeof(int));
	control_ctl->iflag_mhd_evo_scheme_control = (int *)calloc(1, sizeof(int));
	
	control_ctl->tctl = (struct time_data_control_c *) malloc(sizeof(struct time_data_control_c));
	alloc_time_data_control_c(control_ctl->tctl);
	control_ctl->mrst_ctl = (struct mhd_restart_control_c *) malloc(sizeof(struct mhd_restart_control_c));
	alloc_mhd_restart_control_c(control_ctl->mrst_ctl);
	control_ctl->mevo_ctl = (struct mhd_evo_scheme_control_c *) malloc(sizeof(struct mhd_evo_scheme_control_c));
	alloc_mhd_evo_scheme_control_c(control_ctl->mevo_ctl);
	return;
}

void dealloc_sph_mhd_control_control_c(struct sph_mhd_control_control_c *control_ctl){
	free(control_ctl->iflag_time_data_control);
	free(control_ctl->iflag_mhd_restart_control);
	free(control_ctl->iflag_mhd_evo_scheme_control);
	
	dealloc_time_data_control_c(control_ctl->tctl);
	free(control_ctl->tctl);
	dealloc_mhd_restart_control_c(control_ctl->mrst_ctl);
	free(control_ctl->mrst_ctl);
	dealloc_mhd_evo_scheme_control_c(control_ctl->mevo_ctl);
	free(control_ctl->mevo_ctl);
	return;
}


void alloc_visualization_controls_c(struct visualization_controls_c *viz_ctls){
	viz_ctls->num_section_controls = (int *)calloc(1, sizeof(int));
	viz_ctls->num_isosurf_controls = (int *)calloc(1, sizeof(int));
	viz_ctls->num_volume_rendering_controls = (int *)calloc(1, sizeof(int));
	viz_ctls->num_fieldline_controls = (int *)calloc(1, sizeof(int));
	viz_ctls->num_lic_rendering_controls = (int *)calloc(1, sizeof(int));
	return;
}

void alloc_section_controls_c(struct visualization_controls_c *viz_ctls){
	int i, num;
	num = *viz_ctls->num_section_controls;
	viz_ctls->psf_ctls = (struct section_controls_c **) malloc(num*sizeof(struct section_controls_c *));
	
	for(i=0;i<num;i++){
		viz_ctls->psf_ctls[i] = (struct section_controls_c *) malloc(sizeof(struct section_controls_c));
		viz_ctls->psf_ctls[i]->ctl_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	};
	return;
}
void alloc_isosurf_controls_c(struct visualization_controls_c *viz_ctls){
	int i, num;
	num = *viz_ctls->num_isosurf_controls;
	viz_ctls->iso_ctls = (struct isosurf_controls_c **) malloc(num*sizeof(struct isosurf_controls_c *));
	
	for(i=0;i<num;i++){
		viz_ctls->iso_ctls[i] = (struct isosurf_controls_c *) malloc(sizeof(struct isosurf_controls_c));
		viz_ctls->iso_ctls[i]->ctl_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	};
	return;
}
void alloc_volume_rendering_controls_c(struct visualization_controls_c *viz_ctls){
	int i, num;
	num = *viz_ctls->num_volume_rendering_controls;
	viz_ctls->pvr_ctls = (struct volume_rendering_controls_c **) malloc(num*sizeof(struct volume_rendering_controls_c *));
	
	for(i=0;i<num;i++){
		viz_ctls->pvr_ctls[i] = (struct volume_rendering_controls_c *) malloc(sizeof(struct volume_rendering_controls_c));
		viz_ctls->pvr_ctls[i]->ctl_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	};
	return;
}
void alloc_fieldline_controls_c(struct visualization_controls_c *viz_ctls){
	int i, num;
	num = *viz_ctls->num_fieldline_controls;
	viz_ctls->fline_ctls = (struct fieldline_controls_c **) malloc(num*sizeof(struct fieldline_controls_c *));
	
	for(i=0;i<num;i++){
		viz_ctls->fline_ctls[i] = (struct fieldline_controls_c *) malloc(sizeof(struct fieldline_controls_c));
		viz_ctls->fline_ctls[i]->ctl_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	};
	return;
}
void alloc_lic_rendering_controls_c(struct visualization_controls_c *viz_ctls){
	int i, num;
	num = *viz_ctls->num_lic_rendering_controls;
	viz_ctls->lic_ctls = (struct lic_rendering_controls_c **) malloc(num*sizeof(struct lic_rendering_controls_c *));
	
	for(i=0;i<num;i++){
		viz_ctls->lic_ctls[i] = (struct lic_rendering_controls_c *) malloc(sizeof(struct lic_rendering_controls_c));
		viz_ctls->lic_ctls[i]->ctl_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	};
	return;
}

void dealloc_visualization_controls_c(struct visualization_controls_c *viz_ctls){
	int i;
	
	for(i=0;i<*viz_ctls->num_section_controls;i++){
		free(viz_ctls->psf_ctls[i]->ctl_file_name);
		free(viz_ctls->psf_ctls[i]);
	};
	for(i=0;i<*viz_ctls->num_isosurf_controls;i++){
		free(viz_ctls->iso_ctls[i]->ctl_file_name);
		free(viz_ctls->iso_ctls[i]);
	};
	for(i=0;i<*viz_ctls->num_volume_rendering_controls;i++){
		free(viz_ctls->pvr_ctls[i]->ctl_file_name);
		free(viz_ctls->pvr_ctls[i]);
	};
	for(i=0;i<*viz_ctls->num_fieldline_controls;i++){
		free(viz_ctls->fline_ctls[i]->ctl_file_name);
		free(viz_ctls->fline_ctls[i]);
	};
	for(i=0;i<*viz_ctls->num_lic_rendering_controls;i++){
		free(viz_ctls->lic_ctls[i]->ctl_file_name);
		free(viz_ctls->lic_ctls[i]);
	};
	
	free(viz_ctls->num_section_controls);
	free(viz_ctls->num_isosurf_controls);
	free(viz_ctls->num_volume_rendering_controls);
	free(viz_ctls->num_fieldline_controls);
	free(viz_ctls->num_lic_rendering_controls);
	
	free(viz_ctls->psf_ctls);
	free(viz_ctls->iso_ctls);
	free(viz_ctls->pvr_ctls);
	free(viz_ctls->fline_ctls);
	free(viz_ctls->lic_ctls);
	return;
}

void alloc_sph_zonal_means_controls_c(struct sph_zonal_means_controls_c *zm_ctls){
	zm_ctls->iflag_zmean_section_controls = (int *)calloc(1, sizeof(int));
	zm_ctls->zmean_psf_ctls = (struct section_controls_c *) malloc(sizeof(struct section_controls_c));
	zm_ctls->zmean_psf_ctls->ctl_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	
	zm_ctls->iflag_zrms_section_controls = (int *)calloc(1, sizeof(int));
	zm_ctls->zrms_psf_ctls = (struct section_controls_c *) malloc(sizeof(struct section_controls_c));
	zm_ctls->zrms_psf_ctls->ctl_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	return;
}

void dealloc_sph_zonal_means_controls_c(struct sph_zonal_means_controls_c *zm_ctls){
	free(zm_ctls->iflag_zmean_section_controls);
	free(zm_ctls->iflag_zrms_section_controls);
	
	free(zm_ctls->zmean_psf_ctls->ctl_file_name);
	free(zm_ctls->zmean_psf_ctls);
	free(zm_ctls->zrms_psf_ctls->ctl_file_name);
	free(zm_ctls->zrms_psf_ctls);
	return;
}



void alloc_SGS_MHD_control_c(struct SGS_MHD_control_c *mhd_ctl){
	mhd_ctl->iflag_data_files_def = (int *)calloc(1, sizeof(int));
	mhd_ctl->iflag_org_files_def = (int *)calloc(1, sizeof(int));
	mhd_ctl->iflag_new_files_def = (int *)calloc(1, sizeof(int));
	mhd_ctl->iflag_spherical_shell_ctl = (int *)calloc(1, sizeof(int));
	mhd_ctl->ifile_spherical_shell_ctl = (int *)calloc(1, sizeof(int));
	mhd_ctl->iflag_model = (int *)calloc(1, sizeof(int));
	mhd_ctl->iflag_control = (int *)calloc(1, sizeof(int));
	mhd_ctl->iflag_sph_monitor_ctl = (int *)calloc(1, sizeof(int));
	mhd_ctl->iflag_node_monitor_ctl = (int *)calloc(1, sizeof(int));
	mhd_ctl->iflag_visual_control = (int *)calloc(1, sizeof(int));
	mhd_ctl->iflag_zonal_mean_control = (int *)calloc(1, sizeof(int));
	
	mhd_ctl->files = (struct platform_data_control_c *) malloc(sizeof(struct platform_data_control_c));
	alloc_platform_data_control_c(mhd_ctl->files);
	
	mhd_ctl->org_files = (struct platform_data_control_c *) malloc(sizeof(struct platform_data_control_c));
	alloc_platform_data_control_c(mhd_ctl->org_files);
	
	mhd_ctl->new_files = (struct platform_data_control_c *) malloc(sizeof(struct platform_data_control_c));
	alloc_platform_data_control_c(mhd_ctl->new_files);
	
	mhd_ctl->shell_ctl = (struct parallel_sph_shell_control_c *) malloc(sizeof(struct parallel_sph_shell_control_c));
	alloc_parallel_sph_shell_control_c(mhd_ctl->shell_ctl);
	
	mhd_ctl->model_ctl = (struct mhd_model_control_c *) malloc(sizeof(struct mhd_model_control_c));
	alloc_mhd_model_control_c(mhd_ctl->model_ctl);
	mhd_ctl->control_ctl = (struct sph_mhd_control_control_c *) malloc(sizeof(struct sph_mhd_control_control_c));
	alloc_sph_mhd_control_control_c(mhd_ctl->control_ctl);
	
	mhd_ctl->monitor_ctl = (struct sph_monitor_control_c *) malloc(sizeof(struct sph_monitor_control_c));
	alloc_sph_monitor_ctl_c(mhd_ctl->monitor_ctl);
	mhd_ctl->node_monitor_ctl = (struct node_monitor_control_c *) malloc(sizeof(struct node_monitor_control_c));
	
	mhd_ctl->viz_ctls = (struct visualization_controls_c *) malloc(sizeof(struct visualization_controls_c));
	alloc_visualization_controls_c(mhd_ctl->viz_ctls);
	mhd_ctl->zm_ctls = (struct sph_zonal_means_controls_c *) malloc(sizeof(struct sph_zonal_means_controls_c));
	alloc_sph_zonal_means_controls_c(mhd_ctl->zm_ctls);
	return;
}

void dealloc_SGS_MHD_control_c(struct SGS_MHD_control_c *mhd_ctl){
	free(mhd_ctl->iflag_zonal_mean_control);
	free(mhd_ctl->iflag_visual_control);
	free(mhd_ctl->iflag_node_monitor_ctl);
	free(mhd_ctl->iflag_sph_monitor_ctl);
	free(mhd_ctl->iflag_control);
	free(mhd_ctl->iflag_model);
	free(mhd_ctl->ifile_spherical_shell_ctl);
	free(mhd_ctl->iflag_spherical_shell_ctl);
	free(mhd_ctl->iflag_new_files_def);
	free(mhd_ctl->iflag_org_files_def);
	free(mhd_ctl->iflag_data_files_def);
	
	dealloc_sph_zonal_means_controls_c(mhd_ctl->zm_ctls);
	free(mhd_ctl->zm_ctls);
	dealloc_visualization_controls_c(mhd_ctl->viz_ctls);
	free(mhd_ctl->viz_ctls);
	dealloc_sph_monitor_ctl_c(mhd_ctl->monitor_ctl);
	free(mhd_ctl->monitor_ctl);
	free(mhd_ctl->node_monitor_ctl);
	dealloc_sph_mhd_control_control_c(mhd_ctl->control_ctl);
	free(mhd_ctl->control_ctl);
	dealloc_mhd_model_control_c(mhd_ctl->model_ctl);
	free(mhd_ctl->model_ctl);
	dealloc_parallel_sph_shell_control_c(mhd_ctl->shell_ctl);
	free(mhd_ctl->shell_ctl);
	alloc_platform_data_control_c(mhd_ctl->new_files);
	free(mhd_ctl->new_files);
	alloc_platform_data_control_c(mhd_ctl->org_files);
	free(mhd_ctl->org_files);
	alloc_platform_data_control_c(mhd_ctl->files);
	free(mhd_ctl->files);
	return;
}

