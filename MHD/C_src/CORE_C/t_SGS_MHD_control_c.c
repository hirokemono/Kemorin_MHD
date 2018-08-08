/*
//  t_SGS_MHD_control_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/05/15.
*/

#include "t_SGS_MHD_control_c.h"



void alloc_parallel_sph_shell_control_c(struct parallel_sph_shell_control_c *shell_ctl){
	shell_ctl->Fmesh_ctl = (struct FEM_mesh_control_c *) malloc(sizeof(struct FEM_mesh_control_c));
	alloc_FEM_mesh_control_c(shell_ctl->Fmesh_ctl);
	
	shell_ctl->sdctl_c = (struct sphere_domain_ctl_c *) malloc(sizeof(struct sphere_domain_ctl_c));
	alloc_sphere_domain_ctl_c(shell_ctl->sdctl_c);
	shell_ctl->spctl_c = (struct sphere_data_ctl_c *) malloc(sizeof(struct sphere_data_ctl_c));
	alloc_sphere_data_ctl_c(shell_ctl->spctl_c);
	
	return;
}

void dealloc_parallel_sph_shell_control_c(struct parallel_sph_shell_control_c *shell_ctl){
	shell_ctl->iflag_FEM_mesh_ctl = 0;
	shell_ctl->iflag_sph_domain =   0;
	shell_ctl->iflag_sph_shell =    0;
	
	dealloc_FEM_mesh_control_c(shell_ctl->Fmesh_ctl);
	free(shell_ctl->Fmesh_ctl);
	dealloc_sphere_domain_ctl_c(shell_ctl->sdctl_c);
	free(shell_ctl->sdctl_c);
	dealloc_sphere_data_ctl_c(shell_ctl->spctl_c);
	free(shell_ctl->spctl_c);
	return;
}

void alloc_mhd_model_control_c(struct mhd_model_control_c *model_ctl){
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

void alloc_sph_mhd_control_control_c(struct sph_mhd_control_control_c *control_ctl){
	control_ctl->tctl = (struct time_data_control_c *) malloc(sizeof(struct time_data_control_c));
	alloc_time_data_control_c(control_ctl->tctl);
	control_ctl->mrst_ctl = (struct mhd_restart_control_c *) malloc(sizeof(struct mhd_restart_control_c));
	alloc_mhd_restart_control_c(control_ctl->mrst_ctl);
	control_ctl->mevo_ctl = (struct mhd_evo_scheme_control_c *) malloc(sizeof(struct mhd_evo_scheme_control_c));
	alloc_mhd_evo_scheme_control_c(control_ctl->mevo_ctl);
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


void alloc_sph_zonal_means_controls_c(struct sph_zonal_means_controls_c *zm_ctls){
	zm_ctls->iflag_zmean_section_controls = 0;
	zm_ctls->zmean_psf_ctls = (struct section_controls_c *) malloc(sizeof(struct section_controls_c));
	zm_ctls->zmean_psf_ctls->ctl_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	
	zm_ctls->iflag_zrms_section_controls = 0;
	zm_ctls->zrms_psf_ctls = (struct section_controls_c *) malloc(sizeof(struct section_controls_c));
	zm_ctls->zrms_psf_ctls->ctl_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	return;
}

void dealloc_sph_zonal_means_controls_c(struct sph_zonal_means_controls_c *zm_ctls){
    zm_ctls->iflag_zmean_section_controls = 0;
    zm_ctls->iflag_zrms_section_controls =  0;
	
	free(zm_ctls->zmean_psf_ctls->ctl_file_name);
	free(zm_ctls->zmean_psf_ctls);
	free(zm_ctls->zrms_psf_ctls->ctl_file_name);
	free(zm_ctls->zrms_psf_ctls);
	return;
}



void alloc_SGS_MHD_control_c(struct SGS_MHD_control_c *mhd_ctl){
	mhd_ctl->iflag_data_files_def =      0;
	mhd_ctl->iflag_org_files_def =       0;
	mhd_ctl->iflag_new_files_def =       0;
	mhd_ctl->iflag_spherical_shell_ctl = 0;
	mhd_ctl->ifile_spherical_shell_ctl = 0;
	mhd_ctl->iflag_model =               0;
	mhd_ctl->iflag_control =             0;
    mhd_ctl->iflag_sph_monitor_ctl =     0;
	mhd_ctl->iflag_node_monitor_ctl =    0;
	mhd_ctl->iflag_visual_control =      0;
	mhd_ctl->iflag_zonal_mean_control =  0;
	
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
	
	mhd_ctl->viz_c = (struct visualizers_ctl_c *) malloc(sizeof(struct visualizers_ctl_c));
	alloc_vizs_ctl_c(mhd_ctl->viz_c);
	mhd_ctl->zm_ctls = (struct sph_zonal_means_controls_c *) malloc(sizeof(struct sph_zonal_means_controls_c));
	alloc_sph_zonal_means_controls_c(mhd_ctl->zm_ctls);
	return;
}

void dealloc_SGS_MHD_control_c(struct SGS_MHD_control_c *mhd_ctl){
    mhd_ctl->iflag_zonal_mean_control =  0;
    mhd_ctl->iflag_visual_control =      0;
    mhd_ctl->iflag_node_monitor_ctl =    0;
    mhd_ctl->iflag_sph_monitor_ctl =     0;
    mhd_ctl->iflag_control =             0;
    mhd_ctl->iflag_model =               0;
    mhd_ctl->ifile_spherical_shell_ctl = 0;
    mhd_ctl->iflag_spherical_shell_ctl = 0;
    mhd_ctl->iflag_new_files_def =       0;
    mhd_ctl->iflag_org_files_def =       0;
    mhd_ctl->iflag_data_files_def =      0;
	
	dealloc_sph_zonal_means_controls_c(mhd_ctl->zm_ctls);
	free(mhd_ctl->zm_ctls);
	dealloc_vizs_ctl_c(mhd_ctl->viz_c);
	free(mhd_ctl->viz_c);
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

