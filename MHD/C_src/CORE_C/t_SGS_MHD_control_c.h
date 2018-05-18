/*
//  t_SGS_MHD_control_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/05/15.
*/

#ifndef t_SGS_MHD_control_c_h_
#define t_SGS_MHD_control_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "t_ctl_data_4_platforms_c.h"
#include "t_ctl_data_4_time_steps_c.h"


struct sphere_data_control_c{
	int iflag;
};
struct sphere_domain_control_c{
	int iflag;
};

struct field_control_c{
	int iflag;
};
struct mhd_evolution_control_c{
	int iflag;
};
struct mhd_evo_area_control_c{
	int iflag;
};
struct node_bc_control_c{
	int iflag;
};
struct surf_bc_control_c{
	int iflag;
};
struct dimless_control_c{
	int iflag;
};
struct equations_control_c{
	int iflag;
};
struct forces_control_c{
	int iflag;
};
struct gravity_control_c{
	int iflag;
};
struct coriolis_control_c{
	int iflag;
};
struct magneto_convection_control_c{
	int iflag;
};
struct reference_temperature_c{
	int iflag;
};
struct sgs_model_control_c{
	int iflag;
};

struct mhd_restart_control_c{
	int iflag;
};
struct mhd_evo_scheme_control_c{
	int iflag;
};

struct section_controls_c{
	char *ctl_file_name;
};
struct isosurf_controls_c{
	char *ctl_file_name;
};
struct volume_rendering_controls_c{
	char *ctl_file_name;
};
struct lic_rendering_controls_c{
	char *ctl_file_name;
};
struct fieldline_controls_c{
	char *ctl_file_name;
};

struct parallel_sph_shell_control_c{
	int *iflag_sph_shell;
	struct sphere_data_control_c *sp_ctl;
	int *iflag_sph_domain;
	struct sphere_domain_control_c *sd_ctl;
};
struct mhd_model_control_c{
	int *iflag_field_control;
	struct field_control_c *fld_ctl;
	int *iflag_mhd_evolution_control;
	struct mhd_evolution_control_c *evo_ctl;
	int *iflag_mhd_evo_area_control;
	struct mhd_evo_area_control_c *earea_ctl;
	int *iflag_node_bc_control;
	struct node_bc_control_c *nbc_ctl;
	int *iflag_surf_bc_control;
	struct surf_bc_control_c *sbc_ctl;
	int *iflag_forces_control;
	struct forces_control_c *frc_ctl;
	int *iflag_dimless_control;
	struct dimless_control_c *dless_ctl;
	int *iflag_equations_control;
	struct equations_control_c *eqs_ctl;
	int *iflag_gravity_control;
	struct gravity_control_c *g_ctl;
	int *iflag_coriolis_control;
	struct coriolis_control_c *cor_ctl;
	int *iflag_magneto_convection_control;
	struct magneto_convection_control_c *mcv_ctl;
	int *iflag_reference_temp_control;
	struct reference_temperature_c *reft_ctl;
	int *iflag_reference_comp_control;
	struct reference_temperature_c *refc_ctl;
	int *iflag_sgs_model_control;
	struct sgs_model_control_c *sgs_ctl;
};
struct sph_mhd_control_control_c{
	int *iflag_time_data_control;
	struct time_data_control_c *tctl;
	int *iflag_mhd_restart_control;
	struct mhd_restart_control_c *mrst_ctl;
	int *iflag_mhd_evo_scheme_control;
	struct mhd_evo_scheme_control_c *mevo_ctl;
};
struct sph_monitor_control_c{
	int iflag;
};
struct node_monitor_control_c{
	int iflag;
};
struct visualization_controls_c{
	int *num_section_controls;
	struct section_controls_c **psf_ctls;
	int *num_isosurf_controls;
	struct isosurf_controls_c **iso_ctls;
	int *num_volume_rendering_controls;
	struct volume_rendering_controls_c **pvr_ctls;
	int *num_lic_rendering_controls;
	struct lic_rendering_controls_c **lic_ctls;
	int *num_fieldline_controls;
	struct fieldline_controls_c **fline_ctls;
};
struct sph_zonal_means_controls_c{
	int *iflag_zmean_section_controls;
	struct section_controls_c *zmean_psf_ctls;
	int *iflag_zrms_section_controls;
	struct section_controls_c *zrms_psf_ctls;
};

struct SGS_MHD_control_c{
	int *iflag_data_files_def;
	struct platform_data_control_c *files;
	int *iflag_org_files_def;
	struct platform_data_control_c *org_files;
	int *iflag_new_files_def;
	struct platform_data_control_c *new_files;
	
	int *iflag_spherical_shell_ctl;
	int *ifile_spherical_shell_ctl;
	struct parallel_sph_shell_control_c *shell_ctl;
	
	int *iflag_model;
	struct mhd_model_control_c *model_ctl;
	int *iflag_control;
	struct sph_mhd_control_control_c *control_ctl;
	
	int *iflag_sph_monitor_ctl;
	struct sph_monitor_control_c *monitor_ctl;
	int *iflag_node_monitor_ctl;
	struct node_monitor_control_c *node_monitor_ctl;
	
	int *iflag_visual_control;
	struct visualization_controls_c *viz_ctls;
	int *iflag_zonal_mean_control;
	struct sph_zonal_means_controls_c *zm_ctls;
};

/* Prototypes */

void alloc_parallel_sph_shell_control_c(struct parallel_sph_shell_control_c *shell_ctl);
void dealloc_parallel_sph_shell_control_c(struct parallel_sph_shell_control_c *shell_ctl);

void alloc_mhd_model_control_c(struct mhd_model_control_c *model_ctl);
void dealloc_mhd_model_control_c(struct mhd_model_control_c *model_ctl);

void alloc_sph_mhd_control_control_c(struct sph_mhd_control_control_c *control_ctl);
void dealloc_sph_mhd_control_control_c(struct sph_mhd_control_control_c *control_ctl);

void alloc_visualization_controls_c(struct visualization_controls_c *viz_ctls);
void alloc_section_controls_c(struct visualization_controls_c *viz_ctls);
void alloc_isosurf_controls_c(struct visualization_controls_c *viz_ctls);
void alloc_volume_rendering_controls_c(struct visualization_controls_c *viz_ctls);
void alloc_fieldline_controls_c(struct visualization_controls_c *viz_ctls);
void alloc_lic_rendering_controls_c(struct visualization_controls_c *viz_ctls);
void dealloc_visualization_controls_c(struct visualization_controls_c *viz_ctls);

void alloc_sph_zonal_means_controls_c(struct sph_zonal_means_controls_c *zm_ctls);
void dealloc_sph_zonal_means_controls_c(struct sph_zonal_means_controls_c *zm_ctls);

void alloc_SGS_MHD_control_c(struct SGS_MHD_control_c *mhd_ctl);
void dealloc_SGS_MHD_control_c(struct SGS_MHD_control_c *mhd_ctl);


#endif /* t_SGS_MHD_control_c_h */
