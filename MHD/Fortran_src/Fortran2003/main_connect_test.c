#include <stdlib.h>
#include <stdio.h>

#include "all_field_names_c.h"
#include "control_elements_IO_c.h"
#include "t_ctl_data_4_platforms_c.h"
#include "t_ctl_data_4_time_steps_c.h"
#include "t_SGS_MHD_control_c.h"

FILE *fp;

extern void c_read_control_sph_SGS_MHD();
extern void c_write_control_sph_SGS_MHD();

struct field_control{
	struct all_field_def *fld_def;
	int *iflag_use;
	int *iflag_viz;
	int *iflag_monitor;
};

struct field_control *fld_ctl;

int read_section_controls_c(FILE *fp, char buf[LENGTHBUF], const char *label, struct visualization_controls_c *viz_ctls){
	int icou = 0;
	printf("alloc_section_controls_c %d\n", *viz_ctls->num_section_controls);
	alloc_section_controls_c(viz_ctls);
	while(find_control_end_array_flag_c(buf, label, *viz_ctls->num_section_controls, icou) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		icou = icou + right_file_flag_c(buf, label, viz_ctls->psf_ctls[icou]->ctl_file_name);
		if(right_begin_flag_c(buf, label) > 0){
			viz_ctls->psf_ctls[icou]->ctl_file_name = "NO_FILE";
			icou = icou + 1;
		};
		
		printf("Block read_section_controls_c %d %s\n", icou, buf);
	};
	return 1;
}
int read_isosurf_controls_c(FILE *fp, char buf[LENGTHBUF], const char *label, struct visualization_controls_c *viz_ctls){
	int icou = 0;
	printf("alloc_isosurf_controls_c %d\n",*viz_ctls->num_isosurf_controls);
	alloc_isosurf_controls_c(viz_ctls);
	while(find_control_end_array_flag_c(buf, label, *viz_ctls->num_isosurf_controls, icou) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		icou = icou + right_file_flag_c(buf, label, viz_ctls->iso_ctls[icou]->ctl_file_name);
		if(right_begin_flag_c(buf, label) > 0){
			viz_ctls->iso_ctls[icou]->ctl_file_name = "NO_FILE";
			icou = icou + 1;
		};
		
		printf("Block read_isosurf_controls_c %d %s\n", icou, buf);
	};
	return 1;
}
int read_volume_rendering_controls_c(FILE *fp, char buf[LENGTHBUF], const char *label, struct visualization_controls_c *viz_ctls){
	int icou = 0;
	printf("alloc_volume_rendering_controls_c %d\n",*viz_ctls->num_volume_rendering_controls);
	alloc_volume_rendering_controls_c(viz_ctls);
	while(find_control_end_array_flag_c(buf, label, *viz_ctls->num_volume_rendering_controls, icou) == 0){
		fgets(buf, LENGTHBUF, fp);
		printf("Block read_volume_rendering_controls_c %d %s\n", icou, buf);
		
		icou = icou + right_file_flag_c(buf, label, viz_ctls->pvr_ctls[icou]->ctl_file_name);
		if(right_begin_flag_c(buf, label) > 0){
			viz_ctls->pvr_ctls[icou]->ctl_file_name = "NO_FILE";
			icou = icou + 1;
		};
		
	};
	return 1;
}
int read_lic_rendering_controls_c(FILE *fp, char buf[LENGTHBUF], const char *label, struct visualization_controls_c *viz_ctls){
	int icou = 0;
	alloc_lic_rendering_controls_c(viz_ctls);
	while(find_control_end_array_flag_c(buf, "LIC_rendering", *viz_ctls->num_lic_rendering_controls, icou) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		icou = icou + right_file_flag_c(buf, label, viz_ctls->lic_ctls[icou]->ctl_file_name);
		if(right_begin_flag_c(buf, label) > 0){
			viz_ctls->lic_ctls[icou]->ctl_file_name = "NO_FILE";
			icou = icou + 1;
		};
		
		printf("Block read_lic_rendering_controls_c %d %s\n", icou, buf);
	};
	return 1;
}
int read_fieldline_controls_c(FILE *fp, char buf[LENGTHBUF], const char *label, struct visualization_controls_c *viz_ctls){
	int icou = 0;
	printf("alloc_fieldline_controls_c %d\n",*viz_ctls->num_fieldline_controls);
	alloc_fieldline_controls_c(viz_ctls);
	while(find_control_end_array_flag_c(buf, "fieldline", *viz_ctls->num_fieldline_controls, icou) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		icou = icou + right_file_flag_c(buf, label, viz_ctls->fline_ctls[icou]->ctl_file_name);
		if(right_begin_flag_c(buf, label) > 0){
			viz_ctls->fline_ctls[icou]->ctl_file_name = "NO_FILE";
			icou = icou + 1;
		};
		
		printf("Block read_fieldline_controls_c %d %s\n", icou, buf);
	};
	return 1;
}


int read_spherical_shell_ctl_c(FILE *fp, char buf[LENGTHBUF], struct parallel_sph_shell_control_c *shell_ctl){
	while(find_control_end_flag_c(buf, "spherical_shell_ctl") == 0){
		fgets(buf, LENGTHBUF, fp);
		
		if(right_begin_flag_c(buf, "FEM_mesh_ctl") > 0){
			*shell_ctl->iflag_FEM_mesh_ctl = read_FEM_mesh_control_c(fp, buf,
						"FEM_mesh_ctl", shell_ctl->Fmesh_ctl);
		};
		if(right_begin_flag_c(buf, "num_domain_ctl") > 0){
			*shell_ctl->iflag_sph_domain = read_sphere_domain_ctl_c(fp, buf,
						"num_domain_ctl", shell_ctl->sdctl_c);
		};
		if(right_begin_flag_c(buf, "num_grid_sph") > 0){
			*shell_ctl->iflag_sph_shell = read_sphere_data_ctl_c(fp, buf,
						"num_grid_sph", shell_ctl->spctl_c);
		};
	};
	return 1;
}
int read_mhd_model_ctl_c(FILE *fp, char buf[LENGTHBUF], struct mhd_model_control_c *model_ctl){
	while(find_control_end_flag_c(buf, "model") == 0){
		fgets(buf, LENGTHBUF, fp);
		
		if(right_begin_flag_c(buf, "phys_values_ctl") > 0) {
			*model_ctl->iflag_field_control = read_field_ctl_c(fp, buf, 
						"phys_values_ctl", model_ctl->fld_ctl);
		};
		if(right_begin_flag_c(buf, "time_evolution_ctl") > 0) {
			*model_ctl->iflag_mhd_evolution_control = read_mhd_evolution_ctl_c(fp, buf,
						"time_evolution_ctl", model_ctl->evo_ctl);
		};
		if(right_begin_flag_c(buf, "layers_ctl") > 0) {
			*model_ctl->iflag_mhd_evo_area_control = read_mhd_evo_area_ctl_c(fp, buf,
						"layers_ctl", model_ctl->earea_ctl);
		};
		if(right_begin_flag_c(buf, "boundary_condition") > 0) {
			*model_ctl->iflag_node_bc_control = read_MHD_node_bc_ctl_c(fp, buf, 
						"boundary_condition", model_ctl->nbc_ctl);
		};
		if(right_begin_flag_c(buf, "bc_4_node") > 0) {
			*model_ctl->iflag_node_bc_control = read_MHD_node_bc_ctl_c(fp, buf, 
						"bc_4_node", model_ctl->nbc_ctl);
		};
		if(right_begin_flag_c(buf, "bc_4_surface") > 0) {
			*model_ctl->iflag_surf_bc_control = read_MHD_surf_bc_ctl_c(fp, buf, 
						"bc_4_surface", model_ctl->sbc_ctl);
		};
		if(right_begin_flag_c(buf, "forces_define") > 0) {
			*model_ctl->iflag_forces_control = read_forces_ctl_c(fp, buf,
						"forces_define", model_ctl->frc_ctl);
		};
		if(right_begin_flag_c(buf, "dimensionless_ctl") > 0) {
			*model_ctl->iflag_dimless_control = read_dimless_ctl_c(fp, buf, 
						"dimensionless_ctl", model_ctl->dless_ctl);
		};
		if(right_begin_flag_c(buf, "coefficients_ctl") > 0) {
			*model_ctl->iflag_equations_control = read_equations_ctl_c(fp, buf, 
						"coefficients_ctl", model_ctl->eqs_ctl);
		};
		if(right_begin_flag_c(buf, "gravity_define") > 0) {
			*model_ctl->iflag_gravity_control = read_gravity_ctl_c(fp, buf,
						"gravity_define", model_ctl->g_ctl);
		};
		if(right_begin_flag_c(buf, "Coriolis_define") > 0) {
			*model_ctl->iflag_coriolis_control = read_coriolis_ctl_c(fp, buf,
						"Coriolis_define", model_ctl->cor_ctl);
		};
		if(right_begin_flag_c(buf, "Magneto_convection_def") > 0) {
			*model_ctl->iflag_magneto_convection_control = read_magneto_cv_ctl_c(fp, buf, 
						"Magneto_convection_def", model_ctl->mcv_ctl);
		};
		if(right_begin_flag_c(buf, "temperature_define") > 0) {
			*model_ctl->iflag_reference_temp_control = read_ref_temperature_ctl_c(fp, buf, 
						"temperature_define", model_ctl->reft_ctl);
		};
		if(right_begin_flag_c(buf, "composition_define") > 0) {
			*model_ctl->iflag_reference_comp_control = read_ref_composition_ctl_c(fp, buf,
						"composition_define", model_ctl->refc_ctl);
		};
		if(right_begin_flag_c(buf, "SGS_control") > 0) {
			*model_ctl->iflag_sgs_model_control = read_SGS_model_ctl_c(fp, buf, "SGS_control", 
						model_ctl->sgs_ctl);
		};
	};
	return 1;
}
int read_mhd_control_ctl_c(FILE *fp, char buf[LENGTHBUF], struct sph_mhd_control_control_c *control_ctl){
	while(find_control_end_flag_c(buf, "control") == 0){
		fgets(buf, LENGTHBUF, fp);
		
		if(right_begin_flag_c(buf, "time_step_ctl") > 0)
			*control_ctl->iflag_time_data_control = read_time_data_control_c(fp, buf, "time_step_ctl", control_ctl->tctl);
		if(right_begin_flag_c(buf, "restart_file_ctl") > 0) 
			*control_ctl->iflag_mhd_restart_control = read_mhd_restart_control_c(fp, buf, "restart_file_ctl", control_ctl->mrst_ctl);
		if(right_begin_flag_c(buf, "time_loop_ctl") > 0) 
			*control_ctl->iflag_mhd_evo_scheme_control = read_mhd_evo_scheme_control_c(fp, buf, "time_loop_ctl", control_ctl->mevo_ctl);
	};
	return 1;
}
int read_zonal_mean_control_c(FILE *fp, char buf[LENGTHBUF], struct sph_zonal_means_controls_c *zm_ctls){
	while(find_control_end_flag_c(buf, "zonal_mean_control") == 0){
		fgets(buf, LENGTHBUF, fp);
		
		if(right_file_flag_c(buf, "zonal_mean_section_ctl", zm_ctls->zmean_psf_ctls->ctl_file_name) > 0){
			*zm_ctls->iflag_zmean_section_controls = 1;
		};
		if(right_begin_flag_c(buf, "zonal_mean_section_ctl") > 0){
			zm_ctls->zmean_psf_ctls->ctl_file_name = "NO_FILE";
			*zm_ctls->iflag_zmean_section_controls = 1;
		};
		
		if(right_file_flag_c(buf, "zonal_RMS_section_ctl", zm_ctls->zrms_psf_ctls->ctl_file_name) > 0){
			*zm_ctls->iflag_zrms_section_controls = 1;
		};
		if(right_begin_flag_c(buf, "zonal_RMS_section_ctl") > 0){
			zm_ctls->zrms_psf_ctls->ctl_file_name = "NO_FILE";
			*zm_ctls->iflag_zrms_section_controls = 1;
		};
	};
	return 1;
}
int read_visual_control_c(FILE *fp, char buf[LENGTHBUF], struct visualization_controls_c *viz_ctls){
	int iflag;
	while(find_control_end_flag_c(buf, "visual_control") == 0){
		fgets(buf, LENGTHBUF, fp);
		
		iflag = find_control_array_flag_c(buf, "cross_section_ctl", viz_ctls->num_section_controls);
		printf("*viz_ctls->num_section_controls %d\n", *viz_ctls->num_section_controls);
		if(iflag > 0) iflag = read_section_controls_c(fp, buf, "cross_section_ctl", viz_ctls);
		
		iflag = find_control_array_flag_c(buf, "isosurface_ctl", viz_ctls->num_isosurf_controls);
		printf("*viz_ctls->num_isosurf_controls %d\n", *viz_ctls->num_isosurf_controls);
		if(iflag > 0) iflag = read_isosurf_controls_c(fp, buf, "isosurface_ctl", viz_ctls);
		
		iflag = find_control_array_flag_c(buf, "volume_rendering", viz_ctls->num_volume_rendering_controls);
		printf("*viz_ctls->num_volume_rendering_controls %d\n", *viz_ctls->num_volume_rendering_controls);
		if(iflag > 0) iflag = read_volume_rendering_controls_c(fp, buf, "volume_rendering", viz_ctls);
		
		iflag = find_control_array_flag_c(buf, "LIC_rendering", viz_ctls->num_lic_rendering_controls);
		printf("*viz_ctls->num_lic_rendering_controls %d\n", *viz_ctls->num_lic_rendering_controls);
		if(iflag > 0) iflag = read_lic_rendering_controls_c(fp, buf, "LIC_rendering", viz_ctls);
		
		iflag = find_control_array_flag_c(buf, "fieldline", viz_ctls->num_fieldline_controls);
		printf("*viz_ctls->num_fieldline_controls %d\n", *viz_ctls->num_fieldline_controls);
		if(iflag > 0) iflag = read_fieldline_controls_c(fp, buf, "fieldline", viz_ctls);
		
		iflag = find_control_array_flag_c(buf, "surface_rendering", viz_ctls->num_section_controls);
		printf("*viz_ctls->num_section_controls %d\n", *viz_ctls->num_section_controls);
		if(iflag > 0) iflag = read_section_controls_c(fp, buf, "surface_rendering", viz_ctls);
		
		iflag = find_control_array_flag_c(buf, "isosurf_rendering", viz_ctls->num_isosurf_controls);
		printf("*viz_ctls->num_isosurf_controls %d\n", *viz_ctls->num_isosurf_controls);
		if(iflag > 0) iflag = read_isosurf_controls_c(fp, buf, "isosurf_rendering", viz_ctls);
	};
	return 1;
}

int write_spherical_shell_ctl_c(FILE *fp, int level, int *iflag, struct parallel_sph_shell_control_c *shell_ctl){
	if(*iflag == 0) return level;
	level = write_begin_flag_for_ctl_c(fp, level, "spherical_shell_ctl");
	
	level = write_FEM_mesh_control_c(fp, level, shell_ctl->iflag_FEM_mesh_ctl,
				"FEM_mesh_ctl", shell_ctl->Fmesh_ctl);
	if(*shell_ctl->iflag_sph_domain > 0) fprintf(fp, "!\n");
	level = write_sphere_domain_ctl_c(fp, level, shell_ctl->iflag_sph_domain,
				"num_domain_ctl", shell_ctl->sdctl_c);
	if(*shell_ctl->iflag_sph_shell > 0) fprintf(fp, "!\n");
	level = write_sphere_data_ctl_c(fp, level, shell_ctl->iflag_sph_shell,
				"num_grid_sph", shell_ctl->spctl_c);
	
	level = write_end_flag_for_ctl_c(fp, level, "spherical_shell_ctl");
	return level;
}

int write_mhd_model_ctl_c(FILE *fp, int level, int *iflag, struct mhd_model_control_c *model_ctl){
	if(*iflag == 0) return level;
	level = write_begin_flag_for_ctl_c(fp, level, "model");
	
	level = write_field_ctl_c(fp, level, model_ctl->iflag_field_control,
				"phys_values_ctl", model_ctl->fld_ctl);
	if(*model_ctl->iflag_mhd_evolution_control > 0) fprintf(fp, "!\n");
	level = write_mhd_evolution_ctl_c(fp, level, model_ctl->iflag_mhd_evolution_control, 
				"time_evolution_ctl", model_ctl->evo_ctl);
	if(*model_ctl->iflag_mhd_evo_area_control > 0) fprintf(fp, "!\n");
	level = write_mhd_evo_area_ctl_c(fp, level, model_ctl->iflag_mhd_evo_area_control, 
				"layers_ctl", model_ctl->earea_ctl);
	if(*model_ctl->iflag_node_bc_control > 0) fprintf(fp, "!\n");
	level = write_MHD_node_bc_ctl_c(fp, level, model_ctl->iflag_node_bc_control, 
				"boundary_condition", model_ctl->nbc_ctl);
	if(*model_ctl->iflag_surf_bc_control > 0) fprintf(fp, "!\n");
	level = write_MHD_surf_bc_ctl_c(fp, level, model_ctl->iflag_surf_bc_control, 
				"bc_4_surface", model_ctl->sbc_ctl);
	if(*model_ctl->iflag_dimless_control > 0) fprintf(fp, "!\n");
	level = write_dimless_ctl_c(fp, level, model_ctl->iflag_dimless_control, 
				"dimensionless_ctl", model_ctl->dless_ctl);
	if(*model_ctl->iflag_equations_control > 0) fprintf(fp, "!\n");
	level = write_equations_ctl_c(fp, level, model_ctl->iflag_equations_control, 
				"coefficients_ctl", model_ctl->eqs_ctl);
	if(*model_ctl->iflag_forces_control > 0) fprintf(fp, "!\n");
	level = write_forces_ctl_c(fp, level, model_ctl->iflag_forces_control, 
				"forces_define", model_ctl->frc_ctl);
	if(*model_ctl->iflag_gravity_control > 0) fprintf(fp, "!\n");
	level = write_gravity_ctl_c(fp, level, model_ctl->iflag_gravity_control, 
				"gravity_define", model_ctl->g_ctl);
	if(*model_ctl->iflag_coriolis_control > 0) fprintf(fp, "!\n");
	level = write_coriolis_ctl_c(fp, level, model_ctl->iflag_coriolis_control, 
				"Coriolis_define", model_ctl->cor_ctl);
	if(*model_ctl->iflag_magneto_convection_control > 0) fprintf(fp, "!\n");
	level = write_magneto_cv_ctl_c(fp, level, model_ctl->iflag_magneto_convection_control, 
				"Magneto_convection_def", model_ctl->mcv_ctl);
	if(*model_ctl->iflag_reference_temp_control > 0) fprintf(fp, "!\n");
	level = write_ref_temperature_ctl_c(fp, level, model_ctl->iflag_reference_temp_control, 
				"temperature_define", model_ctl->reft_ctl);
	if(*model_ctl->iflag_reference_comp_control > 0) fprintf(fp, "!\n");
	level = write_ref_composition_ctl_c(fp, level, model_ctl->iflag_reference_comp_control,
				"composition_define", model_ctl->refc_ctl);
	if(*model_ctl->iflag_sgs_model_control > 0) fprintf(fp, "!\n");
	level = write_SGS_model_ctl_c(fp, level, model_ctl->iflag_sgs_model_control, 
				"SGS_control", model_ctl->sgs_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, "model");
	return level;
}
int write_mhd_control_ctl_c(FILE *fp, int level, int *iflag, struct sph_mhd_control_control_c *control_ctl){
	if(*iflag == 0) return level;
	level = write_begin_flag_for_ctl_c(fp, level, "control");
	
	level = write_time_data_control_c(fp, level, "time_step_ctl", 
                                      control_ctl->iflag_time_data_control, control_ctl->tctl);
	level = write_mhd_restart_control_c(fp, level, *control_ctl->iflag_mhd_restart_control,
				"restart_file_ctl", control_ctl->mrst_ctl);
	level = write_mhd_evo_scheme_control_c(fp, level, *control_ctl->iflag_mhd_evo_scheme_control,
				"time_loop_ctl", control_ctl->mevo_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, "control");
	return level;
}
int write_zonal_mean_control_c(FILE *fp, int level, int *iflag, struct sph_zonal_means_controls_c *zm_ctls){
	if(*iflag == 0) return level;
	level = write_begin_flag_for_ctl_c(fp, level, "zonal_mean_control");
	if(*zm_ctls->iflag_zmean_section_controls > 0){
		if(cmp_no_case_c(zm_ctls->zmean_psf_ctls->ctl_file_name, "NO_FILE") > 0){
			level = write_begin_flag_for_ctl_c(fp, level, "zonal_mean_section_ctl");
			level = write_end_flag_for_ctl_c(fp, level, "zonal_mean_section_ctl");
		} else {
			write_file_flag_for_ctl_c(fp, level, "zonal_mean_section_ctl", zm_ctls->zmean_psf_ctls->ctl_file_name);
		};
	};
	
	if(*zm_ctls->iflag_zrms_section_controls > 0){
		if(cmp_no_case_c(zm_ctls->zrms_psf_ctls->ctl_file_name, "NO_FILE") > 0){
			level = write_begin_flag_for_ctl_c(fp, level, "zonal_RMS_section_ctl");
			level = write_end_flag_for_ctl_c(fp, level, "zonal_RMS_section_ctl");
		} else {
			write_file_flag_for_ctl_c(fp, level, "zonal_RMS_section_ctl", zm_ctls->zrms_psf_ctls->ctl_file_name);
		};
	};
	
	level = write_end_flag_for_ctl_c(fp, level, "zonal_mean_control");
	return level;
}
int write_visual_control_c(FILE *fp, int level, int *iflag, struct visualization_controls_c *viz_ctls){
	int i;
	if(*iflag == 0) return level;
	level = write_begin_flag_for_ctl_c(fp, level, "visual_control");
	
	if(*viz_ctls->num_section_controls > 0){
		level = write_array_flag_for_ctl_c(fp, level, "cross_section_ctl", *viz_ctls->num_section_controls);
		for(i=0;i<*viz_ctls->num_section_controls;i++){
			if(cmp_no_case_c(viz_ctls->psf_ctls[i]->ctl_file_name, "NO_FILE") > 0){
				level = write_begin_flag_for_ctl_c(fp, level, "cross_section_ctl");
				level = write_end_flag_for_ctl_c(fp, level, "cross_section_ctl");
			} else {
				write_file_flag_for_ctl_c(fp, level, "cross_section_ctl", viz_ctls->psf_ctls[i]->ctl_file_name);
			};
		};
		level = write_end_array_flag_for_ctl_c(fp, level, "cross_section_ctl");
	};
	
	if(*viz_ctls->num_isosurf_controls > 0){
		fprintf(fp, "!\n");
		level = write_array_flag_for_ctl_c(fp, level, "isosurface_ctl", *viz_ctls->num_isosurf_controls);
		for(i=0;i<*viz_ctls->num_isosurf_controls;i++){
			if(cmp_no_case_c(viz_ctls->iso_ctls[i]->ctl_file_name, "NO_FILE") > 0){
				level = write_begin_flag_for_ctl_c(fp, level, "isosurface_ctl");
				level = write_end_flag_for_ctl_c(fp, level, "isosurface_ctl");
			} else {
				write_file_flag_for_ctl_c(fp, level, "isosurface_ctl", viz_ctls->iso_ctls[i]->ctl_file_name);
			};
		};
		level = write_end_array_flag_for_ctl_c(fp, level, "isosurface_ctl");
	};
	
	if(*viz_ctls->num_volume_rendering_controls > 0){
		fprintf(fp, "!\n");
		level = write_array_flag_for_ctl_c(fp, level, "volume_rendering", *viz_ctls->num_volume_rendering_controls);
		for(i=0;i<*viz_ctls->num_volume_rendering_controls;i++){
			if(cmp_no_case_c(viz_ctls->pvr_ctls[i]->ctl_file_name, "NO_FILE") > 0){
				level = write_begin_flag_for_ctl_c(fp, level, "volume_rendering");
				level = write_end_flag_for_ctl_c(fp, level, "volume_rendering");
			} else {
				write_file_flag_for_ctl_c(fp, level, "volume_rendering", viz_ctls->pvr_ctls[i]->ctl_file_name);
			};
		};
		level = write_end_array_flag_for_ctl_c(fp, level, "volume_rendering");
	};
	
	if(*viz_ctls->num_lic_rendering_controls > 0){
		fprintf(fp, "!\n");
		level = write_array_flag_for_ctl_c(fp, level, "LIC_rendering", *viz_ctls->num_lic_rendering_controls);
		for(i=0;i<*viz_ctls->num_lic_rendering_controls;i++){
			if(cmp_no_case_c(viz_ctls->lic_ctls[i]->ctl_file_name, "NO_FILE") > 0){
				level = write_begin_flag_for_ctl_c(fp, level, "LIC_rendering");
				level = write_end_flag_for_ctl_c(fp, level, "LIC_rendering");
			} else {
				write_file_flag_for_ctl_c(fp, level, "LIC_rendering", viz_ctls->lic_ctls[i]->ctl_file_name);
			};
		};
		level = write_end_array_flag_for_ctl_c(fp, level, "LIC_rendering");
	};
	
	if(*viz_ctls->num_fieldline_controls > 0){
		fprintf(fp, "!\n");
		level = write_array_flag_for_ctl_c(fp, level, "fieldline", *viz_ctls->num_fieldline_controls);
		for(i=0;i<*viz_ctls->num_fieldline_controls;i++){
			if(cmp_no_case_c(viz_ctls->fline_ctls[i]->ctl_file_name, "NO_FILE") > 0){
				level = write_begin_flag_for_ctl_c(fp, level, "fieldline");
				level = write_end_flag_for_ctl_c(fp, level, "fieldline");
			} else {
				write_file_flag_for_ctl_c(fp, level, "fieldline", viz_ctls->fline_ctls[i]->ctl_file_name);
			};
		};
		level = write_end_array_flag_for_ctl_c(fp, level, "fieldline");
	};
	
	
	level = write_end_flag_for_ctl_c(fp, level, "visual_control");
	return level;
}


int main(int argc,char *argv[])
{
	struct SGS_MHD_control_c *mhd_ctl;
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char ctmp[32];            /* character buffer for reading line */
	char ctmp2[32];            /* character buffer for reading line */
	int i, j;
	int level;
	
	/*
	c_read_control_sph_SGS_MHD();
	c_write_control_sph_SGS_MHD();
	*/
	fld_ctl = (struct field_control *) malloc(sizeof(struct field_control));
	fld_ctl->fld_def = (struct all_field_def*)malloc(sizeof(struct all_field_def));
	fld_ctl->iflag_use = (int *)calloc(fld_ctl->fld_def->num_field, sizeof(int));
	fld_ctl->iflag_viz = (int *)calloc(fld_ctl->fld_def->num_field, sizeof(int));
	fld_ctl->iflag_monitor = (int *)calloc(fld_ctl->fld_def->num_field, sizeof(int));
	
	i = alloc_copy_field_cond_dimension_list_c(fld_ctl->fld_def);
	
	
		printf("baka %d\n", fld_ctl->fld_def->num_field);
	for(i=0;i<fld_ctl->fld_def->num_field;i++){
		printf("field_name %d: %s %d\n", i, fld_ctl->fld_def->field_names[i], fld_ctl->fld_def->field_comps[i]);
	}
	
	
	
	mhd_ctl = (struct SGS_MHD_control_c *) malloc(sizeof(struct SGS_MHD_control_c));
	
	if ((fp = fopen("control_MHD", "r")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	};
	
	fgets(buf, LENGTHBUF, fp);
	if(right_begin_flag_c(buf, "MHD_control") > 0){
		alloc_SGS_MHD_control_c(mhd_ctl);
		while(find_control_end_flag_c(buf, "MHD_control") == 0){
			fgets(buf, LENGTHBUF, fp);
			
			if(right_begin_flag_c(buf, "data_files_def") > 0) *mhd_ctl->iflag_data_files_def = read_platform_data_control_c(fp, buf, "data_files_def", mhd_ctl->files);
			if(right_begin_flag_c(buf, "org_data_files_def") > 0) *mhd_ctl->iflag_org_files_def = read_platform_data_control_c(fp, buf, "org_data_files_def", mhd_ctl->org_files);
			if(right_begin_flag_c(buf, "new_data_files_def") > 0) *mhd_ctl->iflag_new_files_def = read_platform_data_control_c(fp, buf, "new_data_files_def", mhd_ctl->new_files);
			if(right_begin_flag_c(buf, "spherical_shell_ctl") > 0) *mhd_ctl->iflag_spherical_shell_ctl = read_spherical_shell_ctl_c(fp, buf, mhd_ctl->shell_ctl);
			if(right_begin_flag_c(buf, "model") > 0) *mhd_ctl->iflag_model = read_mhd_model_ctl_c(fp, buf, mhd_ctl->model_ctl);
			if(right_begin_flag_c(buf, "control") > 0) *mhd_ctl->iflag_control = read_mhd_control_ctl_c(fp, buf, mhd_ctl->control_ctl);
			if(right_begin_flag_c(buf, "sph_monitor_ctl") > 0) *mhd_ctl->iflag_sph_monitor_ctl = read_sph_monitor_ctl_c(fp, buf, mhd_ctl->monitor_ctl);
			if(right_begin_flag_c(buf, "zonal_mean_control") > 0) *mhd_ctl->iflag_zonal_mean_control = read_zonal_mean_control_c(fp, buf, mhd_ctl->zm_ctls);
			if(right_begin_flag_c(buf, "visual_control") > 0) *mhd_ctl->iflag_visual_control = read_visual_control_c(fp, buf, mhd_ctl->viz_ctls);
			
/*			i = right_begin_flag_c(buf, "phys_values_ctl");*/
		};
	};
	
	fclose(fp);
	
	if ((fp = fopen("control_MHD_2", "w")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	};
	
	level = 0;
	level = write_begin_flag_for_ctl_c(fp, level, "MHD_control");
	
	level = write_platform_data_control_c(fp, level, mhd_ctl->iflag_data_files_def, 
				"data_files_def", mhd_ctl->files);
	if(*mhd_ctl->iflag_org_files_def > 0) fprintf(fp, "!\n");
	level = write_platform_data_control_c(fp, level, mhd_ctl->iflag_org_files_def, 
				"org_data_files_def", mhd_ctl->org_files);
	if(*mhd_ctl->iflag_new_files_def > 0) fprintf(fp, "!\n");
	level = write_platform_data_control_c(fp, level, mhd_ctl->iflag_new_files_def, 
				"new_data_files_def", mhd_ctl->new_files);
	if(*mhd_ctl->iflag_spherical_shell_ctl > 0) fprintf(fp, "!\n");
	level = write_spherical_shell_ctl_c(fp, level, mhd_ctl->iflag_spherical_shell_ctl, mhd_ctl->shell_ctl);
	if(*mhd_ctl->iflag_model > 0) fprintf(fp, "!\n");
	level = write_mhd_model_ctl_c(fp, level, mhd_ctl->iflag_model, mhd_ctl->model_ctl);
	if(*mhd_ctl->iflag_control > 0) fprintf(fp, "!\n");
	level = write_mhd_control_ctl_c(fp, level, mhd_ctl->iflag_control, mhd_ctl->control_ctl);
	if(*mhd_ctl->iflag_sph_monitor_ctl > 0) fprintf(fp, "!\n");
	level = write_sph_monitor_ctl_c(fp, level, mhd_ctl->iflag_sph_monitor_ctl, mhd_ctl->monitor_ctl);
	if(*mhd_ctl->iflag_zonal_mean_control > 0) fprintf(fp, "!\n");
	level = write_zonal_mean_control_c(fp, level, mhd_ctl->iflag_zonal_mean_control, mhd_ctl->zm_ctls);
	if(*mhd_ctl->iflag_visual_control > 0) fprintf(fp, "!\n");
	level = write_visual_control_c(fp, level, mhd_ctl->iflag_visual_control, mhd_ctl->viz_ctls);
	
	level = write_end_flag_for_ctl_c(fp, level, "MHD_control");
	
	fclose(fp);
	free(mhd_ctl);
	
	return 0;
}
