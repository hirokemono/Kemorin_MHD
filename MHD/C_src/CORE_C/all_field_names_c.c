
/*  all_field_names_c.c */

#include <stdlib.h>
#include "all_field_names_c.h"

const char field_names[NUM_FIELD][NCHARA_FIELD] = {
			{"t_step"},
			{"time"},
			{"velocity"},
			{"pressure"},
			{"vector_potential"},
			{"magnetic_field"},
			{"current_density"},
			{"vector_potential"},
			{"scalar_potential"},
			{"magnetic_potential"},
			{"temperature"},
			{"perturbation_temp"},
			{"reference_temperature"},
			{"composition"},
			{"parturbation_composition"},
			{"reference_composition"},
			{"entropy"},
			{"perturbation_entropy"},
			{"reference_entropy"},
			{"heat_source"},
			{"composition_source"},
			{"entropy_source"},
			{"density"},
			{"perturbation_density"},
			{"reference_density"},
			
			{"grad_temp"},
			{"grad_part_temp"},
			{"grad_reference_temp"},
			{"grad_composition"},
			{"grad_part_composition"},
			{"grad_reference_composition"},
			
			{"grad_v_1"},
			{"grad_v_2"},
			{"grad_v_3"},
			
			{"grad_w_1"},
			{"grad_w_2"},
			{"grad_w_3"},
			
			{"grad_a_1"},
			{"grad_a_2"},
			{"grad_a_3"},
			
			{"grad_b_1"},
			{"grad_b_2"},
			{"grad_b_3"},
			
			{"grad_j_1"},
			{"grad_j_2"},
			{"grad_j_3"},
			
			/* Diffusions */
			{"viscous_diffusion"},
			{"vorticity_diffusion"},
			{"diffuse_vector_p"},
			{"magnetic_diffusion"},
			{"composition_diffusion"},
			{"thermal_diffusion"},
			
			
			{"momentum_flux"},
			{"maxwell_tensor"},
			{"heat_flux"},
			{"part_h_flux"},
			{"composite_flux"},
			{"part_c_flux"},
			
			{"pressure_gradient"},
			{"inertia"},
			{"Lorentz_force"},
			{"Coriolis_force"},
			{"geostrophic_balance"},
			{"buoyancy"},
			{"composite_buoyancy"},
			{"induction_tensor"},
			
			{"div_m_flux"},
			{"div_maxwell_t"},
			
			{"vecp_induction"},
			{"magnetic_induction"},
			{"magnetic_stretch"},
			{"div_induct_t"},
			
			{"heat_advect"},
			{"part_h_advect"},
			{"div_h_flux"},
			{"div_part_h_flux"},
			{"composition_advect"},
			{"part_c_advect"},
			{"div_c_flux"},
			{"div_part_c_flux"},
			
			{"electric_field"},
			{"poynting_flux"},
			{"magnetic_tension"},
			{"magnetic_ene_generation"},
			{"work_against_Lorentz"},
			{"Lorentz_work"},
			{"mag_tension_work"},
			{"buoyancy_flux"},
			{"composite_buoyancy_flux"},
			{"temp_generation"},
			{"part_temp_gen"},
			{"part_comp_gen"},
			{"vis_ene_diffuse"},
			{"mag_ene_diffuse"},
			{"pressure_work"},
			{"m_potential_work"},
			
			{"kinetic_helicity"},
			{"magnetic_helicity"},
			{"current_helicity"},
			{"cross_helicity"},
			
			{"div_velocity"},
			{"div_magnetic"},
			{"div_vector_p"},
			
			{"div_inertia"},
			{"div_Lorentz_force"},
			{"div_Coriolis_force"},
			{"div_buoyancy"},
			{"div_composite_buoyancy"},
			{"div_filtered_buoyancy"},
			{"div_viscousity"},
			
			{"rot_inertia"},
			{"rot_Lorentz_force"},
			{"rot_Coriolis_force"},
			{"rot_buoyancy"},
			{"rot_composite_buoyancy"},
			{"rot_filtered_buoyancy"},
			
			{"sum_forces"},
			{"rot_sum_forces"},
			{"div_sum_forces"},
			
			{"previous_momentum"},
			{"previous_induction"},
			{"previous_heat"},
			{"previous_composition"},
			{"previous_pressure"},
			
			{"check_momentum"},
			{"check_induction"},
			{"check_heat"},
			{"check_composition"},
			{"check_pressure"},
			{"check_potential"},
			
			{"viscosity"},
			{"kinetic_viscosity"},
			{"thermal_conductivity"},
			{"thermal_diffusivity"},
			{"chemical_diffusivity"},
			{"magnetic_diffusivity"},
			
			{"system_Rotation"},
			{"background_B"},
	/* Filtered field */
			{"filter_velo"},
			{"filter_vorticity"},
			{"filter_magne"},
			{"filter_current"},
			{"filter_vecp"},
			{"filter_temp"},
			{"filter_part_temp"},
			{"filter_composition"},
			
			{"grad_filtered_temp"},
			{"grad_filtered_comp"},
			
			{"grad_filtered_v_1"},
			{"grad_filtered_v_2"},
			{"grad_filtered_v_3"},
			
			{"grad_filtered_w_1"},
			{"grad_filtered_w_2"},
			{"grad_filtered_w_3"},
			
			{"grad_filtered_a_1"},
			{"grad_filtered_a_2"},
			{"grad_filtered_a_3"},
			
			{"grad_filtered_b_1"},
			{"grad_filtered_b_2"},
			{"grad_filtered_b_3"},
			
			{"grad_filtered_j_1"},
			{"grad_filtered_j_2"},
			{"grad_filtered_j_3"},
			
			{"div_filter_velo"},
			{"div_filter_magne"},
			{"div_filter_vecp"},
			
			{"filtered_buoyancy"},
			{"filtered_buoyancy_flux"},
			
			{"SGS_heat_flux"},
			{"SGS_composit_flux"},
			{"SGS_momentum_flux"},
			{"SGS_maxwell_tensor"},
			{"SGS_inertia"},
			{"SGS_Lorentz"},
			{"SGS_vecp_induction"},
			{"SGS_induction"},
			{"SGS_buoyancy"},
			{"SGS_composit_buoyancy"},
			{"SGS_induct_tensor"},
			
			{"Csim_SGS_heat_flux"},
			{"Csim_SGS_composit_flux"},
			{"Csim_SGS_inertia"},
			{"Csim_SGS_Lorentz"},
			{"Csim_SGS_vp_induction"},
			{"Csim_SGS_buoyancy"},
			{"Csim_SGS_composit_buo"},
			
			{"heat_flux_w_SGS"},
			{"comp_flux_w_SGS"},
			
			{"rot_SGS_inertia"},
			{"rot_SGS_Lorentz"},
			{"div_SGS_inertia"},
			{"div_SGS_Lorentz"},
			
			{"div_SGS_h_flux"},
			{"div_SGS_c_flux"},
			{"momentum_flux_w_SGS"},
			{"maxwell_tensor_w_sgs"},
			{"intertia_w_SGS"},
			{"Lorentz_w_SGS"},
			{"vecp_induction_w_SGS"},
			{"induction_w_SGS"},
			
			{"SGS_m_ene_gen"},
			{"SGS_temp_gen"},
			{"SGS_Lorentz_work"},
			{"Reynolds_work"},
			{"SGS_buoyancy_flux"},
			{"SGS_comp_buoyancy_flux"},
			
			{"SGS_div_m_flux_true"},
			{"SGS_Lorentz_true"},
			{"SGS_mag_induct_true"},
			{"SGS_div_h_flux_true"},
			{"SGS_div_c_flux_true"},
			{"SGS_Lorentz_work_true"},
			{"Reynolds_work_true"},
			{"SGS_temp_gen_true"},
			{"SGS_comp_gen_true"},
			{"SGS_m_ene_gen_true"},
			
			{"wide_filter_velo"},
			{"wide_filter_vorticity"},
			{"wide_filter_vecp"},
			{"wide_filter_magne"},
			{"wide_filter_current"},
			{"wide_filter_temp"},
			{"wide_filter_composition"},
			{"wide_filter_grad_temp"},
			{"wide_filter_grad_composition"},
			
			{"wide_SGS_heat_flux"},
			{"wide_SGS_composit_flux"},
			{"wide_SGS_inertia"},
			{"wide_SGS_Lorentz"},
			{"wide_SGS_vp_induction"},
			
			{"double_filter_velo"},
			{"double_filter_vorticity"},
			{"double_filter_vecp"},
			{"double_filter_magne"},
			{"double_filter_current"},
			{"double_filter_temp"},
			{"double_filter_composition"},
			{"double_filter_grad_temp"},
			{"double_filter_grad_comp"},
			
			{"double_SGS_heat_flux"},
			{"double_SGS_composit_flux"},
			{"double_SGS_inertia"},
			{"double_SGS_Lorentz"},
			{"double_SGS_vp_induction"},
			
			{"temp_4_SGS"},
			{"comp_4_SGS"},
			
			{"SGS_simi"},
			{"SGS_grad"},
			{"SGS_grad_f"},
			{"SGS_diffuse"},
		};

const int field_comps[NUM_FIELD] = {
			/*{"t_step"}*/  1,
			/*{"time"}*/  1,
			/*{"velocity"}*/  3,
			/*{"pressure"}*/  1,
			/*{"vector_potential"}*/  3,
			/*{"magnetic_field"}*/  3,
			/*{"current_density"}*/  3,
			/*{"vector_potential"}*/  3,
			/*{"scalar_potential"}*/  1,
			/*{"magnetic_potential"}*/  1,
			/*{"temperature"}*/  1,
			/*{"perturbation_temp"}*/  1,
			/*{"reference_temperature"}*/  1,
			/*{"composition"}*/  1,
			/*{"parturbation_composition"}*/  1,
			/*{"reference_composition"}*/  1,
			/*{"entropy"}*/  1,
			/*{"perturbation_entropy"}*/  1,
			/*{"reference_entropy"}*/  1,
			/*{"heat_source"}*/  1,
			/*{"composition_source"}*/  1,
			/*{"entropy_source"}*/  1,
			/*{"density"}*/  1,
			/*{"perturbation_density"}*/  1,
			/*{"reference_density"}*/  1,
			
			/*{"grad_temp"}*/  3,
			/*{"grad_part_temp"}*/  3,
			/*{"grad_reference_temp"}*/  3,
			/*{"grad_composition"}*/  3,
			/*{"grad_part_composition"}*/  3,
			/*{"grad_reference_composition"}*/  3,
			
			/*{"grad_v_1"}*/  3,
			/*{"grad_v_2"}*/  3,
			/*{"grad_v_3"}*/  3,
			
			/*{"grad_w_1"}*/  3,
			/*{"grad_w_2"}*/  3,
			/*{"grad_w_3"}*/  3,
			
			/*{"grad_a_1"}*/  3,
			/*{"grad_a_2"}*/  3,
			/*{"grad_a_3"}*/  3,
			
			/*{"grad_b_1"}*/  3,
			/*{"grad_b_2"}*/  3,
			/*{"grad_b_3"}*/  3,
			
			/*{"grad_j_1"}*/  3,
			/*{"grad_j_2"}*/  3,
			/*{"grad_j_3"}*/  3,
			
			/* Diffusions */
			/*{"viscous_diffusion"}*/  3,
			/*{"vorticity_diffusion"}*/  3,
			/*{"diffuse_vector_p"}*/  3,
			/*{"magnetic_diffusion"}*/  3,
			/*{"composition_diffusion"}*/  1,
			/*{"thermal_diffusion"}*/  1,
			
			
			/*{"momentum_flux"}*/  6,
			/*{"maxwell_tensor"}*/  6,
			/*{"heat_flux"}*/  3,
			/*{"part_h_flux"}*/  3,
			/*{"composite_flux"}*/  3,
			/*{"part_c_flux"}*/  3,
			
			/*{"pressure_gradient"}*/  3,
			/*{"inertia"}*/  3,
			/*{"Lorentz_force"}*/  3,
			/*{"Coriolis_force"}*/  3,
			/*{"geostrophic_balance"}*/  3,
			/*{"buoyancy"}*/  3,
			/*{"composite_buoyancy"}*/  3,
			/*{"induction_tensor"}*/  3,
			
			/*{"div_m_flux"}*/  3,
			/*{"div_maxwell_t"}*/  3,
			
			/*{"vecp_induction"}*/  3,
			/*{"magnetic_induction"}*/  3,
			/*{"magnetic_stretch"}*/  3,
			/*{"div_induct_t"}*/  1,
			
			/*{"heat_advect"}*/  1,
			/*{"part_h_advect"}*/  1,
			/*{"div_h_flux"}*/  1,
			/*{"div_part_h_flux"}*/  1,
			/*{"composition_advect"}*/  1,
			/*{"part_c_advect"}*/  1,
			/*{"div_c_flux"}*/  1,
			/*{"div_part_c_flux"}*/  1,
			
			/*{"electric_field"}*/  3,
			/*{"poynting_flux"}*/  3,
			/*{"magnetic_tension"}*/  3,
			/*{"magnetic_ene_generation"}*/  1,
			/*{"work_against_Lorentz"}*/  1,
			/*{"Lorentz_work"}*/  1,
			/*{"mag_tension_work"}*/  1,
			/*{"buoyancy_flux"}*/  1,
			/*{"composite_buoyancy_flux"}*/  1,
			/*{"temp_generation"}*/  1,
			/*{"part_temp_gen"}*/  1,
			/*{"part_comp_gen"}*/  1,
			/*{"vis_ene_diffuse"}*/  1,
			/*{"mag_ene_diffuse"}*/  1,
			/*{"pressure_work"}*/  1,
			/*{"m_potential_work"}*/  1,
			
			/*{"kinetic_helicity"}*/  1,
			/*{"magnetic_helicity"}*/  1,
			/*{"current_helicity"}*/  1,
			/*{"cross_helicity"}*/  1,
			
			/*{"div_velocity"}*/  1,
			/*{"div_magnetic"}*/  1,
			/*{"div_vector_p"}*/  1,
			
			/*{"div_inertia"}*/  1,
			/*{"div_Lorentz_force"}*/  1,
			/*{"div_Coriolis_force"}*/  1,
			/*{"div_buoyancy"}*/  1,
			/*{"div_composite_buoyancy"}*/  1,
			/*{"div_filtered_buoyancy"}*/  1,
			/*{"div_viscousity"}*/  1,
			
			/*{"rot_inertia"}*/  3,
			/*{"rot_Lorentz_force"}*/  3,
			/*{"rot_Coriolis_force"}*/  3,
			/*{"rot_buoyancy"}*/  3,
			/*{"rot_composite_buoyancy"}*/  3,
			/*{"rot_filtered_buoyancy"}*/  3,
			
			/*{"sum_forces"}*/  3,
			/*{"rot_sum_forces"}*/  3,
			/*{"div_sum_forces"}*/  1,
			
			/*{"previous_momentum"}*/  3,
			/*{"previous_induction"}*/  3,
			/*{"previous_heat"}*/  1,
			/*{"previous_composition"}*/  1,
			/*{"previous_pressure"}*/  1,
			
			/*{"check_momentum"}*/  3,
			/*{"check_induction"}*/  3,
			/*{"check_heat"}*/  1,
			/*{"check_composition"}*/  1,
			/*{"check_pressure"}*/  1,
			/*{"check_potential"}*/  1,
			
			/*{"viscosity"}*/  1,
			/*{"kinetic_viscosity"}*/  1,
			/*{"thermal_conductivity"}*/  1,
			/*{"thermal_diffusivity"}*/  1,
			/*{"chemical_diffusivity"}*/  1,
			/*{"magnetic_diffusivity"}*/  1,
			
			/*{"system_Rotation"}*/  3,
			/*{"background_B"}*/  3,
			
			/*{"filter_velo"}*/  3,
			/*{"filter_vorticity"}*/  3,
			/*{"filter_magne"}*/  3,
			/*{"filter_current"}*/  3,
			/*{"filter_vecp"}*/  3,
			/*{"filter_temp"}*/  1,
			/*{"filter_part_temp"}*/  1,
			/*{"filter_composition"}*/  1,
			
			/*{"grad_filtered_temp"}*/  3,
			/*{"grad_filtered_comp"}*/  3,
			
			/*{"grad_filtered_v_1"}*/  3,
			/*{"grad_filtered_v_2"}*/  3,
			/*{"grad_filtered_v_3"}*/  3,
			
			/*{"grad_filtered_w_1"}*/  3,
			/*{"grad_filtered_w_2"}*/  3,
			/*{"grad_filtered_w_3"}*/  3,
			
			/*{"grad_filtered_a_1"}*/  3,
			/*{"grad_filtered_a_2"}*/  3,
			/*{"grad_filtered_a_3"}*/  3,
			
			/*{"grad_filtered_b_1"}*/  3,
			/*{"grad_filtered_b_2"}*/  3,
			/*{"grad_filtered_b_3"}*/  3,
			
			/*{"grad_filtered_j_1"}*/  3,
			/*{"grad_filtered_j_2"}*/  3,
			/*{"grad_filtered_j_3"}*/  3,
			
			/*{"div_filter_velo"}*/  1,
			/*{"div_filter_magne"}*/  1,
			/*{"div_filter_vecp"}*/  1,
			
			/*{"filtered_buoyancy"}*/  3,
			/*{"filtered_buoyancy_flux"}*/  1,
			
			/*{"SGS_heat_flux"}*/  3,
			/*{"SGS_composit_flux"}*/  3,
			/*{"SGS_momentum_flux"}*/  6,
			/*{"SGS_maxwell_tensor"}*/  6,
			/*{"SGS_inertia"}*/  3,
			/*{"SGS_Lorentz"}*/  3,
			/*{"SGS_vecp_induction"}*/  3,
			/*{"SGS_induction"}*/  3,
			/*{"SGS_buoyancy"}*/  3,
			/*{"SGS_composit_buoyancy"}*/  3,
			/*{"SGS_induct_tensor"}*/  3,
			
			/*{"Csim_SGS_heat_flux"}*/  1,
			/*{"Csim_SGS_composit_flux"}*/  1,
			/*{"Csim_SGS_inertia"}*/  1,
			/*{"Csim_SGS_Lorentz"}*/  1,
			/*{"Csim_SGS_vp_induction"}*/  1,
			/*{"Csim_SGS_buoyancy"}*/  1,
			/*{"Csim_SGS_composit_buo"}*/  1,
			
			/*{"heat_flux_w_SGS"}*/  3,
			/*{"comp_flux_w_SGS"}*/  3,
			
			/*{"rot_SGS_inertia"}*/  3,
			/*{"rot_SGS_Lorentz"}*/  3,
			/*{"div_SGS_inertia"}*/  1,
			/*{"div_SGS_Lorentz"}*/  1,
			
			/*{"div_SGS_h_flux"}*/  1,
			/*{"div_SGS_c_flux"}*/  1,
			/*{"momentum_flux_w_SGS"}*/  6,
			/*{"maxwell_tensor_w_sgs"}*/  6,
			/*{"intertia_w_SGS"}*/  3,
			/*{"Lorentz_w_SGS"}*/  3,
			/*{"vecp_induction_w_SGS"}*/  3,
			/*{"induction_w_SGS"}*/  3,
			
			/*{"SGS_m_ene_gen"}*/  1,
			/*{"SGS_temp_gen"}*/  1,
			/*{"SGS_Lorentz_work"}*/  1,
			/*{"Reynolds_work"}*/  1,
			/*{"SGS_buoyancy_flux"}*/  1,
			/*{"SGS_comp_buoyancy_flux"}*/  1,
			
			/*{"SGS_div_m_flux_true"}*/  3,
			/*{"SGS_Lorentz_true"}*/  3,
			/*{"SGS_mag_induct_true"}*/  3,
			/*{"SGS_div_h_flux_true"}*/  1,
			/*{"SGS_div_c_flux_true"}*/  1,
			/*{"SGS_Lorentz_work_true"}*/  1,
			/*{"Reynolds_work_true"}*/  1,
			/*{"SGS_temp_gen_true"}*/  1,
			/*{"SGS_comp_gen_true"}*/  1,
			/*{"SGS_m_ene_gen_true"}*/  1,
			
			/*{"wide_filter_velo"}*/  3,
			/*{"wide_filter_vorticity"}*/  3,
			/*{"wide_filter_vecp"}*/  3,
			/*{"wide_filter_magne"}*/  3,
			/*{"wide_filter_current"}*/  3,
			/*{"wide_filter_temp"}*/  1,
			/*{"wide_filter_composition"}*/  1,
			/*{"wide_filter_grad_temp"}*/  3,
			/*{"wide_filter_grad_composition"}*/  3,
			
			/*{"wide_SGS_heat_flux"}*/  3,
			/*{"wide_SGS_composit_flux"}*/  3,
			/*{"wide_SGS_inertia"}*/  3,
			/*{"wide_SGS_Lorentz"}*/  3,
			/*{"wide_SGS_vp_induction"}*/  3,
			
			/*{"double_filter_velo"}*/  3,
			/*{"double_filter_vorticity"}*/  3,
			/*{"double_filter_vecp"}*/  3,
			/*{"double_filter_magne"}*/  3,
			/*{"double_filter_current"}*/  3,
			/*{"double_filter_temp"}*/  1,
			/*{"double_filter_composition"}*/  1,
			/*{"double_filter_grad_temp"}*/  3,
			/*{"double_filter_grad_comp"}*/  3,
			
			/*{"double_SGS_heat_flux"}*/  3,
			/*{"double_SGS_composit_flux"}*/  3,
			/*{"double_SGS_inertia"}*/  3,
			/*{"double_SGS_Lorentz"}*/  3,
			/*{"double_SGS_vp_induction"}*/  3,
			
			/*{"temp_4_SGS"}*/  1,
			/*{"comp_4_SGS"}*/  1,
			
			/*{"SGS_simi"}*/  6,
			/*{"SGS_grad"}*/  6,
			/*{"SGS_grad_f"}*/  6,
			/*{"SGS_diffuse"}*/  3,
		};

int alloc_copy_field_cond_dimension_list_c(struct all_field_def *fld_def){
	int i, j;
	
	fld_def->num_field = NUM_FIELD;
	fld_def->field_names = (char **)calloc(NUM_FIELD, sizeof(char *));
	for (i = 0; i < NUM_FIELD; i++) {
		fld_def->field_names[i] = (char *)calloc(NCHARA_FIELD+1, sizeof(char));
	};
	
	for (i = 0; i < NUM_FIELD; i++) {
		for (j = 0; j < NCHARA_FIELD;j++) {
			fld_def->field_names[i][j] = field_names[i][j];
		};
	};
	
	fld_def->field_comps = (int *)calloc(NUM_FIELD, sizeof(int));
	
	for (i = 0; i < NUM_FIELD; i++) {
		fld_def->field_comps[i] = field_comps[i];
	};
	
	return NUM_FIELD;
}

void dealloc_copy_field_cond_dimension_list_c(struct all_field_def *fld_def){
	int i;
	
	for (i = 0; i < NUM_FIELD; i++) {
		free(fld_def->field_names[i]);
	};
	free(fld_def->field_names);
	free(fld_def->field_comps);
	return;
}
