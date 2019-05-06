
/*  all_field_names_c.c */

#include <stdlib.h>
#include "all_field_names_c.h"


const struct field_def field_props[NUM_FIELD] = {
			{1, "t_step", "$$  \\Delta t  $$"},
			{1, "time", "$$  t  $$"},
			{3, "velocity", "$$  u  $$"},
			{3, "vorticity", "$$  \\omega  $$"},
			{1, "pressure", "$$  p  $$"},
			{3, "magnetic_field", "$$  B  $$"},
			{3, "current_density", "$$  J  $$"},
			{3, "vector_potential", "$$  A  $$"},
			{1, "scalar_potential", "$$  \\varphi  $$"},
			{1, "magnetic_potential", "$$  \\varphi  $$"},
			{1, "temperature", "$$  T  $$"},
			{1, "perturbation_temp", "$$  \\Theta = T-T_{0}  $$"},
			{1, "reference_temperature", "$$  T_{0}  $$"},
			{1, "composition", "$$  C  $$"},
			{1, "parturbation_composition", "$$  \\Delta C = C-C_{0}  $$"},
			{1, "reference_composition", "$$  C_{0}  $$"},
			{1, "entropy", "$$  S  $$"},
			{1, "perturbation_entropy", "$$  \\Delta S = S-S_{0}  $$"},
			{1, "reference_entropy", "$$  S_{0}  $$"},
			{1, "heat_source", "$$  Q_{T}  $$"},
			{1, "composition_source", "$$  Q_{C}  $$"},
			{1, "entropy_source", "$$  Q_{S}  $$"},
			{1, "density", "$$  \\rho  $$"},
			{1, "perturbation_density", "$$  \\Delta \rho = \\rho - \\rho_{0}  $$"},
			{1, "reference_density", "$$  \\rho_{0}  $$"},
			
			{3, "grad_temp", "$$  \\nabla T  $$"},
			{3, "grad_part_temp", "$$  \\nabla \\Theta  $$"},
			{3, "grad_reference_temp", "$$  \\nabla T_{0}  $$"},
			{3, "grad_composition", "$$  \\nabla C  $$"},
			{3, "grad_part_composition", "$$  \\nabla \\left(\\Delta C\\right)  $$"},
			{3, "grad_reference_composition", "$$  \\nabla C_{0}  $$"},
			
			/* Square of fields */
			{1, "square_velocity", "$$  u^{2}  $$"},
			{1, "square_vorticity", "$$  \omega^{2}  $$"},
			{1, "square_magne", "$$  B^{2}  $$"},
			{1, "square_vector_p", "$$  A^{2}  $$"},
			{1, "square_current", "$$  J^{2}  $$"},
			{1, "square_temperature", "$$  T^{2}  $$"},
			{1, "square_composition", "$$  C^{2} $$"},
			
			/* Gradients */
			{3, "grad_v_1", "$$  \\nabla u_{1}  $$"},
			{3, "grad_v_2", "$$  \\nabla u_{2}  $$"},
			{3, "grad_v_3", "$$  \\nabla u_{3}  $$"},
			
			{3, "grad_w_1", "$$  \\nabla \\omega_{1}  $$"},
			{3, "grad_w_2", "$$  \\nabla \\omega_{2}  $$"},
			{3, "grad_w_3", "$$  \\nabla \\omega_{3}  $$"},
			
			{3, "grad_a_1", "$$    $$"},
			{3, "grad_a_2", "$$    $$"},
			{3, "grad_a_3", "$$    $$"},
			
			{3, "grad_b_1", "$$    $$"},
			{3, "grad_b_2", "$$    $$"},
			{3, "grad_b_3", "$$    $$"},
			
			{3, "grad_j_1", "$$    $$"},
			{3, "grad_j_2", "$$    $$"},
			{3, "grad_j_3", "$$    $$"},
			
			/* Diffusions */
			{3, "viscous_diffusion", "$$    $$"},
			{3, "vorticity_diffusion", "$$    $$"},
			{3, "diffuse_vector_p", "$$    $$"},
			{3, "magnetic_diffusion", "$$    $$"},
			{1, "composition_diffusion", "$$    $$"},
			{1, "thermal_diffusion", "$$    $$"},
			
			/* Fluxes */
			{6, "momentum_flux", "$$    $$"},
			{6, "maxwell_tensor", "$$    $$"},
			{3, "heat_flux", "$$    $$"},
			{3, "part_h_flux", "$$    $$"},
			{3, "composite_flux", "$$    $$"},
			{3, "part_c_flux", "$$    $$"},
			
			{3, "pressure_gradient", "$$    $$"},
			{3, "inertia", "$$    $$"},
			{3, "Lorentz_force", "$$    $$"},
			{3, "Coriolis_force", "$$    $$"},
			{3, "geostrophic_balance", "$$    $$"},
			{3, "buoyancy", "$$    $$"},
			{3, "composite_buoyancy", "$$    $$"},
			{3, "induction_tensor", "$$    $$"},
			
			{3, "div_m_flux", "$$    $$"},
			{3, "div_maxwell_t", "$$    $$"},
			
			{3, "vecp_induction", "$$    $$"},
			{3, "magnetic_induction", "$$    $$"},
			{3, "magnetic_stretch", "$$    $$"},
			{1, "div_induct_t", "$$    $$"},
			
			{1, "heat_advect", "$$    $$"},
			{1, "part_h_advect", "$$    $$"},
			{1, "div_h_flux", "$$    $$"},
			{1, "div_part_h_flux", "$$    $$"},
			{1, "composition_advect", "$$    $$"},
			{1, "part_c_advect", "$$    $$"},
			{1, "div_c_flux", "$$    $$"},
			{1, "div_part_c_flux", "$$    $$"},
			
			{3, "electric_field", "$$    $$"},
			{3, "poynting_flux", "$$    $$"},
			{3, "magnetic_tension", "$$    $$"},
			{1, "magnetic_ene_generation", "$$    $$"},
			{1, "work_against_Lorentz", "$$    $$"},
			{1, "Lorentz_work", "$$    $$"},
			{1, "mag_tension_work", "$$    $$"},
			{1, "buoyancy_flux", "$$    $$"},
			{1, "composite_buoyancy_flux", "$$    $$"},
			{1, "temp_generation", "$$    $$"},
			{1, "part_temp_gen", "$$    $$"},
			{1, "part_comp_gen", "$$    $$"},
			{1, "vis_ene_diffuse", "$$    $$"},
			{1, "mag_ene_diffuse", "$$    $$"},
			{1, "pressure_work", "$$    $$"},
			{1, "m_potential_work", "$$    $$"},
			
			{1, "kinetic_helicity", "$$    $$"},
			{1, "magnetic_helicity", "$$    $$"},
			{1, "current_helicity", "$$    $$"},
			{1, "cross_helicity", "$$    $$"},
			
			{1, "div_velocity", "$$    $$"},
			{1, "div_magnetic", "$$    $$"},
			{1, "div_vector_p", "$$    $$"},
			
			{1, "div_inertia", "$$    $$"},
			{1, "div_Lorentz_force", "$$    $$"},
			{1, "div_Coriolis_force", "$$    $$"},
			{1, "div_buoyancy", "$$    $$"},
			{1, "div_composite_buoyancy", "$$    $$"},
			{1, "div_filtered_buoyancy", "$$    $$"},
			{1, "div_viscousity", "$$    $$"},
			
			{3, "rot_inertia", "$$    $$"},
			{3, "rot_Lorentz_force", "$$    $$"},
			{3, "rot_Coriolis_force", "$$    $$"},
			{3, "rot_buoyancy", "$$    $$"},
			{3, "rot_composite_buoyancy", "$$    $$"},
			{3, "rot_filtered_buoyancy", "$$    $$"},
			
			{3, "sum_forces", "$$    $$"},
			{3, "rot_sum_forces", "$$    $$"},
			{1, "div_sum_forces", "$$    $$"},
			
			{3, "previous_momentum", "$$    $$"},
			{3, "previous_induction", "$$    $$"},
			{1, "previous_heat", "$$    $$"},
			{1, "previous_composition", "$$    $$"},
			{1, "previous_pressure", "$$    $$"},
			
			{3, "check_momentum", "$$    $$"},
			{3, "check_induction", "$$    $$"},
			{1, "check_heat", "$$    $$"},
			{1, "check_composition", "$$    $$"},
			{1, "check_pressure", "$$    $$"},
			{1, "check_potential", "$$    $$"},
			
			{1, "viscosity", "$$    $$"},
			{1, "kinetic_viscosity", "$$    $$"},
			{1, "thermal_conductivity", "$$    $$"},
			{1, "thermal_diffusivity", "$$    $$"},
			{1, "chemical_diffusivity", "$$    $$"},
			{1, "magnetic_diffusivity", "$$    $$"},
			
			{3, "system_Rotation", "$$    $$"},
			{3, "background_B", "$$    $$"},
	/* Filtered field */
			{3, "filter_velo", "$$    $$"},
			{3, "filter_vorticity", "$$    $$"},
			{3, "filter_magne", "$$    $$"},
			{3, "filter_current", "$$    $$"},
			{3, "filter_vecp", "$$    $$"},
			{1, "filter_temp", "$$    $$"},
			{1, "filter_part_temp", "$$    $$"},
			{1, "filter_composition", "$$    $$"},
			
			{3, "grad_filtered_temp", "$$    $$"},
			{3, "grad_filtered_comp", "$$    $$"},
			
			{3, "grad_filtered_v_1", "$$    $$"},
			{3, "grad_filtered_v_2", "$$    $$"},
			{3, "grad_filtered_v_3", "$$    $$"},
			
			{3, "grad_filtered_w_1", "$$    $$"},
			{3, "grad_filtered_w_2", "$$    $$"},
			{3, "grad_filtered_w_3", "$$    $$"},
			
			{3, "grad_filtered_a_1", "$$    $$"},
			{3, "grad_filtered_a_2", "$$    $$"},
			{3, "grad_filtered_a_3", "$$    $$"},
			
			{3, "grad_filtered_b_1", "$$    $$"},
			{3, "grad_filtered_b_2", "$$    $$"},
			{3, "grad_filtered_b_3", "$$    $$"},
			
			{3, "grad_filtered_j_1", "$$    $$"},
			{3, "grad_filtered_j_2", "$$    $$"},
			{3, "grad_filtered_j_3", "$$    $$"},
			
			{1, "div_filter_velo", "$$    $$"},
			{1, "div_filter_magne", "$$    $$"},
			{1, "div_filter_vecp", "$$    $$"},
			
			{3, "filtered_buoyancy", "$$    $$"},
			{1, "filtered_buoyancy_flux", "$$    $$"},
			
			{3, "SGS_heat_flux", "$$    $$"},
			{3, "SGS_composit_flux", "$$    $$"},
			{6, "SGS_momentum_flux", "$$    $$"},
			{6, "SGS_maxwell_tensor", "$$    $$"},
			{3, "SGS_inertia", "$$    $$"},
			{3, "SGS_Lorentz", "$$    $$"},
			{3, "SGS_vecp_induction", "$$    $$"},
			{3, "SGS_induction", "$$    $$"},
			{3, "SGS_buoyancy", "$$    $$"},
			{3, "SGS_composit_buoyancy", "$$    $$"},
			{3, "SGS_induct_tensor", "$$    $$"},
			
			{1, "Csim_SGS_heat_flux", "$$    $$"},
			{1, "Csim_SGS_composit_flux", "$$    $$"},
			{1, "Csim_SGS_inertia", "$$    $$"},
			{1, "Csim_SGS_Lorentz", "$$    $$"},
			{1, "Csim_SGS_vp_induction", "$$    $$"},
			{1, "Csim_SGS_buoyancy", "$$    $$"},
			{1, "Csim_SGS_composit_buo", "$$    $$"},
			
			{3, "heat_flux_w_SGS", "$$    $$"},
			{3, "comp_flux_w_SGS", "$$    $$"},
			
			{3, "rot_SGS_inertia", "$$    $$"},
			{3, "rot_SGS_Lorentz", "$$    $$"},
			{1, "div_SGS_inertia", "$$    $$"},
			{1, "div_SGS_Lorentz", "$$    $$"},
			
			{1, "div_SGS_h_flux", "$$    $$"},
			{1, "div_SGS_c_flux", "$$    $$"},
			{3, "div_SGS_m_flux", "$$    $$"},
			{6, "momentum_flux_w_SGS", "$$    $$"},
			{6, "maxwell_tensor_w_sgs", "$$    $$"},
			{3, "intertia_w_SGS", "$$    $$"},
			{3, "Lorentz_w_SGS", "$$    $$"},
			{3, "vecp_induction_w_SGS", "$$    $$"},
			{3, "induction_w_SGS", "$$    $$"},
			
			{1, "SGS_m_ene_gen", "$$    $$"},
			{1, "SGS_temp_gen", "$$    $$"},
			{1, "SGS_Lorentz_work", "$$    $$"},
			{1, "Reynolds_work", "$$    $$"},
			{1, "SGS_buoyancy_flux", "$$    $$"},
			{1, "SGS_comp_buoyancy_flux", "$$    $$"},
			
			{3, "SGS_div_m_flux_true", "$$    $$"},
			{3, "SGS_Lorentz_true", "$$    $$"},
			{3, "SGS_mag_induct_true", "$$    $$"},
			{1, "SGS_div_h_flux_true", "$$    $$"},
			{1, "SGS_div_c_flux_true", "$$    $$"},
			{1, "SGS_Lorentz_work_true", "$$    $$"},
			{1, "Reynolds_work_true", "$$    $$"},
			{1, "SGS_temp_gen_true", "$$    $$"},
			{1, "SGS_comp_gen_true", "$$    $$"},
			{1, "SGS_m_ene_gen_true", "$$    $$"},
			
			{3, "wide_filter_velo", "$$    $$"},
			{3, "wide_filter_vorticity", "$$    $$"},
			{3, "wide_filter_vecp", "$$    $$"},
			{3, "wide_filter_magne", "$$    $$"},
			{3, "wide_filter_current", "$$    $$"},
			{1, "wide_filter_temp", "$$    $$"},
			{1, "wide_filter_composition", "$$    $$"},
			{3, "wide_filter_grad_temp", "$$    $$"},
			{3, "wide_filter_grad_composition", "$$    $$"},
			
			{3, "wide_SGS_heat_flux", "$$    $$"},
			{3, "wide_SGS_composit_flux", "$$    $$"},
			{3, "wide_SGS_inertia", "$$    $$"},
			{3, "wide_SGS_Lorentz", "$$    $$"},
			{3, "wide_SGS_vp_induction", "$$    $$"},
			
			{3, "double_filter_velo", "$$    $$"},
			{3, "double_filter_vorticity", "$$    $$"},
			{3, "double_filter_vecp", "$$    $$"},
			{3, "double_filter_magne", "$$    $$"},
			{3, "double_filter_current", "$$    $$"},
			{1, "double_filter_temp", "$$    $$"},
			{1, "double_filter_composition", "$$    $$"},
			{3, "double_filter_grad_temp", "$$    $$"},
			{3, "double_filter_grad_comp", "$$    $$"},
			
			{3, "double_SGS_heat_flux", "$$    $$"},
			{3, "double_SGS_composit_flux", "$$    $$"},
			{3, "double_SGS_inertia", "$$    $$"},
			{3, "double_SGS_Lorentz", "$$    $$"},
			{3, "double_SGS_vp_induction", "$$    $$"},
			
			{1, "temp_4_SGS", "$$    $$"},
			{1, "comp_4_SGS", "$$    $$"},
			
			{6, "SGS_simi", "$$    $$"},
			{6, "SGS_grad", "$$    $$"},
			{3, "SGS_grad_f", "$$    $$"},
			{3, "SGS_diffuse", "$$    $$"},
			
			{1, "velocity_scale", "$$    $$"},
			{1, "magnetic_scale", "$$    $$"},
			{1, "temperature_scale", "$$    $$"},
			{1, "composition_scale", "$$    $$"},
		};


int get_field_properties(int index, char *name, char *math){
	int j;
	
	if(index < -1 || index >= NUM_FIELD) return 0;
	for (j = 0; j < NCHARA_FIELD;j++) {
		name[j] = field_props[index].field_name[j];
	};
	for (j = 0; j < KCHARA_C;j++) {
		math[j] = field_props[index].field_math[j];
	};
    return field_props[index].num_comps;
};
