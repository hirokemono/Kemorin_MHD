!m_phys_labels.f90
!      module m_phys_labels
!
!        programmed by H.Matsui on June, 2009
!
!!!!!  physical values!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! field names 
!
!   velocity:     velocity    v
!   temperature:  temperature T
!   pressure:     pressure    P
!   density:      density     \rho
!   vorticity:    vorticity   \omega = \nabra \times v
!   vector_potential:  vector potential \nabla \times A = B
!   magnetic_field:    magnetic field   B
!   current_density:   current density  J = \nabla \times B
!   magnetic_potential: potential       \phi
!   composition:        Composition anormally C
!   entropy:            Entropy               S
!
!   electric_field:    electric field   E
!   poynting_flux:     Poynting flux    S = E \times B
!
!   reference_temperature:   T_0
!   reference_density:       \rho_0
!   reference_entropy:       S_0
!
!   perturbation_temp:         \Theta = T - T_0
!   perturbation_density:      \rho - \rho_0
!   parturbation_composition:  C - C_0
!   perturbation_entropy:      S - S_0
!
!   filter_velo, filter_temp, filter_part_temp
!   filter_vecp, filter_magne, filter_composition
!
!   kinetic_helicity, magnetic_helicity
!   current_helicity, cross_helicity
!
!   buoyancy_work, Lorentz_work, mag_tension_work,
!   composite_buoyancy_flux, filtered_buoyancy_flux
!   magnetic_ene_generation, work_against_Lorentz
!   temp_generation, part_temp_gen
!   vis_ene_diffuse, mag_ene_diffuse
!
!   thermal_diffusion, viscous_diffusion, vorticity_diffusion
!   diffuse_vector_p, magnetic_diffusion, composition_diffusion
!   magnetic_tension, Lorentz_force
!   Coriolis_force, buoyancy, composite_buoyancy, filtered_buoyancy
!
!   div_inertia, div_Lorentz_force, div_Coriolis_force
!
!   heat_flux,      part_h_flux
!   composite_flux, part_c_flux
!   momentum_flux, maxwell_tensor
!   magnetic_induction, vecp_induction
!
!   heat_advect, part_h_advect
!   inertia,  
!   div_h_flux, div_part_h_flux
!   div_m_flux, div_maxwell_t
!
!   induction_tensor, div_induct_t
!
!   SGS_heat_flux, SGS_composit_flux
!   SGS_momentum_flux, SGS_maxwell_tensor
!   SGS_buoyancy,      SGS_composit_buoyancy
!   SGS_induct_tensor, SGS_vecp_induction
!
!   div_SGS_h_flux, div_SGS_m_flux
!   SGS_Lorentz
!   SGS_induction
!   temp_4_SGS, comp_4_SGS
!
!   SGS_Lorentz_work      Reynolds_work
!   SGS_temp_gen          SGS_m_ene_gen
!   SGS_buoyancy_flux     SGS_comp_buoyancy_flux
!
! termes for direct estimation
!   SGS_div_h_flux_true
!   SGS_div_m_flux_true, SGS_Lorentz_true, SGS_mag_induct_true
!
!   SGS_Lorentz_work_true   Reynolds_work_true
!   SGS_temp_gen_true       SGS_m_ene_gen_true
!
!   velocity_scale   temperature_scale
!   magnetic_scale   composition_scale
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    name of terms
!        heat flux:         heat
!        advection:         inertia
!        Lorentz force:     Lorentz
!        Coriolis force:    Coriolis
!        induction:         induction
!        composition flux:  comp_flux
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      module m_phys_labels
!
      use m_precision
!
      implicit none
!
      character(len=kchara), parameter :: fhd_t_step = 't_step'
      character(len=kchara), parameter :: fhd_time =   'time'
!
      character(len=kchara), parameter :: fhd_velo = 'velocity' 
      character(len=kchara), parameter :: fhd_vort = 'vorticity'
      character(len=kchara), parameter :: fhd_magne = 'magnetic_field'
      character(len=kchara), parameter :: fhd_vecp = 'vector_potential'
      character(len=kchara), parameter                                  &
     &             :: fhd_current = 'current_density'
      character(len=kchara), parameter                                  &
     &             :: fhd_e_field = 'electric_field'
      character(len=kchara), parameter                                  &
     &             :: fhd_poynting = 'poynting_flux'
!
      character(len=kchara), parameter :: fhd_filter_v = 'filter_velo'
      character(len=kchara), parameter :: fhd_filter_a = 'filter_vecp'
      character(len=kchara), parameter :: fhd_filter_b = 'filter_magne'
!
      character(len=kchara), parameter                                  &
     &             :: fhd_viscous = 'viscous_diffusion'
      character(len=kchara), parameter                                  &
     &             :: fhd_w_viscous = 'vorticity_diffusion'
      character(len=kchara), parameter                                  &
     &             :: fhd_vecp_diffuse = 'diffuse_vector_p'
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_diffuse = 'magnetic_diffusion'
      character(len=kchara), parameter                                  &
     &             :: fhd_c_diffuse = 'composition_diffusion'
!
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_tension = 'magnetic_tension'
!
      character(len=kchara), parameter :: fhd_h_flux =  'heat_flux'
      character(len=kchara), parameter :: fhd_ph_flux = 'part_h_flux'
!
      character(len=kchara), parameter :: fhd_c_flux =  'composite_flux'
!
      character(len=kchara), parameter :: fhd_inertia = 'inertia'
      character(len=kchara), parameter :: fhd_div_m_flux = 'div_m_flux'
      character(len=kchara), parameter                                  &
     &             :: fhd_div_maxwell_t = 'div_maxwell_t'
      character(len=kchara), parameter                                  &
     &             :: fhd_div_induct_t =  'div_induct_t'
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_induct =    'magnetic_induction'
      character(len=kchara), parameter                                  &
     &             :: fhd_vp_induct =     'vecp_induction'
      character(len=kchara), parameter                                  &
     &             :: fhd_Lorentz =       'Lorentz_force'
      character(len=kchara), parameter                                  &
     &             :: fhd_Coriolis =      'Coriolis_force'
      character(len=kchara), parameter                                  &
     &             :: fhd_buoyancy =      'buoyancy'
      character(len=kchara), parameter                                  &
     &             :: fhd_comp_buo =      'composite_buoyancy'
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_buo =    'filtered_buoyancy'
!
      character(len=kchara), parameter                                  &
     &             :: fhd_div_SGS_m_flux =      'div_SGS_m_flux'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_h_flux =          'SGS_heat_flux'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_c_flux =          'SGS_composit_flux'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_Lorentz =         'SGS_Lorentz'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_induction =       'SGS_induction'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_vp_induct =       'SGS_vecp_induction'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_div_m_flux_true = 'SGS_div_m_flux_true'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_Lorentz_true =    'SGS_Lorentz_true'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_mag_induct_true = 'SGS_mag_induct_true'
!
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_buoyancy =  'SGS_buoyancy'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_comp_buo = 'SGS_composit_buoyancy'
!
!  scalars
!
      character(len=kchara), parameter :: fhd_press = 'pressure'
!
      character(len=kchara), parameter :: fhd_temp =  'temperature'
      character(len=kchara), parameter                                  &
     &             :: fhd_part_temp = 'perturbation_temp'
      character(len=kchara), parameter                                  &
     &             :: fhd_ref_temp =  'reference_temperature'
!
      character(len=kchara), parameter :: fhd_light = 'composition'
      character(len=kchara), parameter                                  &
     &             :: fhd_part_light = 'parturbation_composition'
!
      character(len=kchara), parameter :: fhd_entropy =  'entropy'
      character(len=kchara), parameter                                  &
     &             :: fhd_per_entropy = 'perturbation_entropy'
      character(len=kchara), parameter                                  &
     &             :: fhd_ref_entropy =  'reference_entropy'
!
      character(len=kchara), parameter :: fhd_density =  'density'
      character(len=kchara), parameter                                  &
     &             :: fhd_per_density = 'perturbation_density'
      character(len=kchara), parameter                                  &
     &             :: fhd_ref_density =  'reference_density'
!
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_potential =     'magnetic_potential'
      character(len=kchara), parameter                                  &
     &             :: fhd_scalar_potential =  'scalar_potential'
!
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_temp =       'filter_temp'
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_part_temp =  'filter_part_temp'
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_comp =       'filter_composition'
!
      character(len=kchara), parameter                                  &
     &             :: fhd_kinetic_helicity =  'kinetic_helicity'
      character(len=kchara), parameter                                  &
     &             :: fhd_magnetic_helicity = 'magnetic_helicity'
      character(len=kchara), parameter                                  &
     &             :: fhd_current_helicity =  'current_helicity'
      character(len=kchara), parameter                                  &
     &             :: fhd_cross_helicity =    'cross_helicity'
!
      character(len=kchara), parameter                                  &
     &             :: fhd_thermal_diffusion = 'thermal_diffusion'
      character(len=kchara), parameter                                  &
     &             :: fhd_heat_advect =       'heat_advect'
      character(len=kchara), parameter                                  &
     &             :: fhd_part_h_advect =     'part_h_advect'
      character(len=kchara), parameter                                  &
     &             :: fhd_div_h_flux =        'div_h_flux'
      character(len=kchara), parameter                                  &
     &             :: fhd_div_ph_flux =       'div_part_h_flux'
      character(len=kchara), parameter                                  &
     &             :: fhd_composit_advect =    'composition_advect'
!
!   Energy fluxes
!
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_ene_gen =       'magnetic_ene_generation'
      character(len=kchara), parameter                                  &
     &             :: fhd_work_agst_Lorentz = 'work_against_Lorentz'
      character(len=kchara), parameter                                  &
     &             :: fhd_Lorentz_work =      'Lorentz_work'
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_tension_work =  'mag_tension_work'
      character(len=kchara), parameter                                  &
     &             :: fhd_buoyancy_work =     'buoyancy_work'
      character(len=kchara), parameter                                  &
     &             :: fhd_comp_buo_work =     'composite_buoyancy_flux'
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_buo_work =   'filtered_buoyancy_flux'
!
      character(len=kchara), parameter                                  &
     &             :: fhd_div_SGS_h_flux =    'div_SGS_h_flux'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_m_ene_gen =     'SGS_m_ene_gen'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_temp_gen =      'SGS_temp_gen'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_Lorentz_work =  'SGS_Lorentz_work'
      character(len=kchara), parameter                                  &
     &             :: fhd_Reynolds_work =     'Reynolds_work'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_buo_work =      'SGS_buoyancy_flux'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_comp_buo_work = 'SGS_comp_buoyancy_flux'
!
      character(len=kchara), parameter                                  &
     &             :: fhd_temp_generation =   'temp_generation'
      character(len=kchara), parameter                                  &
     &             :: fhd_part_temp_gen =     'part_temp_gen'
      character(len=kchara), parameter                                  &
     &             :: fhd_vis_ene_diffuse =   'vis_ene_diffuse'
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_ene_diffuse =   'mag_ene_diffuse'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_div_h_flux_true = 'SGS_div_h_flux_true'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_Lorentz_wk_true = 'SGS_Lorentz_work_true'
      character(len=kchara), parameter                                  &
     &             :: fhd_Reynolds_work_true =  'Reynolds_work_true'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_temp_gen_true =   'SGS_temp_gen_true'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_m_ene_gen_true =  'SGS_m_ene_gen_true'
!
!  fluxes by resolved field
!
      character(len=kchara), parameter                                  &
     &             :: fhd_mom_flux =      'momentum_flux'
      character(len=kchara), parameter                                  &
     &             :: fhd_maxwell_t =     'maxwell_tensor'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_m_flux =    'SGS_momentum_flux'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_maxwell_t = 'SGS_maxwell_tensor'
!
      character(len=kchara), parameter                                  &
     &             :: fhd_induct_t =      'induction_tensor'
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_induct_t =  'SGS_induct_tensor'
!
!   work fields
!
      character(len=kchara), parameter                                  &
     &             :: fhd_press_work =      'pressure_work'
      character(len=kchara), parameter                                  &
     &             :: fhd_m_potential_work = 'm_potential_work'
!
      character(len=kchara), parameter :: fhd_SGS_simi =   'SGS_simi'
      character(len=kchara), parameter :: fhd_SGS_grad =   'SGS_grad'
      character(len=kchara), parameter :: fhd_SGS_grad_f = 'SGS_grad_f'
      character(len=kchara), parameter                                  &
     &              :: fhd_SGS_diffuse = 'SGS_diffuse'
!
      character(len=kchara), parameter :: fhd_SGS_temp =   'temp_4_SGS'
      character(len=kchara), parameter :: fhd_SGS_comp =   'comp_4_SGS'
!
!  difference of field
!
      character(len=kchara), parameter :: fhd_grad_v_1 =  'grad_v_1'
      character(len=kchara), parameter :: fhd_grad_v_2 =  'grad_v_2'
      character(len=kchara), parameter :: fhd_grad_v_3 =  'grad_v_3'
!
      character(len=kchara), parameter :: fhd_grad_w_1 =  'grad_w_1'
      character(len=kchara), parameter :: fhd_grad_w_2 =  'grad_w_2'
      character(len=kchara), parameter :: fhd_grad_w_3 =  'grad_w_3'
!
      character(len=kchara), parameter :: fhd_grad_a_1 =  'grad_a_1'
      character(len=kchara), parameter :: fhd_grad_a_2 =  'grad_a_2'
      character(len=kchara), parameter :: fhd_grad_a_3 =  'grad_a_3'
!
      character(len=kchara), parameter :: fhd_grad_b_1 =  'grad_b_1'
      character(len=kchara), parameter :: fhd_grad_b_2 =  'grad_b_2'
      character(len=kchara), parameter :: fhd_grad_b_3 =  'grad_b_3'
!
      character(len=kchara), parameter :: fhd_grad_j_1 =  'grad_j_1'
      character(len=kchara), parameter :: fhd_grad_j_2 =  'grad_j_2'
      character(len=kchara), parameter :: fhd_grad_j_3 =  'grad_j_3'
!
      character(len=kchara), parameter :: fhd_grad_temp = 'grad_temp'
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_par_temp = 'grad_part_temp'
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_ref_temp = 'grad_reference_temp'
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_temp = 'grad_filtered_temp'
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_composit = 'grad_composition'
!
!  wider filtered field
!
      character(len=kchara), parameter                                  &
     &             :: fhd_w_filter_velo = 'wide_filter_velo'
      character(len=kchara), parameter                                  &
     &             :: fhd_w_filter_vecp = 'wide_filter_vecp'
      character(len=kchara), parameter                                  &
     &             :: fhd_w_filter_magne = 'wide_filter_magne'
!
      character(len=kchara), parameter                                  &
     &             :: fhd_w_filter_temp = 'wide_filter_temp'
!
!  divergence of momentum equations
!
      character(len=kchara), parameter                                  &
     &             :: fhd_div_inertia =    'div_inertia'
      character(len=kchara), parameter                                  &
     &             :: fhd_div_Lorentz =    'div_Lorentz_force'
      character(len=kchara), parameter                                  &
     &             :: fhd_div_Coriolis =   'div_Coriolis_force'
      character(len=kchara), parameter                                  &
     &             :: fhd_div_buoyancy =   'div_buoyancy'
      character(len=kchara), parameter                                  &
     &             :: fhd_div_comp_buo =   'div_composite_buoyancy'
      character(len=kchara), parameter                                  &
     &             :: fhd_div_filter_buo = 'div_filtered_buoyancy'
      character(len=kchara), parameter                                  &
     &             :: fhd_div_viscous =    'div_viscousity'
!
!  rotation of momentum equations
!
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_inertia =  'rot_inertia'
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_Lorentz =  'rot_Lorentz_force'
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_Coriolis = 'rot_Coriolis_force'
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_buoyancy = 'rot_buoyancy'
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_comp_buo = 'rot_composite_buoyancy'
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_filter_buo = 'rot_filtered_buoyancy'
!
!  arrays for current forces
!
      character(len=kchara), parameter                                  &
     &             :: fhd_forces =        'sum_forces'
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_forces =    'rot_sum_forces'
      character(len=kchara), parameter                                  &
     &             :: fhd_div_forces =    'div_sum_forces'
!
!  arrays for previous evolution
!
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_mom =        'previous_momentum'
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_uxb =        'previous_induction'
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_heat =       'previous_heat'
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_composit =   'previous_composition'
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_press =      'previous_pressure'
!
!  arrays for evolution check
!
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_mom =       'check_momentum'
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_uxb =       'check_induction'
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_heat =      'check_heat'
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_composit =  'check_composition'
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_press =     'check_pressure'
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_potential = 'check_potential'
!
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_mom_2 =       'check_momentum_2'
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_uxb_2 =       'check_induction_2'
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_heat_2 =      'check_heat_2'
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_composit_2 =  'check_composition_2'
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_press_2 =     'check_pressure_2'
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_potential_2 = 'check_potential_2'
!
!   --------------------------------------------------------------------
!
      character(len=kchara), parameter                                  &
     &             :: fhd_velocity_scale =    'velocity_scale'
      character(len=kchara), parameter                                  &
     &             :: fhd_magnetic_scale =    'magnetic_scale'
      character(len=kchara), parameter                                  &
     &             :: fhd_temp_scale =        'temperature_scale'
      character(len=kchara), parameter                                  &
     &             :: fhd_composiiton_scale = 'composition_scale'
!
!   --------------------------------------------------------------------
!
      character(len=kchara), parameter                                  &
     &             :: thd_heat_flux =  'heat'
      character(len=kchara), parameter                                  &
     &             :: thd_advection =  'inertia'
      character(len=kchara), parameter                                  &
     &             :: thd_lorentz =    'Lorentz'
      character(len=kchara), parameter                                  &
     &             :: thd_coriolis =   'Coriolis'
      character(len=kchara), parameter                                  &
     &             :: thd_induction =  'induction'
      character(len=kchara), parameter                                  &
     &             :: thd_comp_flux =  'comp_flux'
      character(len=kchara), parameter                                  &
     &             :: thd_gravity =    'gravity'
!
!
      end module m_phys_labels
                                                                                                                                                                                                                                                                       