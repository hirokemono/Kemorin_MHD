!>@file   check_MHD_dependency_by_id.f90
!!@brief  module check_MHD_dependency_by_id
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief  Check dependecy of field list fro MHD dynamo
!!
!!@verbatim
!!      subroutine check_dependencies_by_id(cd_prop, iphys, fld)
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: fld
!!@endverbatim
!
      module check_MHD_dependency_by_id
!
      use m_precision
      use m_error_IDs
      use m_phys_labels
!
      use calypso_mpi
!
      use t_physical_property
      use t_phys_address
      use t_phys_data
!
      implicit none
!
      private :: check_missing_field
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_dependencies_by_id(cd_prop, iphys, fld)
!
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: fld
!
      integer(kind = kint) :: i, i_start
!
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(     i_start .eq. iphys%i_filter_velo                        &
     &     .or. i_start .eq. iphys%i_vort                               &
     &     .or. i_start .eq. iphys%i_press                              &
     &     .or. i_start .eq. iphys%i_magne                              &
     &     .or. i_start .eq. iphys%i_temp                               &
     &     .or. i_start .eq. iphys%i_light                              &
     &     .or. i_start .eq. iphys%i_v_diffuse                          &
     &     .or. i_start .eq. iphys%i_square_v) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
        else if(i_start .eq. iphys%i_filter_vort                        &
     &     .or. i_start .eq. iphys%i_velo_scale                         &
     &     .or. i_start .eq. iphys%i_k_heli                             &
     &     .or. i_start .eq. iphys%i_square_w) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_vort, fhd_vort)
!
        else if(i_start .eq. iphys%i_filter_magne                       &
     &     .or. i_start .eq. iphys%i_current                            &
     &     .or. i_start .eq. iphys%i_b_diffuse                          &
     &     .or. i_start .eq. iphys%i_mag_p                              &
     &     .or. i_start .eq. iphys%i_square_b                           &
     &     .or. i_start .eq. iphys%i_truncated_B) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
        else if(i_start .eq. iphys%i_filter_vecp                        &
     &     .or. i_start .eq. iphys%i_scalar_p                           &
     &     .or. i_start .eq. iphys%i_m_heli                             &
     &     .or. i_start .eq. iphys%i_vp_diffuse                         &
     &     .or. i_start .eq. iphys%i_square_a) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_vecp, fhd_vecp)
        else if(i_start .eq. iphys%i_filter_current                     &
     &     .or. i_start .eq. iphys%i_c_heli                             &
     &     .or. i_start .eq. iphys%i_magne_scale                        &
     &     .or. i_start .eq. iphys%i_square_j) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_current, fhd_current)
        else if(i_start .eq. iphys%i_vecp) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_mag_p, fhd_mag_potential)
!
        else if(i_start .eq. iphys%i_t_diffuse                          &
     &     .or. i_start .eq. iphys%i_per_temp                           &
     &     .or. i_start .eq. iphys%i_filter_temp                        &
     &     .or. i_start .eq. iphys%i_heat_source                        &
     &     .or. i_start .eq. iphys%i_square_t) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_temp, fhd_temp)
        else if(i_start .eq. iphys%i_filter_comp                        &
     &     .or. i_start .eq. iphys%i_c_diffuse                          &
     &     .or. i_start .eq. iphys%i_square_c                           &
     &     .or. i_start .eq. iphys%i_light_source) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_light, fhd_light)
        else if(i_start .eq. iphys%i_entropy_source) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_entropy, fhd_entropy)
        end if
      end do
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(i_start .eq. iphys%i_temp_scale) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_t_diffuse, fhd_thermal_diffusion)
        else if(i_start .eq. iphys%i_comp_scale) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_c_diffuse, fhd_c_diffuse)
        else if(i_start .eq. iphys%forces%i_mag_stretch) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%diff_vector%i_grad_vx, grad_v_1%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%diff_vector%i_grad_vy, grad_v_2%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%diff_vector%i_grad_vz, grad_v_3%name)
!
        else if(i_start .eq. iphys%i_c_buo_gen) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_light, fhd_light)
        else if(i_start .eq. iphys%i_temp_gen) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_h_advect, heat_advect%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_temp, fhd_temp)
        else if(i_start .eq. iphys%i_par_t_gen) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_ph_advect, pert_heat_advect%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_temp, fhd_temp)
        else if(i_start .eq. iphys%i_par_c_gen) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_pc_advect,                   &
     &        pert_comp_advect%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_light, fhd_light)
        else if(i_start .eq. iphys%i_per_entropy) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_entropy, fhd_per_entropy)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_ref_entropy, fhd_ref_entropy)
!
        else if(i_start .eq. iphys%i_x_heli) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
        else if(i_start .eq. iphys%i_buo_gen                            &
     &     .or. i_start .eq. iphys%i_entropy) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_temp, fhd_temp)
        end if
      end do
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(     i_start .eq. iphys%i_me_gen) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_induction, magnetic_induction%name)
        else if(i_start .eq. iphys%i_m_tension_wk) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_m_tension, magnetic_tension%name)
        else if(i_start .eq. iphys%i_vis_e_diffuse) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_v_diffuse, fhd_viscous)
        else if(i_start .eq. iphys%i_mag_e_diffuse) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_b_diffuse, fhd_mag_diffuse)
        else if(i_start .eq. iphys%i_ujb                                &
     &     .or. i_start .eq. iphys%i_nega_ujb) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_current, fhd_current)
        else if(i_start .eq. iphys%i_density) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_temp, fhd_temp)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_light, fhd_light)
        else if(i_start .eq. iphys%i_per_density) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_density, fhd_density)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_ref_density, fhd_ref_density)
        else if(i_start .eq. iphys%i_electric                           &
     &     .or. i_start .eq. iphys%i_poynting) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_vp_induct, vecp_induction%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_current, fhd_current)
!
        else if(i_start .eq. iphys%i_div_Lorentz                        &
     &     .or. i_start .eq. iphys%i_rot_Lorentz) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_lorentz, Lorentz_force%name)
        else if(i_start .eq. iphys%i_geostrophic) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_coriolis, Coriolis_force%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_press_grad, pressure_gradient%name)
!
        else if(i_start .eq. iphys%i_m_flux_div) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_m_flux, momentum_flux%name)
        else if(i_start .eq. iphys%i_maxwell_div) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_maxwell, maxwell_tensor%name)
        else if(i_start .eq. iphys%i_induct_div) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_induct_t, induction_tensor%name)
        else if(i_start .eq. iphys%i_h_flux_div) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_h_flux, heat_flux%name)
        else if(i_start .eq. iphys%i_ph_flux_div) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_ph_flux, pert_heat_flux%name)
        else if(i_start .eq. iphys%i_c_flux_div) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_c_flux, composite_flux%name)
        else if(i_start .eq. iphys%i_pc_flux_div) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_pc_flux, pert_comp_flux%name)
!
        else if(i_start .eq. iphys%i_SGS_div_hf_true) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_velo, fhd_filter_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_temp, fhd_filter_temp)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_h_flux_div, fhd_div_h_flux)
        else if(i_start .eq. iphys%i_SGS_div_cf_true) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_velo, fhd_filter_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_comp, fhd_filter_comp)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_c_flux_div, fhd_div_c_flux)
        else if(i_start .eq. iphys%i_SGS_div_mf_true) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_velo, fhd_filter_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_m_flux_div, fhd_div_m_flux)
        else if(i_start .eq. iphys%i_SGS_Lor_true) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_magne, fhd_filter_magne)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_maxwell_div, fhd_div_maxwell_t)
        else if(i_start .eq. iphys%i_SGS_idct_true) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_velo, fhd_filter_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_magne, fhd_filter_magne)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_induct_div, fhd_div_induct_t)
        else if(i_start .eq. iphys%i_SGS_Lor_wk_tr) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_Lor_true, fhd_SGS_Lorentz_true)
        else if(i_start .eq. iphys%i_reynolds_wk_tr) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_div_mf_true,                    &
     &        fhd_SGS_div_m_flux_true)
        else if(i_start .eq. iphys%i_SGS_t_gen_tr) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_temp, fhd_temp)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_div_hf_true,                    &
     &        fhd_SGS_div_h_flux_true)
        else if(i_start .eq. iphys%i_SGS_c_gen_tr) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_light, fhd_light)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_div_cf_true,                    &
     &        fhd_SGS_div_c_flux_true)
        else if(i_start .eq. iphys%i_SGS_idct_true) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
        end if
      end do
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(i_start .eq. iphys%i_Csim_SGS_h_flux) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%SGS_term%i_SGS_h_flux, SGS_heat_flux%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%wide_SGS%i_SGS_h_flux,                    &
     &        wide_SGS_heat_flux%name)
        else if(i_start .eq. iphys%i_Csim_SGS_c_flux) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%SGS_term%i_SGS_c_flux, SGS_composit_flux%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%wide_SGS%i_SGS_c_flux,                &
     &        wide_SGS_composit_flux%name)
        else if(i_start .eq. iphys%i_Csim_SGS_m_flux) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%SGS_term%i_SGS_inertia, SGS_inertia%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%wide_SGS%i_SGS_inertia,                   &
     &        wide_SGS_inertia%name)
        else if(i_start .eq. iphys%i_Csim_SGS_Lorentz) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%SGS_term%i_SGS_Lorentz, SGS_Lorentz%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%wide_SGS%i_SGS_Lorentz,               &
     &        wide_SGS_Lorentz%name)
        else if(i_start .eq. iphys%i_Csim_SGS_induction) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%SGS_term%i_SGS_induction, SGS_vecp_induction%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%wide_SGS%i_SGS_induction,             &
     &       wide_SGS_vp_induction%name)
        else if(i_start .eq. iphys%i_Csim_SGS_buoyancy) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%SGS_term%i_SGS_h_flux, SGS_heat_flux%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
        else if(i_start .eq. iphys%i_Csim_SGS_comp_buo) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%SGS_term%i_SGS_c_flux, SGS_composit_flux%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
!
        else if(i_start .eq. iphys%i_h_flux_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_h_flux, heat_flux%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%SGS_term%i_SGS_h_flux, SGS_heat_flux%name)
        else if(i_start .eq. iphys%i_c_flux_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_c_flux, composite_flux%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%SGS_term%i_SGS_c_flux, SGS_composit_flux%name)
        else if(i_start .eq. iphys%i_inertia_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_m_advect, inertia%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%SGS_term%i_SGS_inertia, SGS_inertia%name)
        else if(i_start .eq. iphys%i_Lorentz_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_lorentz, Lorentz_force%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%SGS_term%i_SGS_Lorentz, SGS_Lorentz%name)
        else if(i_start .eq. iphys%i_Lorentz_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_lorentz, Lorentz_force%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%SGS_term%i_SGS_Lorentz, SGS_Lorentz%name)
        else if(i_start .eq. iphys%i_vp_induct_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_vp_induct, vecp_induction%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%SGS_term%i_SGS_induction, SGS_vecp_induction%name)
        else if(i_start .eq. iphys%i_mag_induct_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_induction, magnetic_induction%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%rot_SGS%i_SGS_induction, SGS_induction%name)
        else if(i_start .eq. iphys%i_mom_flux_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_m_flux, momentum_flux%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%SGS_term%i_SGS_m_flux, SGS_momentum_flux%name)
        else if(i_start .eq. iphys%i_maxwell_t_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_maxwell, maxwell_tensor%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%SGS_term%i_SGS_maxwell, SGS_maxwell_tensor%name)
        end if
      end do
!
      end subroutine check_dependencies_by_id
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_missing_field(fld, iphys_tgt, iphys_ref, name)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: iphys_tgt, iphys_ref
      character(len = kchara), intent(in) :: name
!
      if(iphys_ref .gt. 0) return
      write(*,*) 'Following fields are required for ',                  &
     &     trim(field_name_by_address(fld, iphys_tgt)),                 &
     &     ': ', trim(name)
      call calypso_MPI_abort(ierr_fld,'Stop program.')
!
      end subroutine check_missing_field
!
! -----------------------------------------------------------------------
!
     end module check_MHD_dependency_by_id
