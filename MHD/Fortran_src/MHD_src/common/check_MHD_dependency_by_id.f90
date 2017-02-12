!>@file   check_MHD_dependency_by_id.f90
!!@brief  module check_MHD_dependency_by_id
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief  Check dependecy of field list fro MHD dynamo
!!
!!@verbatim
!      subroutine check_dependencies_by_id(iphys, fld)
!      subroutine check_dependence_FEM_MHD_by_id(iphys, fld)
!      subroutine check_dependence_SPH_MHD_by_id(iphys, fld)
!!@endverbatim
!
      module check_MHD_dependency_by_id
!
      use m_precision
      use m_error_IDs
!
      use calypso_mpi
      use m_control_parameter
!
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
      subroutine check_dependencies_by_id(iphys, fld)
!
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: fld
!
      integer(kind = kint) :: i, i_start
!
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(     i_start .eq. iphys%i_filter_velo                        &
     &     .or. i_start .eq. iphys%i_wide_fil_velo                      &
     &     .or. i_start .eq. iphys%i_vort                               &
     &     .or. i_start .eq. iphys%i_press                              &
     &     .or. i_start .eq. iphys%i_magne                              &
     &     .or. i_start .eq. iphys%i_temp                               &
     &     .or. i_start .eq. iphys%i_light                              &
     &     .or. i_start .eq. iphys%i_k_heli                             &
     &     .or. i_start .eq. iphys%i_v_diffuse                          &
     &     .or. i_start .eq. iphys%i_m_advect                           &
     &     .or. i_start .eq. iphys%i_m_flux                             &
     &     .or. i_start .eq. iphys%i_coriolis                           &
     &     .or. i_start .eq. iphys%i_SGS_m_flux                         &
     &     .or. i_start .eq. iphys%i_grad_vx                            &
     &     .or. i_start .eq. iphys%i_grad_vy                            &
     &     .or. i_start .eq. iphys%i_grad_vz) then
          call check_missing_field(fld, i_start, iphys%i_velo)
        else if(i_start .eq. iphys%i_filter_vort                        &
     &     .or. i_start .eq. iphys%i_wide_fil_vort                      &
     &     .or. i_start .eq. iphys%i_k_heli                             &
     &     .or. i_start .eq. iphys%i_SGS_inertia                        &
     &     .or. i_start .eq. iphys%i_wide_SGS_inertia                   &
     &     .or. i_start .eq. iphys%i_velo_scale) then 
          call check_missing_field(fld, i_start, iphys%i_vort)
        else if(i_start .eq. iphys%i_press_grad) then
          call check_missing_field(fld, i_start, iphys%i_press)
!
        else if(i_start .eq. iphys%i_filter_magne                       &
     &     .or. i_start .eq. iphys%i_wide_fil_magne                     &
     &     .or. i_start .eq. iphys%i_current                            &
     &     .or. i_start .eq. iphys%i_b_diffuse                          &
     &     .or. i_start .eq. iphys%i_mag_p                              &
     &     .or. i_start .eq. iphys%i_m_tension                          &
     &     .or. i_start .eq. iphys%i_lorentz                            &
     &     .or. i_start .eq. iphys%i_maxwell                            &
     &     .or. i_start .eq. iphys%i_SGS_maxwell) then 
          call check_missing_field(fld, i_start, iphys%i_magne)
        else if(i_start .eq. iphys%i_filter_vecp                        &
     &     .or. i_start .eq. iphys%i_scalar_p                           &
     &     .or. i_start .eq. iphys%i_m_heli                             &
     &     .or. i_start .eq. iphys%i_vp_diffuse) then
          call check_missing_field(fld, i_start, iphys%i_vecp)
        else if(i_start .eq. iphys%i_filter_current                     &
     &     .or. i_start .eq. iphys%i_wide_fil_current                   &
     &     .or. i_start .eq. iphys%i_c_heli                             &
     &     .or. i_start .eq. iphys%i_magne_scale) then 
          call check_missing_field(fld, i_start, iphys%i_current)
        else if(i_start .eq. iphys%i_vecp) then
          call check_missing_field(fld, i_start, iphys%i_velo)
          call check_missing_field(fld, i_start, iphys%i_magne)
          call check_missing_field(fld, i_start, iphys%i_mag_p)
!
        else if(i_start .eq. iphys%i_t_diffuse                          &
     &     .or. i_start .eq. iphys%i_par_temp                           &
     &     .or. i_start .eq. iphys%i_filter_temp                        &
     &     .or. i_start .eq. iphys%i_wide_fil_temp                      &
     &     .or. i_start .eq. iphys%i_buoyancy                           &
     &     .or. i_start .eq. iphys%i_heat_source                        &
     &     .or. i_start .eq. iphys%i_grad_t) then 
          call check_missing_field(fld, i_start, iphys%i_temp)
        else if(i_start .eq. iphys%i_grad_composit                      &
     &     .or. i_start .eq. iphys%i_filter_comp                        &
     &     .or. i_start .eq. iphys%i_wide_fil_comp                      &
     &     .or. i_start .eq. iphys%i_comp_buo                           &
     &     .or. i_start .eq. iphys%i_c_diffuse                          &
     &     .or. i_start .eq. iphys%i_light_source) then 
          call check_missing_field(fld, i_start, iphys%i_light)
        else if(i_start .eq. iphys%i_entropy_source) then 
          call check_missing_field(fld, i_start, iphys%i_entropy)
        end if
      end do
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(     i_start .eq. iphys%i_filter_buo) then 
          call check_missing_field(fld, i_start, iphys%i_filter_temp)
        else if(i_start .eq. iphys%i_temp_scale) then 
          call check_missing_field(fld, i_start, iphys%i_t_diffuse)
        else if(i_start .eq. iphys%i_comp_scale) then 
          call check_missing_field(fld, i_start, iphys%i_c_diffuse)
        else if(i_start .eq. iphys%i_comp_scale) then 
          call check_missing_field(fld, i_start, iphys%i_magne)
          call check_missing_field(fld, i_start, iphys%i_grad_vx)
          call check_missing_field(fld, i_start, iphys%i_grad_vy)
          call check_missing_field(fld, i_start, iphys%i_grad_vz)
!
        else if(i_start .eq. iphys%i_c_advect                           &
     &     .or. i_start .eq. iphys%i_pc_advect                          &
     &     .or. i_start .eq. iphys%i_c_flux                             &
     &     .or. i_start .eq. iphys%i_pc_flux                            &
     &     .or. i_start .eq. iphys%i_c_buo_gen                          &
     &     .or. i_start .eq. iphys%i_SGS_c_flux) then 
          call check_missing_field(fld, i_start, iphys%i_velo)
          call check_missing_field(fld, i_start, iphys%i_light)
        else if(i_start .eq. iphys%i_f_buo_gen) then 
          call check_missing_field(fld, i_start, iphys%i_velo)
          call check_missing_field(fld, i_start, iphys%i_filter_temp)
        else if(i_start .eq. iphys%i_temp_gen) then 
          call check_missing_field(fld, i_start, iphys%i_h_advect)
          call check_missing_field(fld, i_start, iphys%i_temp)
        else if(i_start .eq. iphys%i_par_t_gen) then 
          call check_missing_field(fld, i_start, iphys%i_ph_advect)
          call check_missing_field(fld, i_start, iphys%i_temp)
        else if(i_start .eq. iphys%i_par_c_gen) then 
          call check_missing_field(fld, i_start, iphys%i_pc_advect)
          call check_missing_field(fld, i_start, iphys%i_light)
        else if(i_start .eq. iphys%i_par_entropy) then 
          call check_missing_field(fld, i_start, iphys%i_entropy)
          call check_missing_field(fld, i_start, iphys%i_ref_entropy)
!
        else if(i_start .eq. iphys%i_x_heli                             &
     &     .or. i_start .eq. iphys%i_vp_induct                          &
     &     .or. i_start .eq. iphys%i_induct_t                           &
     &     .or. i_start .eq. iphys%i_wide_SGS_vp_induct                 &
     &     .or. i_start .eq. iphys%i_SGS_vp_induct                      &
     &     .or. i_start .eq. iphys%i_SGS_induct_t) then 
          call check_missing_field(fld, i_start, iphys%i_velo)
          call check_missing_field(fld, i_start, iphys%i_magne)
        else if(i_start .eq. iphys%i_buo_gen                            &
     &     .or. i_start .eq. iphys%i_h_flux                             &
     &     .or. i_start .eq. iphys%i_ph_flux                            &
     &     .or. i_start .eq. iphys%i_SGS_h_flux                         &
     &     .or. i_start .eq. iphys%i_wide_SGS_h_flux                    &
     &     .or. i_start .eq. iphys%i_entropy                            &
     &     .or. i_start .eq. iphys%i_SGS_temp_gen                       &
     &     .or. i_start .eq. iphys%i_ph_advect                          &
     &     .or. i_start .eq. iphys%i_h_advect) then 
          call check_missing_field(fld, i_start, iphys%i_velo)
          call check_missing_field(fld, i_start, iphys%i_temp)
        else if(i_start .eq. iphys%i_induction) then 
          if (evo_magne%iflag_scheme .gt. id_no_evolution) then
            call check_missing_field(fld, i_start, iphys%i_velo)
            call check_missing_field(fld, i_start, iphys%i_magne)
          else
            call check_missing_field(fld, i_start, iphys%i_vp_induct)
          end if
        else if(i_start .eq. iphys%i_SGS_induction) then 
          if (evo_magne%iflag_scheme .gt. id_no_evolution) then
            call check_missing_field(fld, i_start, iphys%i_velo)
            call check_missing_field(fld, i_start, iphys%i_magne)
          else
            call check_missing_field                                    &
     &         (fld, i_start, iphys%i_SGS_vp_induct)
          end if
        end if
      end do
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(     i_start .eq. iphys%i_me_gen) then 
          call check_missing_field(fld, i_start, iphys%i_magne)
          call check_missing_field(fld, i_start, iphys%i_induction)
        else if(i_start .eq. iphys%i_m_tension_wk) then 
          call check_missing_field(fld, i_start, iphys%i_velo)
          call check_missing_field(fld, i_start, iphys%i_m_tension)
        else if(i_start .eq. iphys%i_vis_e_diffuse) then 
          call check_missing_field(fld, i_start, iphys%i_velo)
          call check_missing_field(fld, i_start, iphys%i_v_diffuse)
        else if(i_start .eq. iphys%i_mag_e_diffuse) then 
          call check_missing_field(fld, i_start, iphys%i_magne)
          call check_missing_field(fld, i_start, iphys%i_b_diffuse)
        else if(i_start .eq. iphys%i_ujb                                &
     &     .or. i_start .eq. iphys%i_nega_ujb                           &
     &     .or. i_start .eq. iphys%i_SGS_Lor_wk) then 
          call check_missing_field(fld, i_start, iphys%i_velo)
          call check_missing_field(fld, i_start, iphys%i_magne)
          call check_missing_field(fld, i_start, iphys%i_current)
        else if(i_start .eq. iphys%i_density) then 
          call check_missing_field(fld, i_start, iphys%i_velo)
          call check_missing_field(fld, i_start, iphys%i_temp)
          call check_missing_field(fld, i_start, iphys%i_light)
        else if(i_start .eq. iphys%i_par_density) then 
          call check_missing_field(fld, i_start, iphys%i_density)
          call check_missing_field(fld, i_start, iphys%i_ref_density)
        else if(i_start .eq. iphys%i_electric                           &
     &     .or. i_start .eq. iphys%i_poynting) then 
          call check_missing_field(fld, i_start, iphys%i_vp_induct)
          call check_missing_field(fld, i_start, iphys%i_current)
!
        else if(i_start .eq. iphys%i_div_Lorentz                        &
     &     .or. i_start .eq. iphys%i_rot_Lorentz) then 
          call check_missing_field(fld, i_start, iphys%i_lorentz)
        else if(i_start .eq. iphys%i_geostrophic) then 
          call check_missing_field(fld, i_start, iphys%i_coriolis)
          call check_missing_field(fld, i_start, iphys%i_press_grad)
!
        else if(i_start .eq. iphys%i_m_flux_div) then 
          call check_missing_field(fld, i_start, iphys%i_m_flux)
        else if(i_start .eq. iphys%i_maxwell_div) then 
          call check_missing_field(fld, i_start, iphys%i_maxwell)
        else if(i_start .eq. iphys%i_induct_div) then 
          call check_missing_field(fld, i_start, iphys%i_induct_t)
        else if(i_start .eq. iphys%i_h_flux_div) then 
          call check_missing_field(fld, i_start, iphys%i_h_flux)
        else if(i_start .eq. iphys%i_ph_flux_div) then 
          call check_missing_field(fld, i_start, iphys%i_ph_flux)
        else if(i_start .eq. iphys%i_c_flux_div) then 
          call check_missing_field(fld, i_start, iphys%i_c_flux)
        else if(i_start .eq. iphys%i_pc_flux_div) then 
          call check_missing_field(fld, i_start, iphys%i_pc_flux)
!
        else if(i_start .eq. iphys%i_SGS_div_hf_true) then 
          call check_missing_field(fld, i_start, iphys%i_filter_velo)
          call check_missing_field(fld, i_start, iphys%i_filter_temp)
          call check_missing_field(fld, i_start, iphys%i_h_flux_div)
        else if(i_start .eq. iphys%i_SGS_div_cf_true) then 
          call check_missing_field(fld, i_start, iphys%i_filter_velo)
          call check_missing_field(fld, i_start, iphys%i_filter_comp)
          call check_missing_field(fld, i_start, iphys%i_c_flux_div)
        else if(i_start .eq. iphys%i_SGS_div_mf_true) then 
          call check_missing_field(fld, i_start, iphys%i_filter_velo)
          call check_missing_field(fld, i_start, iphys%i_m_flux_div)
        else if(i_start .eq. iphys%i_SGS_Lor_true) then 
          call check_missing_field(fld, i_start, iphys%i_filter_magne)
          call check_missing_field(fld, i_start, iphys%i_maxwell_div)
        else if(i_start .eq. iphys%i_SGS_idct_true) then 
          call check_missing_field(fld, i_start, iphys%i_filter_velo)
          call check_missing_field(fld, i_start, iphys%i_filter_magne)
          call check_missing_field(fld, i_start, iphys%i_induct_div)
        else if(i_start .eq. iphys%i_SGS_Lor_wk_tr) then 
          call check_missing_field(fld, i_start, iphys%i_velo)
          call check_missing_field(fld, i_start, iphys%i_SGS_Lor_true)
        else if(i_start .eq. iphys%i_reynolds_wk_tr) then 
          call check_missing_field(fld, i_start, iphys%i_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_div_mf_true)
        else if(i_start .eq. iphys%i_SGS_t_gen_tr) then 
          call check_missing_field(fld, i_start, iphys%i_temp)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_div_hf_true)
        else if(i_start .eq. iphys%i_SGS_c_gen_tr) then 
          call check_missing_field(fld, i_start, iphys%i_light)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_div_cf_true)
        else if(i_start .eq. iphys%i_SGS_idct_true) then 
          call check_missing_field(fld, i_start, iphys%i_magne)
          call check_missing_field(fld, i_start, iphys%i_SGS_idct_true)
        end if
      end do
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(     i_start .eq. iphys%i_SGS_rot_inertia                    &
     &     .or. i_start .eq. iphys%i_SGS_div_inertia) then 
          call check_missing_field(fld, i_start, iphys%i_SGS_inertia)
        else if(i_start .eq. iphys%i_SGS_div_c_flux                     &
     &     .or. i_start .eq. iphys%i_wide_SGS_c_flux) then 
          call check_missing_field(fld, i_start, iphys%i_SGS_c_flux)
        else if(i_start .eq. iphys%i_SGS_me_gen) then 
          call check_missing_field(fld, i_start, iphys%i_SGS_induction)
        else if(i_start .eq. iphys%i_SGS_div_m_flux) then
          call check_missing_field(fld, i_start, iphys%i_SGS_m_flux)
        else if(i_start .eq. iphys%i_SGS_Lorentz                        &
     &     .or. i_start .eq. iphys%i_wide_SGS_Lorentz) then
          call check_missing_field(fld, i_start, iphys%i_magne)
        else if(i_start .eq. iphys%i_SGS_rot_Lorentz                    &
     &     .or. i_start .eq. iphys%i_SGS_div_Lorentz) then
          call check_missing_field(fld, i_start, iphys%i_SGS_Lorentz)
!
        else if(i_start .eq. iphys%i_SGS_buoyancy                       &
     &     .or. i_start .eq. iphys%i_SGS_buo_wk) then 
          call check_missing_field(fld, i_start, iphys%i_velo)
          call check_missing_field(fld, i_start, iphys%i_SGS_h_flux)
        else if(i_start .eq. iphys%i_SGS_comp_buo                       &
     &     .or. i_start .eq. iphys%i_SGS_comp_buo_wk) then 
          call check_missing_field(fld, i_start, iphys%i_velo)
          call check_missing_field(fld, i_start, iphys%i_SGS_c_flux)
!
        else if(i_start .eq. iphys%i_Csim_SGS_h_flux) then 
          call check_missing_field(fld, i_start, iphys%i_SGS_h_flux)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_wide_SGS_h_flux)
        else if(i_start .eq. iphys%i_Csim_SGS_c_flux) then 
          call check_missing_field(fld, i_start, iphys%i_SGS_c_flux)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_wide_SGS_c_flux)
        else if(i_start .eq. iphys%i_Csim_SGS_m_flux) then 
          call check_missing_field(fld, i_start, iphys%i_SGS_inertia)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_wide_SGS_inertia)
        else if(i_start .eq. iphys%i_Csim_SGS_Lorentz) then 
          call check_missing_field(fld, i_start, iphys%i_SGS_Lorentz)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_wide_SGS_Lorentz)
        else if(i_start .eq. iphys%i_Csim_SGS_induction) then 
          call check_missing_field(fld, i_start, iphys%i_SGS_vp_induct)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_wide_SGS_vp_induct)
        else if(i_start .eq. iphys%i_Csim_SGS_buoyancy) then 
          call check_missing_field(fld, i_start, iphys%i_SGS_h_flux)
          call check_missing_field(fld, i_start, iphys%i_velo)
        else if(i_start .eq. iphys%i_Csim_SGS_comp_buo) then 
          call check_missing_field(fld, i_start, iphys%i_SGS_c_flux)
          call check_missing_field(fld, i_start, iphys%i_velo)
!
        else if(i_start .eq. iphys%i_h_flux_w_sgs) then 
          call check_missing_field(fld, i_start, iphys%i_h_flux)
          call check_missing_field(fld, i_start, iphys%i_SGS_h_flux)
        else if(i_start .eq. iphys%i_c_flux_w_sgs) then 
          call check_missing_field(fld, i_start, iphys%i_c_flux)
          call check_missing_field(fld, i_start, iphys%i_SGS_c_flux)
        else if(i_start .eq. iphys%i_inertia_w_sgs) then 
          call check_missing_field(fld, i_start, iphys%i_m_advect)
          call check_missing_field(fld, i_start, iphys%i_SGS_inertia)
        else if(i_start .eq. iphys%i_Lorentz_w_sgs) then 
          call check_missing_field(fld, i_start, iphys%i_lorentz)
          call check_missing_field(fld, i_start, iphys%i_SGS_Lorentz)
        else if(i_start .eq. iphys%i_Lorentz_w_sgs) then 
          call check_missing_field(fld, i_start, iphys%i_lorentz)
          call check_missing_field(fld, i_start, iphys%i_SGS_Lorentz)
        else if(i_start .eq. iphys%i_vp_induct_w_sgs) then 
          call check_missing_field(fld, i_start, iphys%i_vp_induct)
          call check_missing_field(fld, i_start, iphys%i_SGS_vp_induct)
        else if(i_start .eq. iphys%i_mag_induct_w_sgs) then 
          call check_missing_field(fld, i_start, iphys%i_induction)
          call check_missing_field(fld, i_start, iphys%i_SGS_induction)
        else if(i_start .eq. iphys%i_mom_flux_w_sgs) then 
          call check_missing_field(fld, i_start, iphys%i_m_flux)
          call check_missing_field(fld, i_start, iphys%i_SGS_m_flux)
        else if(i_start .eq. iphys%i_maxwell_t_w_sgs) then 
          call check_missing_field(fld, i_start, iphys%i_maxwell)
          call check_missing_field(fld, i_start, iphys%i_SGS_maxwell)
        end if
      end do
!
      end subroutine check_dependencies_by_id
!
! -----------------------------------------------------------------------
!
      subroutine check_dependence_FEM_MHD_by_id(iphys, fld)
!
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: fld
!
      integer(kind = kint) :: i, i_start
!
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(i_start .eq. iphys%i_reynolds_wk) then
          call check_missing_field(fld, i_start, iphys%i_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_div_m_flux)
        end if
      end do
!
      end subroutine check_dependence_FEM_MHD_by_id
!
! -----------------------------------------------------------------------
!
      subroutine check_dependence_SPH_MHD_by_id(iphys, fld)
!
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: fld
!
      integer(kind = kint) :: i, i_start
!
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(i_start .eq. iphys%i_reynolds_wk) then
          call check_missing_field(fld, i_start, iphys%i_velo)
          call check_missing_field(fld, i_start, iphys%i_SGS_inertia)
        end if
      end do
!
      end subroutine check_dependence_SPH_MHD_by_id
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_missing_field(fld, iphys_tgt, iphys_ref)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: iphys_tgt, iphys_ref
!
      if(iphys_ref .gt. 0) return
      write(*,*) 'Following fields are required for ',                  &
     &     trim(field_name_by_address(fld, iphys_tgt)),                 &
     &     ': ', trim(field_name_by_address(fld, iphys_ref))
      call calypso_MPI_abort(ierr_fld,'Stop program.')
!
      end subroutine check_missing_field
!
! -----------------------------------------------------------------------
!
     end module check_MHD_dependency_by_id
