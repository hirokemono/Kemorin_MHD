!>@file   cal_sph_rot_filtered_force.f90
!!@brief  module cal_sph_rot_filtered_force
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate curl or divergence of forces
!!
!!@verbatim
!!      subroutine rot_filtered_mom_eq_exp_sph                          &
!!     &         (sph_rj, r_2nd, MHD_prop, sph_MHD_bc, leg,             &
!!     &          ipol_frc, ipol_rot_frc, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(base_force_address), intent(in) :: ipol_rot_frc
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine cal_div_of_filter_forces_sph_2                       &
!!     &         (sph_rj, r_2nd, MHD_prop, sph_MHD_bc, g_sph_rj,        &
!!     &          ipol_frc, ipol_div_frc, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(base_force_address), intent(in) :: ipol_div_frc
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module cal_sph_rot_filtered_force
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_control_parameter
      use t_spheric_rj_data
      use t_base_force_labels
!      use t_base_field_labels
!      use t_grad_field_labels
      use t_phys_data
      use t_fdm_coefs
      use t_schmidt_poly_on_rtm
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_coef_fdm2_MHD_boundaries
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine rot_filtered_mom_eq_exp_sph                            &
     &         (sph_rj, r_2nd, MHD_prop, sph_MHD_bc, leg,               &
     &          ipol_frc, ipol_rot_frc, rj_fld)
!
      use cal_sph_field_by_rotation
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(legendre_4_sph_trans), intent(in) :: leg
      type(base_force_address), intent(in) :: ipol_frc
      type(base_force_address), intent(in) :: ipol_rot_frc
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'cal_rot_of_forces_sph_2'
      call cal_rot_of_forces_sph_2                                      &
     &   (MHD_prop%fl_prop%iflag_4_filter_inertia,                      &
     &    MHD_prop%fl_prop%iflag_4_filter_lorentz,                      &
     &    sph_rj, r_2nd, leg%g_sph_rj, sph_MHD_bc%sph_bc_U,             &
     &    sph_MHD_bc%fdm2_free_ICB, sph_MHD_bc%fdm2_free_CMB,           &
     &    ipol_frc, ipol_rot_frc, rj_fld)
!
      call cal_rot_of_induction_sph                                     &
     &   (MHD_prop%cd_prop%iflag_4_filter_induction,                    &
     &    sph_rj, r_2nd, leg%g_sph_rj, sph_MHD_bc%sph_bc_B,             &
     &    ipol_frc, rj_fld)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'cal_div_of_fluxes_sph'
      call cal_div_of_fluxes_sph                                        &
     &   (MHD_prop%ht_prop%iflag_4_filter_advection,                    &
     &    MHD_prop%cp_prop%iflag_4_filter_advection,                    &
     &    sph_rj, r_2nd, leg%g_sph_rj,                                  &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%bcs_T,                        &
     &    sph_MHD_bc%sph_bc_C, sph_MHD_bc%bcs_C,                        &
     &    sph_MHD_bc%fdm2_center, ipol_frc, rj_fld)
!
      end subroutine rot_filtered_mom_eq_exp_sph
!
! ----------------------------------------------------------------------
!
      subroutine cal_div_of_filter_forces_sph_2                         &
     &         (sph_rj, r_2nd, MHD_prop, sph_MHD_bc, g_sph_rj,          &
     &          ipol_frc, ipol_div_frc, rj_fld)
!     &          ipol_base, ipol_grad, ipol_frc, ipol_div_frc, rj_fld)
!
      use const_sph_divergence
      use cal_sph_divergence_of_force
!      use div_self_buoyancies_sph
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!      type(base_field_address), intent(in) :: ipol_base
!      type(gradient_field_address), intent(in) :: ipol_grad
      type(base_force_address), intent(in) :: ipol_frc
      type(base_force_address), intent(in) :: ipol_div_frc
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call const_sph_div_force                                          &
     &   (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, g_sph_rj,                 &
     &    ipol_frc%i_m_advect, ipol_div_frc%i_m_advect, rj_fld)
!
      if(MHD_prop%fl_prop%iflag_4_lorentz) then
        call const_sph_div_force                                        &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, g_sph_rj,               &
     &      ipol_frc%i_lorentz, ipol_div_frc%i_lorentz, rj_fld)
      end if
!
      call cal_div_of_buoyancies_sph_2(sph_rj, r_2nd, MHD_prop,         &
     &    sph_MHD_bc, g_sph_rj, ipol_frc, ipol_div_frc, rj_fld)
!
!      call sel_div_buoyancies_sph_MHD                                  &
!     &   (sph_rj, ipol_base, ipol_grad, ipol_div_frc,                  &
!     &    MHD_prop%fl_prop, MHD_prop%ref_param_T, MHD_prop%ref_param_C,&
!     &    sph_MHD_bc%sph_bc_U, rj_fld)
!
      end subroutine cal_div_of_filter_forces_sph_2
!
! -----------------------------------------------------------------------
!
      end module cal_sph_rot_filtered_force
