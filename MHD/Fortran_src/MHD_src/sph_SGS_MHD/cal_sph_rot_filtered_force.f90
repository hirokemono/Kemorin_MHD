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
!!     &          ipol_fil_frc, ipol_rot_fil_frc, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(base_force_address), intent(in) :: ipol_fil_frc
!!        type(base_force_address), intent(in) :: ipol_rot_fil_frc
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine cal_div_of_filter_forces_sph_2                       &
!!     &         (sph_rj, r_2nd, MHD_prop, sph_MHD_bc, g_sph_rj,        &
!!     &          ipol_fil_frc, ipol_div_fil_frc, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(base_force_address), intent(in) :: ipol_fil_frc
!!        type(base_force_address), intent(in) :: ipol_div_fil_frc
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine const_radial_fil_forces_on_bc(sph_rj, g_sph_rj,      &
!!     &          fl_prop, sph_bc_U, ref_param_T, ref_param_C,          &
!!     &          ipol_fil, ipol_fil_frc, ipol_div_fil_frc, rj_fld)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(base_field_address), intent(in) :: ipol_fil
!!        type(base_force_address), intent(in) :: ipol_fil_frc
!!        type(base_force_address), intent(in) :: ipol_div_fil_frc
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
     &          ipol_fil_frc, ipol_rot_fil_frc, rj_fld)
!
      use cal_sph_field_by_rotation
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(legendre_4_sph_trans), intent(in) :: leg
      type(base_force_address), intent(in) :: ipol_fil_frc
      type(base_force_address), intent(in) :: ipol_rot_fil_frc
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
     &    ipol_fil_frc, ipol_rot_fil_frc, rj_fld)
!
      call cal_rot_of_induction_sph                                     &
     &   (MHD_prop%cd_prop%iflag_4_filter_induction,                    &
     &    sph_rj, r_2nd, leg%g_sph_rj, sph_MHD_bc%sph_bc_B,             &
     &    ipol_fil_frc, rj_fld)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'cal_div_of_fluxes_sph'
      call cal_div_of_fluxes_sph                                        &
     &   (MHD_prop%ht_prop%iflag_4_filter_advection,                    &
     &    MHD_prop%cp_prop%iflag_4_filter_advection,                    &
     &    sph_rj, r_2nd, leg%g_sph_rj,                                  &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%bcs_T,                        &
     &    sph_MHD_bc%sph_bc_C, sph_MHD_bc%bcs_C,                        &
     &    sph_MHD_bc%fdm2_center, ipol_fil_frc, rj_fld)
!
      end subroutine rot_filtered_mom_eq_exp_sph
!
! ----------------------------------------------------------------------
!
      subroutine cal_div_of_filter_forces_sph_2                         &
     &         (sph_rj, r_2nd, MHD_prop, sph_MHD_bc, g_sph_rj,          &
     &          ipol_fil_frc, ipol_div_fil_frc, rj_fld)
!     &          ipol_fil, ipol_graa_fil, ipol_fil_frc,                 &
!     &          ipol_div_fil_frc, rj_fld)
!
      use const_sph_divergence
      use cal_sph_divergence_of_force
!      use div_self_buoyancies_sph
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!      type(base_field_address), intent(in) :: ipol_fil
!      type(gradient_field_address), intent(in) :: ipol_graa_fil
      type(base_force_address), intent(in) :: ipol_fil_frc
      type(base_force_address), intent(in) :: ipol_div_fil_frc
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(MHD_prop%fl_prop%iflag_4_filter_inertia) then
        call const_sph_div_force                                        &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, g_sph_rj,               &
     &      ipol_fil_frc%i_m_advect, ipol_div_fil_frc%i_m_advect,       &
     &      rj_fld)
      end if
!
      if(MHD_prop%fl_prop%iflag_4_filter_lorentz) then
        call const_sph_div_force                                        &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, g_sph_rj,               &
     &      ipol_fil_frc%i_lorentz, ipol_div_fil_frc%i_lorentz, rj_fld)
      end if
!
      call cal_div_of_buoyancies_sph_2                                  &
     &   (MHD_prop%fl_prop%iflag_4_filter_gravity,                      &
     &    MHD_prop%fl_prop%iflag_4_filter_comp_buo,                     &
     &    sph_rj, r_2nd, sph_MHD_bc, g_sph_rj,                          &
     &    ipol_fil_frc, ipol_div_fil_frc, rj_fld)
!
!      call sel_div_buoyancies_sph_MHD                                  &
!     &   (MHD_prop%fl_prop%iflag_4_filter_gravity,                     &
!     &    MHD_prop%fl_prop%iflag_4_filter_comp_buo,                    &
!     &    sph_rj, ipol_fil, ipol_graa_fil, ipol_div_fil_frc,           &
!     &    MHD_prop%fl_prop%coef_buo,  MHD_prop%fl_prop%coef_comp_buo,  &
!     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                  &
!     &    sph_MHD_bc%sph_bc_U, rj_fld)
!
      end subroutine cal_div_of_filter_forces_sph_2
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_fil_forces_on_bc(sph_rj, g_sph_rj,        &
     &          fl_prop, sph_bc_U, ref_param_T, ref_param_C,            &
     &          ipol_fil, ipol_fil_frc, ipol_div_fil_frc, rj_fld)
!
      use t_physical_property
      use t_reference_scalar_param
!
      use self_buoyancy_on_sphere
      use const_radial_forces_on_bc
!
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(base_field_address), intent(in) :: ipol_fil
      type(base_force_address), intent(in) :: ipol_fil_frc
      type(base_force_address), intent(in) :: ipol_div_fil_frc
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call r_buoyancy_on_sphere                                         &
     &   (fl_prop%iflag_4_filter_gravity,                               &
     &    fl_prop%iflag_4_filter_comp_buo,                              &
     &    sph_bc_U%kr_in, sph_rj, ipol_fil, ipol_div_fil_frc,           &
     &    fl_prop%coef_buo, fl_prop%coef_comp_buo,                      &
     &    ref_param_T, ref_param_C, rj_fld)
      call r_buoyancy_on_sphere                                         &
     &   (fl_prop%iflag_4_filter_gravity,                               &
     &    fl_prop%iflag_4_filter_comp_buo,                              &
     &    sph_bc_U%kr_out, sph_rj, ipol_fil, ipol_div_fil_frc,          &
     &    fl_prop%coef_buo, fl_prop%coef_comp_buo,                      &
     &    ref_param_T, ref_param_C, rj_fld)
!
!$omp parallel
      if(fl_prop%iflag_4_filter_inertia) then
        call cal_radial_force_on_sph(sph_bc_U%kr_in,                    &
     &      ipol_fil_frc%i_m_advect, ipol_div_fil_frc%i_m_advect,       &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call cal_radial_force_on_sph(sph_bc_U%kr_out,                   &
     &      ipol_fil_frc%i_m_advect, ipol_div_fil_frc%i_m_advect,       &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(fl_prop%iflag_4_filter_lorentz) then
        call cal_radial_force_on_sph(sph_bc_U%kr_in,                    &
     &      ipol_fil_frc%i_lorentz, ipol_div_fil_frc%i_lorentz,         &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call cal_radial_force_on_sph(sph_bc_U%kr_out,                   &
     &      ipol_fil_frc%i_lorentz, ipol_div_fil_frc%i_lorentz,         &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!$omp end parallel
!
      end subroutine const_radial_fil_forces_on_bc
!
! -----------------------------------------------------------------------
!
      end module cal_sph_rot_filtered_force
