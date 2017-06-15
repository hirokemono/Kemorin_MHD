!>@file   cal_sph_rotation_of_SGS.f90
!!@brief  module cal_sph_rotation_of_SGS
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate curl or divergence of forces
!!
!!@verbatim
!!      subroutine rot_SGS_terms_exp_sph                                &
!!     &         (sph_rj, r_2nd, sph_MHD_bc, leg, ipol, itor, rj_fld)
!!      subroutine cal_div_of_SGS_forces_sph_2                          &
!!     &         (sph_rj, r_2nd, sph_MHD_bc, g_sph_rj, ipol, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol, itor
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module cal_sph_rotation_of_SGS
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_schmidt_poly_on_rtm
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
!
      implicit none
!
      private :: SGS_rot_of_SGS_forces_sph_2
      private :: cal_rot_of_SGS_induction_sph
      private :: cal_div_of_SGS_fluxes_sph
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine rot_SGS_terms_exp_sph                                  &
     &         (sph_rj, r_2nd, sph_MHD_bc, leg, ipol, itor, rj_fld)
!
      use calypso_mpi
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'SGS_rot_of_SGS_forces_sph_2'
      call SGS_rot_of_SGS_forces_sph_2(sph_rj, r_2nd, leg%g_sph_rj,     &
     &    sph_MHD_bc%sph_bc_U, ipol, itor, rj_fld)
!
      call cal_rot_of_SGS_induction_sph(sph_rj, r_2nd, leg%g_sph_rj,    &
     &    sph_MHD_bc%sph_bc_B, ipol, rj_fld)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'cal_div_of_SGS_fluxes_sph'
      call cal_div_of_SGS_fluxes_sph(sph_rj, r_2nd, leg%g_sph_rj,       &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%sph_bc_C, ipol, rj_fld)
!
      end subroutine rot_SGS_terms_exp_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SGS_rot_of_SGS_forces_sph_2                            &
     &         (sph_rj, r_2nd, g_sph_rj, sph_bc_U, ipol, itor, rj_fld)
!
      use calypso_mpi
      use const_sph_radial_grad
      use const_sph_rotation
      use cal_inner_core_rotation
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(phys_address), intent(in) :: ipol, itor
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      type(sph_boundary_type), intent(in) :: sph_bc_U
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if( (ipol%i_SGS_inertia*ipol%i_SGS_rot_inertia) .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'take rotation of advection'
        call const_sph_force_rot2(sph_rj, r_2nd, sph_bc_U, g_sph_rj,    &
     &      ipol%i_SGS_inertia, ipol%i_SGS_rot_inertia, rj_fld)
      end if
!
      if( (ipol%i_SGS_Lorentz*ipol%i_SGS_rot_Lorentz) .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'take rotation of Lorentz'
        call const_sph_force_rot2(sph_rj, r_2nd, sph_bc_U, g_sph_rj,    &
     &      ipol%i_SGS_Lorentz, ipol%i_SGS_rot_Lorentz, rj_fld)
      end if
!
      end subroutine SGS_rot_of_SGS_forces_sph_2
!
! ----------------------------------------------------------------------
!
      subroutine cal_div_of_SGS_forces_sph_2                            &
     &         (sph_rj, r_2nd, sph_MHD_bc, g_sph_rj, ipol, rj_fld)
!
      use cal_div_buoyancies_sph_MHD
      use const_sph_divergence
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if( (ipol%i_SGS_inertia*ipol%i_div_inertia) .gt. 0) then
        call const_sph_div_force                                        &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, g_sph_rj,               &
     &      ipol%i_SGS_inertia, ipol%i_SGS_div_inertia, rj_fld)
      end if
!
      if( (ipol%i_SGS_Lorentz*ipol%i_div_Lorentz) .gt. 0) then
        call const_sph_div_force                                        &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, g_sph_rj,               &
     &      ipol%i_SGS_Lorentz, ipol%i_SGS_div_Lorentz, rj_fld)
      end if
!
      end subroutine cal_div_of_SGS_forces_sph_2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rot_of_SGS_induction_sph                           &
     &         (sph_rj, r_2nd, g_sph_rj, sph_bc_B, ipol, rj_fld)
!
      use calypso_mpi
      use const_sph_radial_grad
      use const_sph_rotation
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(phys_address), intent(in) :: ipol
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      type(sph_boundary_type), intent(in) :: sph_bc_B
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if( (ipol%i_SGS_vp_induct*ipol%i_induction) .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'obtain magnetic induction'
        call const_sph_rotation_uxb(sph_rj, r_2nd, sph_bc_B, g_sph_rj,  &
     &      ipol%i_SGS_vp_induct, ipol%i_SGS_induction, rj_fld)
      end if
!
      end subroutine cal_rot_of_SGS_induction_sph
!
! ----------------------------------------------------------------------
!
      subroutine cal_div_of_SGS_fluxes_sph(sph_rj, r_2nd, g_sph_rj,     &
     &          sph_bc_T, sph_bc_C, ipol, rj_fld)
!
      use m_coef_fdm_to_center
      use calypso_mpi
      use const_sph_divergence
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(phys_address), intent(in) :: ipol
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if( (ipol%i_SGS_h_flux*ipol%i_h_advect) .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'take div of heat flux'
        call const_sph_scalar_advect                                    &
     &     (sph_rj, r_2nd, sph_bc_T, fdm2_center1, g_sph_rj,            &
     &      ipol%i_SGS_h_flux, ipol%i_SGS_div_h_flux, rj_fld)
      end if
!
      if( (ipol%i_SGS_c_flux*ipol%i_c_advect) .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'take div  of composit flux'
        call const_sph_scalar_advect                                    &
     &     (sph_rj, r_2nd, sph_bc_C, fdm2_center1, g_sph_rj,            &
     &      ipol%i_SGS_c_flux, ipol%i_SGS_div_c_flux, rj_fld)
      end if
!
      end subroutine cal_div_of_SGS_fluxes_sph
!
! -----------------------------------------------------------------------
!
      end module cal_sph_rotation_of_SGS
