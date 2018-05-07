!>@file   cal_grad_of_sph_vectors.f90
!!@brief  module cal_grad_of_sph_vectors
!!
!!@author H. Matsui
!!@date Programmed in Aug, 2007
!
!>@brief  Evaluate pressure and energy fluxes for snapshots
!!
!!@verbatim
!!      subroutine overwrt_grad_of_vectors_sph(sph, r_2nd, sph_MHD_bc,  &
!!     &          leg, ipol, rj_fld)
!!      subroutine overwrt_grad_filter_vecs_sph(sph, r_2nd, sph_MHD_bc, &
!!     &          leg, ipol, rj_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_MHD_boundary_data), intent(in)  :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(legendre_4_sph_trans) , intent(in) :: leg
!!@endverbatim
!
      module cal_grad_of_sph_vectors
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_phys_data
      use t_fdm_coefs
      use t_boundary_data_sph_MHD
      use t_schmidt_poly_on_rtm
!
      implicit none
!
      private :: overwrt_grad_of_vector_sph
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine overwrt_grad_of_vectors_sph(sph, r_2nd, sph_MHD_bc,    &
     &          leg, ipol, rj_fld)
!
      use const_sph_radial_grad
!
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(legendre_4_sph_trans), intent(in) :: leg
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call overwrt_grad_of_vector_sph                                   &
     &   (sph%sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, leg%g_sph_rj,         &
     &    ipol%i_grad_vx, ipol%i_grad_vy, ipol%i_grad_vz, rj_fld)
      call overwrt_grad_of_vector_sph                                   &
     &   (sph%sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, leg%g_sph_rj,         &
     &    ipol%i_grad_wx, ipol%i_grad_wy, ipol%i_grad_wz, rj_fld)
      call overwrt_grad_of_vector_sph                                   &
     &   (sph%sph_rj, r_2nd, sph_MHD_bc%sph_bc_B, leg%g_sph_rj,         &
     &    ipol%i_grad_ax, ipol%i_grad_ay, ipol%i_grad_az, rj_fld)
      call overwrt_grad_of_vector_sph                                   &
     &   (sph%sph_rj, r_2nd, sph_MHD_bc%sph_bc_B, leg%g_sph_rj,         &
     &    ipol%i_grad_bx, ipol%i_grad_by, ipol%i_grad_bz, rj_fld)
      call overwrt_grad_of_vector_sph                                   &
     &   (sph%sph_rj, r_2nd, sph_MHD_bc%sph_bc_B, leg%g_sph_rj,         &
     &    ipol%i_grad_jx, ipol%i_grad_jy, ipol%i_grad_jz, rj_fld)
!
!
!         Input: ipol%i_temp,  Solution: ipol%i_grad_t
      if(ipol%i_grad_t .gt. 0) then
        if(iflag_debug .gt. 0)  write(*,*)                              &
     &           'const_radial_grad_temp', ipol%i_grad_t
        call const_radial_grad_scalar(sph%sph_rj, r_2nd,                &
     &      sph_MHD_bc%sph_bc_T, sph_MHD_bc%fdm2_center, leg%g_sph_rj,  &
     &      ipol%i_temp, ipol%i_grad_t, rj_fld)
      end if
!
      if(ipol%i_grad_composit .gt. 0) then
        if(iflag_debug .gt. 0)  write(*,*)                              &
     &           'const_radial_grad_composition', ipol%i_grad_composit
        call const_radial_grad_scalar(sph%sph_rj, r_2nd,                &
     &      sph_MHD_bc%sph_bc_C, sph_MHD_bc%fdm2_center, leg%g_sph_rj,  &
     &      ipol%i_light, ipol%i_grad_composit, rj_fld)
      end if
!
      end subroutine overwrt_grad_of_vectors_sph
!
! -----------------------------------------------------------------------
!
      subroutine overwrt_grad_filter_vecs_sph(sph, r_2nd, sph_MHD_bc,   &
     &          leg, ipol, rj_fld)
!
      use const_sph_radial_grad
!
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(legendre_4_sph_trans), intent(in) :: leg
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call overwrt_grad_of_vector_sph                                   &
     &   (sph%sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, leg%g_sph_rj,         &
     &    ipol%i_grad_filter_vx, ipol%i_grad_filter_vy,                 &
     &    ipol%i_grad_filter_vz, rj_fld)
      call overwrt_grad_of_vector_sph                                   &
     &   (sph%sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, leg%g_sph_rj,         &
     &    ipol%i_grad_filter_wx, ipol%i_grad_filter_wy,                 &
     &    ipol%i_grad_filter_wz, rj_fld)
      call overwrt_grad_of_vector_sph                                   &
     &   (sph%sph_rj, r_2nd, sph_MHD_bc%sph_bc_B, leg%g_sph_rj,         &
     &    ipol%i_grad_filter_ax, ipol%i_grad_filter_ay,                 &
     &    ipol%i_grad_filter_az, rj_fld)
      call overwrt_grad_of_vector_sph                                   &
     &   (sph%sph_rj, r_2nd, sph_MHD_bc%sph_bc_B, leg%g_sph_rj,         &
     &    ipol%i_grad_filter_bx, ipol%i_grad_filter_by,                 &
     &    ipol%i_grad_filter_bz, rj_fld)
      call overwrt_grad_of_vector_sph                                   &
     &   (sph%sph_rj, r_2nd, sph_MHD_bc%sph_bc_B, leg%g_sph_rj,         &
     &    ipol%i_grad_filter_jx, ipol%i_grad_filter_jy,                 &
     &    ipol%i_grad_filter_jz, rj_fld)
      return
!
!         Input: ipol%i_filter_temp,  Solution: ipol%i_grad_t
      if(ipol%i_grad_filter_temp .gt. 0) then
        if(iflag_debug .gt. 0)  write(*,*)                              &
     &     'const_radial_grad_filter_temp', ipol%i_grad_filter_temp
        call const_radial_grad_scalar(sph%sph_rj, r_2nd,                &
     &      sph_MHD_bc%sph_bc_T, sph_MHD_bc%fdm2_center, leg%g_sph_rj,  &
     &      ipol%i_filter_temp, ipol%i_grad_filter_temp, rj_fld)
      end if
!
      if(ipol%i_grad_filter_comp .gt. 0) then
        if(iflag_debug .gt. 0)  write(*,*)                              &
     &     'const_radial_grad_filter_comp', ipol%i_grad_filter_comp
        call const_radial_grad_scalar(sph%sph_rj, r_2nd,                &
     &      sph_MHD_bc%sph_bc_C, sph_MHD_bc%fdm2_center, leg%g_sph_rj,  &
     &      ipol%i_filter_comp, ipol%i_grad_filter_comp, rj_fld)
      end if
!
      end subroutine overwrt_grad_filter_vecs_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrt_grad_of_vector_sph                             &
     &         (sph_rj, r_2nd, sph_bc, g_sph_rj,                        &
     &          i_grad_vx, i_grad_vy, i_grad_vz, rj_fld)
!
      use const_sph_radial_grad
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in)  :: sph_bc
      integer(kind = kint), intent(in) :: i_grad_vx, i_grad_vy
      integer(kind = kint), intent(in) :: i_grad_vz
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(i_grad_vx .gt. 0) then
        call const_sph_gradient_no_bc(sph_rj, r_2nd, sph_bc, g_sph_rj,  &
     &      i_grad_vx, i_grad_vx, rj_fld)
      end if
      if(i_grad_vy .gt. 0) then
        call const_sph_gradient_no_bc(sph_rj, r_2nd, sph_bc, g_sph_rj,  &
     &      i_grad_vy, i_grad_vy, rj_fld)
      end if
      if(i_grad_vz .gt. 0) then
        call const_sph_gradient_no_bc(sph_rj, r_2nd, sph_bc, g_sph_rj,  &
     &      i_grad_vz, i_grad_vz, rj_fld)
      end if
!
      end subroutine overwrt_grad_of_vector_sph
!
! -----------------------------------------------------------------------
!
      end module cal_grad_of_sph_vectors
