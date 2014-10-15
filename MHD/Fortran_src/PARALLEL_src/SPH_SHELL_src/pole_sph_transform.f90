!>@file   pole_sph_transform.f90
!!@brief  module pole_sph_transform
!!
!!@author H. Matsui
!!@date Programmed in June, 2012
!
!>@brief  Spherical transform for poles
!!
!!@verbatim
!!      subroutine init_pole_transform
!!
!!      subroutine pole_backward_transforms(ncomp, nvector, nscalar,    &
!!     &          n_WS, n_WR, WS, WR)
!
!!@endverbatim
!!
!!@param ncomp Number of components for transform
!
      module pole_sph_transform
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_phys_constants
      use m_spheric_constants
      use m_spheric_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_pole_transform
!
      use m_geometry_parameter
      use m_work_pole_sph_trans
      use sum_b_trans_at_pole
!
!
      if(iflag_debug .gt. 0) write(*,*) 'set_pole_flag_4_sph_trans'
      call set_pole_flag_4_sph_trans(numnod, internal_node)
      call allocate_work_pole_sph_trans
!
      end subroutine init_pole_transform
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine pole_backward_transforms(ncomp, nvector, nscalar,      &
     &          n_WS, n_WR, WS, WR)
!
      use spherical_SRs_N
      use schmidt_b_trans_at_pole
      use schmidt_b_trans_at_center
      use sum_b_trans_at_pole
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: nvector, nscalar
      integer(kind = kint), intent(in) :: n_WS, n_WR
      real(kind = kreal), intent(in) ::    WS(n_WS)
      real(kind = kreal), intent(inout) :: WR(n_WR)
!
!
      if     (iflag_shell_mode.eq.iflag_no_FEMMESH                      &
        .or.  iflag_shell_mode.eq.iflag_MESH_same) return
!
      call calypso_sph_comm_rj_2_rlm_N(ncomp)
!
      if (iflag_debug.gt.0)  write(*,*) 'schmidt_b_trans_pole_vect'
      call schmidt_b_trans_pole_vect(ncomp, nvector,                    &
     &    irev_sr_rlm, n_WR, WR)
      call schmidt_b_trans_pole_scalar(ncomp, nvector, nscalar,         &
     &    irev_sr_rlm, n_WR, WR)
!
      if (iflag_debug.gt.0)  write(*,*) 'sum_back_trans_at_pole'
      call sum_back_trans_at_pole(ncomp)
!
      if(iflag_shell_mode .eq. iflag_MESH_w_center) then
        call schmidt_b_trans_center_vect(ncomp, nvector,                &
     &      irev_sr_rlm, n_WR, WR)
        call schmidt_b_trans_center_scalar(ncomp, nvector, nscalar)
!
        call sum_back_trans_at_center(ncomp)
      end if
!
      call finish_send_recv_rj_2_rlm
!
      end subroutine pole_backward_transforms
!
! -----------------------------------------------------------------------
!
      end module pole_sph_transform
