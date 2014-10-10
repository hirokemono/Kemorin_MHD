!>@file   sph_transforms.f90
!!@brief  module sph_transforms
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief Spherical harmonics transform for vector
!!       and gradient of scalar
!!
!!@verbatim
!!      subroutine sph_backward_transforms                              &
!!     &         (ncomp_trans, nvector, nscalar, ntensor)
!!      subroutine sph_forward_transforms                               &
!!     &         (ncomp_trans, nvector, nscalar, ntensor)
!!
!!   input /outpt arrays for single field
!!
!!      radial component:      vr_rtp(i_rtp,1)
!!      elevetional component: vr_rtp(i_rtp,2)
!!      azimuthal component:   vr_rtp(i_rtp,3)
!!
!!     forward transform: 
!!      Poloidal component:          sp_rj(3*i_rj-2)
!!      diff. of Poloidal component: sp_rj(3*i_rj-1)
!!      Toroidal component:          sp_rj(3*i_rj  )
!!
!!     backward transform: 
!!      Poloidal component:          sp_rj(3*i_rj-2)
!!      diff. of Poloidal component: sp_rj(3*i_rj-1)
!!      Toroidal component:          sp_rj(3*i_rj  )
!!@endverbatim
!!
!!@param ncomp_trans Number of components for transform
!
      module sph_transforms
!
      use m_precision
!
      use calypso_mpi
      use m_work_time
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_work_4_sph_trans
      use sph_FFT_selector
      use legendre_transform_select
      use spherical_SRs_N
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sph_backward_transforms                                &
     &         (ncomp_trans, nvector, nscalar, ntensor)
!
      use m_work_time
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: nvector, nscalar, ntensor
!
      integer(kind = kint) :: nscalar_trans
!
!
      nscalar_trans = nscalar + 6*ntensor
      call check_calypso_rj_2_rlm_buf_N(ncomp_trans)
      call check_calypso_rtm_2_rtp_buf_N(ncomp_trans)
!
!      call check_sp_rj(my_rank, ncomp_trans)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(18)
      call calypso_rj_to_send_N(ncomp_trans, n_WS, sp_rj, WS)
      call calypso_sph_comm_rj_2_rlm_N(ncomp_trans)
      call end_eleps_time(18)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      call start_eleps_time(22)
      if(iflag_debug .gt. 0) write(*,*) 'sel_backward_legendre_trans'
      call sel_backward_legendre_trans                                  &
     &   (ncomp_trans, nvector, nscalar_trans, n_WR, n_WS, WR, WS)
      call end_eleps_time(22)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(19)
      call calypso_sph_comm_rtm_2_rtp_N(ncomp_trans)
      call end_eleps_time(19)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!      call check_vr_rtp(my_rank, ncomp_trans)
!
      call start_eleps_time(24)
      call back_FFT_select_from_recv(ncomp_trans, n_WR, WR)
      call end_eleps_time(24)
!
      call finish_send_recv_rtm_2_rtp
!
!      call check_vr_rtp(my_rank, ncomp_trans)
!
      end subroutine sph_backward_transforms
!
! -----------------------------------------------------------------------
!
      subroutine sph_forward_transforms                                 &
     &         (ncomp_trans, nvector, nscalar, ntensor)
!
      use m_work_time
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: nvector, nscalar, ntensor
!
      integer(kind = kint) :: nscalar_trans
!
!
      nscalar_trans = nscalar + 6*ntensor
      call check_calypso_rtp_2_rtm_buf_N(ncomp_trans)
      call check_calypso_rlm_2_rj_buf_N(ncomp_trans)
!
!      call check_vr_rtp(my_rank, ncomp_trans)
      call start_eleps_time(24)
      call fwd_FFT_select_to_send(ncomp_trans, n_WS, WS)
      call end_eleps_time(24)
!      call check_vr_rtp(my_rank, ncomp_trans)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(20)
      call calypso_sph_comm_rtp_2_rtm_N(ncomp_trans)
      call end_eleps_time(20)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
      call start_eleps_time(23)
      if(iflag_debug .gt. 0) write(*,*) 'sel_forward_legendre_trans'
      call sel_forward_legendre_trans                                   &
     &   (ncomp_trans, nvector, nscalar, n_WR, n_WS, WR, WS)
      call end_eleps_time(23)
!
      START_SRtime= MPI_WTIME()
      call start_eleps_time(21)
      call calypso_sph_comm_rlm_2_rj_N(ncomp_trans)
      call calypso_rj_from_recv_N(ncomp_trans, n_WR, WR, sp_rj)
      call finish_send_recv_rlm_2_rj
!
      call end_eleps_time(21)
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!      call check_sp_rj(my_rank, ncomp_trans)
!
      end subroutine sph_forward_transforms
!
! -----------------------------------------------------------------------
!
      end module sph_transforms
