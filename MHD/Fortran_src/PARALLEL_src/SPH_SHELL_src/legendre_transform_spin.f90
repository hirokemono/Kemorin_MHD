!>@file   legendre_transform_spin.f90
!!@brief  module legendre_transform_spin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transforms
!!       (innermost loop is spherical harmonics)
!!
!!
!!@verbatim
!!    Backward transforms
!!      subroutine leg_backward_trans_spin                              &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_forward_trans_spin                               &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!
!!      subroutine leg_backward_trans_sym_spin                          &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!      subroutine leg_forward_trans_sym_spin(ncomp, nvector, nscalar)
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_transform_spin
!
      use m_precision
      use m_work_time
      use m_work_4_sph_trans_spin
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_spin                                &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use ordering_schmidt_trans_spin
      use ordering_schmidt_trans_krin
      use legendre_bwd_trans_spin
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call start_eleps_time(25)
      call calypso_rlm_from_recv_N(ncomp, n_WR, WR, sp_rlm)
      call order_b_trans_fields_spin(ncomp, nvector, nscalar,           &
     &    sp_rlm(1), sp_rlm_wk(1))
      call end_eleps_time(25)
!
      call start_eleps_time(27)
      if(nvector .gt. 0) call legendre_b_trans_vector_spin              &
     &                      (ncomp, nvector,                            &
     &                       sp_rlm_wk(1), vr_rtm_wk(1))
      if(nscalar .gt. 0) call legendre_b_trans_scalar_spin              &
     &                      (ncomp, nvector, nscalar,                   &
     &                       sp_rlm_wk(1), vr_rtm_wk(1))
      call end_eleps_time(27)
!
      call start_eleps_time(28)
      call back_b_trans_fields_krin(ncomp, nvector, nscalar,            &
     &    vr_rtm_wk(1), vr_rtm(1))
      call finish_send_recv_rj_2_rlm
      call calypso_rtm_to_send_N(ncomp, n_WS, vr_rtm, WS)
      call end_eleps_time(28)
!
      end subroutine leg_backward_trans_spin
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_spin                                 &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use ordering_schmidt_trans_spin
      use ordering_schmidt_trans_krin
      use legendre_fwd_trans_spin
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call start_eleps_time(29)
      call calypso_rtm_from_recv_N(ncomp, n_WR, WR, vr_rtm)
      call order_f_trans_fields_spin(ncomp, nvector, nscalar,           &
     &    vr_rtm(1), vr_rtm_wk(1))
      call end_eleps_time(29)
!
      call start_eleps_time(31)
      if(nvector .gt. 0) call legendre_f_trans_vector_spin              &
     &                      (ncomp, nvector,  vr_rtm_wk(1),             &
     &                       sp_rlm_wk(1))
      if(nscalar .gt. 0) call legendre_f_trans_scalar_spin              &
     &                      (ncomp, nvector, nscalar, vr_rtm_wk(1),     &
     &                       sp_rlm_wk(1))
      call end_eleps_time(31)
!
      call start_eleps_time(32)
      call back_f_trans_fields_krin(ncomp, nvector, nscalar,            &
     &    sp_rlm_wk(1), sp_rlm(1))
      call finish_send_recv_rtp_2_rtm
      call calypso_rlm_to_send_N(ncomp, n_WS, sp_rlm, WS)
      call end_eleps_time(32)
!
      end subroutine leg_forward_trans_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_sym_spin                            &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use ordering_schmidt_trans_spin
      use ordering_schmidt_trans_krin
      use legendre_bwd_trans_sym_spin
      use spherical_SRs_N
      use legendre_bwd_trans_spin
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call start_eleps_time(25)
      call calypso_rlm_from_recv_N(ncomp, n_WR, WR, sp_rlm)
      call order_b_trans_fields_spin(ncomp, nvector, nscalar,           &
     &    sp_rlm(1), sp_rlm_wk(1))
      vr_rtm_wk(1:ncomp*nnod_rtm) = 0.0d0
      call end_eleps_time(25)
!
      call start_eleps_time(27)
      if(nvector .gt. 0) call leg_bwd_trans_vector_sym_spin             &
     &                      (ncomp, nvector,                            &
     &                       sp_rlm_wk(1), vr_rtm_wk(1))
      if(nscalar .gt. 0) call leg_bwd_trans_scalar_sym_spin             &
     &                      (ncomp, nvector, nscalar,                   &
     &                       sp_rlm_wk(1), vr_rtm_wk(1))
      call end_eleps_time(27)
!
      call start_eleps_time(28)
      call back_b_trans_fields_krin(ncomp, nvector, nscalar,            &
     &    vr_rtm_wk(1), vr_rtm(1))
      call finish_send_recv_rj_2_rlm
      call calypso_rtm_to_send_N(ncomp, n_WS, vr_rtm, WS)
      call end_eleps_time(28)
!
      end subroutine leg_backward_trans_sym_spin
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_sym_spin                             &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use ordering_schmidt_trans_spin
      use ordering_schmidt_trans_krin
      use legendre_fwd_trans_sym_spin
      use spherical_SRs_N
      use legendre_fwd_trans_spin
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call start_eleps_time(29)
      call calypso_rtm_from_recv_N(ncomp, n_WR, WR, vr_rtm)
      call order_f_trans_fields_spin(ncomp, nvector, nscalar,           &
     &    vr_rtm(1), vr_rtm_wk(1))
      sp_rlm_wk(1:ncomp*nnod_rlm) = 0.0d0
      call end_eleps_time(29)
!
      call start_eleps_time(31)
      if(nvector .gt. 0) call leg_fwd_trans_vector_sym_spin             &
     &                      (ncomp, nvector,  vr_rtm_wk(1),             &
     &                       sp_rlm_wk(1))
      if(nscalar .gt. 0) call leg_fwd_trans_scalar_sym_spin             &
     &                      (ncomp, nvector, nscalar, vr_rtm_wk(1),     &
     &                       sp_rlm_wk(1))
      call end_eleps_time(31)
!
      call start_eleps_time(32)
      call back_f_trans_fields_krin(ncomp, nvector, nscalar,            &
     &    sp_rlm_wk(1), sp_rlm(1))
      call finish_send_recv_rtp_2_rtm
      call calypso_rlm_to_send_N(ncomp, n_WS, sp_rlm, WS)
      call end_eleps_time(32)
!
      end subroutine leg_forward_trans_sym_spin
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_spin

