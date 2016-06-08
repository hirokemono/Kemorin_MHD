!>@file   legendre_transform_lgloop.f90
!!@brief  module legendre_transform_lgloop
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transforms
!!       (longest loop version)
!!
!!
!!@verbatim
!!      subroutine leg_backward_trans_long(ncomp, nvector, nscalar,     &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm,                 &
!!     &          n_WR, n_WS, WR, WS)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_forward_trans_long(ncomp, nvector, nscalar,      &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm,                 &
!!     &          n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_transform_lgloop
!
      use m_precision
      use m_schmidt_poly_on_rtm
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_long(ncomp, nvector, nscalar,       &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm,                   &
     &          n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use legendre_bwd_trans_lgloop
      use spherical_SRs_N
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call calypso_sph_from_recv_N(ncomp, sph_rlm%nnod_rlm,             &
     &    comm_rlm, n_WR, WR, sp_rlm_wk(1))
      call clear_bwd_legendre_work(ncomp, sph_rtm%nnod_rtm)
!
      call legendre_b_trans_vector_long                                 &
     &    (ncomp, nvector, sph_rlm, sph_rtm, g_sph_rlm,                 &
     &     leg1%P_rtm, leg1%dPdt_rtm, sp_rlm_wk, vr_rtm_wk)
      call legendre_b_trans_scalar_long                                 &
     &    (ncomp, nvector, nscalar, sph_rlm, sph_rtm, leg1%P_rtm,       &
     &     sp_rlm_wk, vr_rtm_wk)
!
      call calypso_sph_to_send_N(ncomp, sph_rtm%nnod_rtm,               &
     &    comm_rtm, n_WS, vr_rtm_wk(1), WS)
!
      end subroutine leg_backward_trans_long
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_long(ncomp, nvector, nscalar,        &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm,                   &
     &          n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use legendre_fwd_trans_lgloop
      use spherical_SRs_N
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call calypso_sph_from_recv_N(ncomp, sph_rtm%nnod_rtm,             &
     &    comm_rtm, n_WR, WR, vr_rtm_wk(1))
!
      call legendre_f_trans_vector_long                                 &
     &    (ncomp, nvector, sph_rtm, sph_rlm,  g_sph_rlm, weight_rtm,    &
     &     leg1%P_rtm, leg1%dPdt_rtm, vr_rtm_wk, sp_rlm_wk)
      call legendre_f_trans_scalar_long                                 &
     &    (ncomp, nvector, nscalar, sph_rtm, sph_rlm,                   &
     &     g_sph_rlm, weight_rtm, leg1%P_rtm, vr_rtm_wk, sp_rlm_wk)
!
      call calypso_sph_to_send_N(ncomp, sph_rlm%nnod_rlm,               &
     &    comm_rlm, n_WS, sp_rlm_wk(1), WS)
!
      end subroutine leg_forward_trans_long
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_lgloop

