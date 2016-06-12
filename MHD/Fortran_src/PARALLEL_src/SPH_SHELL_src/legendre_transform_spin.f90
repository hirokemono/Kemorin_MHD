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
!!      subroutine leg_backward_trans_spin(ncomp, nvector, nscalar,     &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg,            &
!!     &          n_WR, n_WS, WR, WS)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_forward_trans_spin(ncomp, nvector, nscalar,      &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg,            &
!!     &          n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!
!!      subroutine leg_backward_trans_sym_spin(ncomp, nvector, nscalar, &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg,            &
!!     &          n_WR, n_WS, WR, WS)
!!      subroutine leg_forward_trans_sym_spin(ncomp, nvector, nscalar,  &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg,            &
!!     &          n_WR, n_WS, WR, WS)
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
      use m_work_4_sph_trans
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_schmidt_poly_on_rtm
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_spin(ncomp, nvector, nscalar,       &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg,              &
     &          n_WR, n_WS, WR, WS)
!
      use legendre_bwd_trans_spin
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
        call legendre_b_trans_vector_spin(ncomp, nvector,               &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns1,            &
     &      asin_theta_1d_rtm, leg%g_sph_rlm, leg%P_jl, leg%dPdt_jl,    &
     &      n_WR, n_WS, WR, WS)
        call legendre_b_trans_scalar_spin(ncomp, nvector, nscalar,      &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns1, leg%P_jl,  &
     &      n_WR, n_WS, WR, WS)
!
      end subroutine leg_backward_trans_spin
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_spin(ncomp, nvector, nscalar,        &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg,              &
     &          n_WR, n_WS, WR, WS)
!
      use legendre_fwd_trans_spin
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
        call legendre_f_trans_vector_spin(ncomp, nvector,               &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns1,            &
     &      asin_theta_1d_rtm, leg%g_sph_rlm, leg%weight_rtm,           &
     &      leg%P_rtm, leg%dPdt_rtm, n_WR, n_WS, WR, WS)
        call legendre_f_trans_scalar_spin(ncomp, nvector, nscalar,      &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns1,            &
     &      leg%g_sph_rlm, leg%weight_rtm, leg%P_rtm,                   &
     &      n_WR, n_WS, WR, WS)
!
      end subroutine leg_forward_trans_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_sym_spin(ncomp, nvector, nscalar,   &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg,              &
     &          n_WR, n_WS, WR, WS)
!
      use legendre_bwd_trans_sym_spin
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call leg_bwd_trans_vector_sym_spin(ncomp, nvector,                &
     &    sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns1,              &
     &    asin_theta_1d_rtm, leg%g_sph_rlm, n_WR, n_WS, WR, WS)
      call leg_bwd_trans_scalar_sym_spin(ncomp, nvector, nscalar,       &
     &    sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns1,              &
     &    n_WR, n_WS, WR, WS)
!
      end subroutine leg_backward_trans_sym_spin
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_sym_spin(ncomp, nvector, nscalar,    &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg,              &
     &          n_WR, n_WS, WR, WS)
!
      use legendre_fwd_trans_sym_spin
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call leg_fwd_trans_vector_sym_spin(ncomp, nvector,                &
     &    sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns1,              &
     &    asin_theta_1d_rtm, leg%g_sph_rlm, leg%weight_rtm,             &
     &    n_WR, n_WS, WR, WS)
      call leg_fwd_trans_scalar_sym_spin(ncomp, nvector, nscalar,       &
     &    sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns1,              &
     &    leg%g_sph_rlm, leg%weight_rtm, n_WR, n_WS, WR, WS)
!
      end subroutine leg_forward_trans_sym_spin
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_spin

