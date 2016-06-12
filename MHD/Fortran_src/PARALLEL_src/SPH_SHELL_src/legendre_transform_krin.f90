!>@file   legendre_transform_krin.f90
!!@brief  module legendre_transform_krin
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
!!      subroutine leg_bwd_trans_fields_krin(ncomp, nvector, nscalar,   &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg,            &
!!     &          n_WR, n_WS, WR, WS)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_fwd_trans_fields_krin(ncomp, nvector, nscalar,   &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg,            &
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
      module legendre_transform_krin
!
      use m_precision
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
      subroutine leg_bwd_trans_fields_krin(ncomp, nvector, nscalar,     &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg,              &
     &          n_WR, n_WS, WR, WS)
!
      use m_sph_communicators
      use legendre_bwd_trans_krin
      use ordering_schmidt_trans_krin
      use spherical_SRs_N
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
      call order_b_trans_fields_krin                                    &
     &   (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,        &
     &    sph_rlm%a_r_1d_rlm_r, sph_rlm%istack_rlm_j_smp,               &
     &    ncomp, nvector, nscalar, comm_rlm%irev_sr,                    &
     &    n_WR, WR, sp_rlm_wk(1))
!
      call legendre_b_trans_vector_krin                                 &
     &   (ncomp, nvector, sph_rlm, sph_rtm, idx_trns1,                  &
     &    asin_theta_1d_rtm, leg%g_sph_rlm, leg%P_jl, leg%dPdt_jl,      &
     &    sp_rlm_wk(1), vr_rtm_wk(1))
      call legendre_b_trans_scalar_krin(ncomp, nvector, nscalar,        &
     &    sph_rlm, sph_rtm, idx_trns1, leg%P_jl,                        &
     &    sp_rlm_wk(1), vr_rtm_wk(1))
!
      call back_b_trans_fields_krin                                     &
     &   (sph_rtm%nidx_rtm, ncomp, nvector, nscalar,                    &
     &    vr_rtm_wk(1), comm_rtm%nneib_domain, comm_rtm%istack_sr,      &
     &    comm_rtm%item_sr, WS(1))
!
      end subroutine leg_bwd_trans_fields_krin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_fields_krin(ncomp, nvector, nscalar,     &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg,              &
     &          n_WR, n_WS, WR, WS)
!
      use m_sph_communicators
      use legendre_fwd_trans_krin
      use ordering_schmidt_trans_krin
      use spherical_SRs_N
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
      call order_f_trans_fields_krin                                    &
     &   (sph_rlm%nnod_rlm, sph_rtm%nidx_rtm,                           &
     &    sph_rtm%istep_rtm, sph_rtm%istack_rtm_m_smp,                  &
     &    ncomp, nvector, nscalar, comm_rtm%irev_sr,                    &
     &    n_WR, WR, vr_rtm_wk(1))
!
      call legendre_f_trans_vector_krin                                 &
     &   (ncomp, nvector, sph_rtm, sph_rlm, idx_trns1,                  &
     &    asin_theta_1d_rtm, leg%g_sph_rlm, leg%weight_rtm,             &
     &    leg%P_rtm, leg%dPdt_rtm, vr_rtm_wk(1), sp_rlm_wk(1))
      call legendre_f_trans_scalar_krin                                 &
     &   (ncomp, nvector, nscalar, sph_rtm, sph_rlm, idx_trns1,         &
     &    leg%g_sph_rlm, leg%weight_rtm, leg%P_rtm,                     &
     &    vr_rtm_wk(1), sp_rlm_wk(1))
!
      call back_f_trans_fields_krin                                     &
     &   (sph_rlm%nidx_rlm, ncomp, nvector, nscalar,                    &
     &    sp_rlm_wk(1), comm_rlm%nneib_domain, comm_rlm%istack_sr,      &
     &    comm_rlm%item_sr, WS(1))
!
      end subroutine leg_fwd_trans_fields_krin
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_krin
