!>@file   legendre_trans_matmul_big.f90
!!@brief  module legendre_trans_matmul_big
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
!!      subroutine leg_backward_trans_matmul_big                        &
!!     &         (ncomp, nvector, nscalar,                              &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm,                 &
!!     &          n_WR, n_WS, WR, WS)
!!      subroutine leg_backward_trans_dgemm_big                         &
!!     &         (ncomp, nvector, nscalar,                              &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm,                 &
!!     &          n_WR, n_WS, WR, WS)
!!      subroutine leg_backward_trans_matprod_big                       &
!!     &         (ncomp, nvector, nscalar,                              &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm,                 &
!!     &          n_WR, n_WS, WR, WS)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_forward_trans_matmul_big                         &
!!     &         (ncomp, nvector, nscalar,                              &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm,                 &
!!     &          n_WR, n_WS, WR, WS)
!!      subroutine leg_forward_trans_dgemm_big                          &
!!     &         (ncomp, nvector, nscalar,                              &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm,                 &
!!     &          n_WR, n_WS, WR, WS)
!!      subroutine leg_forward_trans_matprod_big                        &
!!     &         (ncomp, nvector, nscalar,                              &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm,                 &
!!     &          n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_trans_matmul_big
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
      subroutine leg_backward_trans_matmul_big                          &
     &         (ncomp, nvector, nscalar,                                &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm,                   &
     &          n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use leg_b_trans_sym_matmul_big
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      if(ncomp .le. 0) return
        call leg_bwd_trans_sym_matmul_big(ncomp, nvector, nscalar,      &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, g_sph_rlm,            &
     &      n_WR, n_WS, WR, WS)
!
      end subroutine leg_backward_trans_matmul_big
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_matmul_big                           &
     &         (ncomp, nvector, nscalar,                                &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm,                   &
     &          n_WR, n_WS, WR, WS)
!
      use leg_f_trans_sym_matmul_big
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      if(ncomp .le. 0) return
        call leg_fwd_trans_sym_matmul_big(ncomp, nvector, nscalar,      &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm,                       &
     &      g_sph_rlm, weight_rtm, n_WR, n_WS, WR, WS)
!
      end subroutine leg_forward_trans_matmul_big
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_dgemm_big                           &
     &         (ncomp, nvector, nscalar,                                &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm,                   &
     &          n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use leg_b_trans_sym_matmul_big
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      if(ncomp .le. 0) return
        call leg_bwd_trans_sym_dgemm_big(ncomp, nvector, nscalar,       &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, g_sph_rlm,            &
     &      n_WR, n_WS, WR, WS)
!
      end subroutine leg_backward_trans_dgemm_big
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_dgemm_big                            &
     &         (ncomp, nvector, nscalar,                                &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm,                   &
     &          n_WR, n_WS, WR, WS)
!
      use leg_f_trans_sym_matmul_big
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      if(ncomp .le. 0) return
        call leg_fwd_trans_sym_dgemm_big(ncomp, nvector, nscalar,       &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm,                       &
     &      g_sph_rlm, weight_rtm, n_WR, n_WS, WR, WS)
!
      end subroutine leg_forward_trans_dgemm_big
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_matprod_big                         &
     &         (ncomp, nvector, nscalar,                                &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm,                   &
     &          n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use leg_b_trans_sym_matmul_big
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      if(ncomp .le. 0) return
        call leg_bwd_trans_sym_matprod_big(ncomp, nvector, nscalar,     &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, g_sph_rlm,            &
     &      n_WR, n_WS, WR, WS)
!
      end subroutine leg_backward_trans_matprod_big
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_matprod_big                          &
     &         (ncomp, nvector, nscalar,                                &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm,                   &
     &          n_WR, n_WS, WR, WS)
!
      use leg_f_trans_sym_matmul_big
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      if(ncomp .le. 0) return
        call leg_fwd_trans_sym_matprod_big(ncomp, nvector, nscalar,     &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm,                       &
     &      g_sph_rlm, weight_rtm, n_WR, n_WS, WR, WS)
!
      end subroutine leg_forward_trans_matprod_big
!
! -----------------------------------------------------------------------
!
      end module legendre_trans_matmul_big

