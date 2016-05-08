!>@file   legendre_trans_sym_matmul.f90
!!@brief  module legendre_trans_sym_matmul
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
!!      subroutine leg_backward_trans_sym_matmul                        &
!!     &         (ncomp, nvector, nscalar,                              &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm,                 &
!!     &          n_WR, n_WS, WR, WS)
!!      subroutine leg_backward_trans_sym_dgemm                         &
!!     &         (ncomp, nvector, nscalar,                              &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm,                 &
!!     &          n_WR, n_WS, WR, WS)
!!      subroutine leg_backward_trans_sym_matprod                       &
!!     &         (ncomp, nvector, nscalar,                              &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm,                 &
!!     &          n_WR, n_WS, WR, WS)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_forward_trans_sym_matmul                         &
!!     &         (ncomp, nvector, nscalar,                              &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm,                 &
!!     &          n_WR, n_WS, WR, WS)
!!      subroutine leg_forward_trans_sym_dgemm                          &
!!     &         (ncomp, nvector, nscalar,                              &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm,                 &
!!     &          n_WR, n_WS, WR, WS)
!!      subroutine leg_forward_trans_sym_matprod                        &
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
      module legendre_trans_sym_matmul
!
      use m_precision
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
      subroutine leg_backward_trans_sym_matmul                          &
     &         (ncomp, nvector, nscalar,                                &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm,                   &
     &          n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use legendre_bwd_sym_matmul
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
      if(nvector .gt. 0) then
        call leg_b_trans_vec_sym_matmul(ncomp, nvector,                 &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, n_WR, n_WS, WR, WS)
      end if
      if(nscalar .gt. 0) then
        call leg_b_trans_scl_sym_matmul(ncomp, nvector, nscalar,        &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, n_WR, n_WS, WR, WS)
      end if
!
      end subroutine leg_backward_trans_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_sym_matmul                           &
     &         (ncomp, nvector, nscalar,                                &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm,                   &
     &          n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use legendre_fwd_sym_matmul
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
      if(nvector .gt. 0) then
        call leg_f_trans_vec_sym_matmul(ncomp, nvector,                 &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, n_WR, n_WS, WR, WS)
      end if
      if(nscalar .gt. 0) then
        call leg_f_trans_scl_sym_matmul(ncomp, nvector, nscalar,        &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, n_WR, n_WS, WR, WS)
      end if
!
      end subroutine leg_forward_trans_sym_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_sym_dgemm                           &
     &         (ncomp, nvector, nscalar,                                &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm,                   &
     &          n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use legendre_bwd_sym_matmul
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
      if(nvector .gt. 0) then
        call leg_b_trans_vec_sym_dgemm(ncomp, nvector,                  &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, n_WR, n_WS, WR, WS)
      end if
      if(nscalar .gt. 0) then
        call leg_b_trans_scl_sym_dgemm(ncomp, nvector, nscalar,         &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, n_WR, n_WS, WR, WS)
      end if
!
      end subroutine leg_backward_trans_sym_dgemm
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_sym_dgemm                            &
     &         (ncomp, nvector, nscalar,                                &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm,                   &
     &          n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use legendre_fwd_sym_matmul
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
      if(nvector .gt. 0) then
        call leg_f_trans_vec_sym_dgemm(ncomp, nvector,                  &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, n_WR, n_WS, WR, WS)
      end if
      if(nscalar .gt. 0) then
        call leg_f_trans_scl_sym_dgemm(ncomp, nvector, nscalar,         &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, n_WR, n_WS, WR, WS)
      end if
!
      end subroutine leg_forward_trans_sym_dgemm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_sym_matprod                         &
     &         (ncomp, nvector, nscalar,                                &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm,                   &
     &          n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use legendre_bwd_sym_matmul
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
      if(nvector .gt. 0) then
        call leg_b_trans_vec_sym_matprod(ncomp, nvector,                &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, n_WR, n_WS, WR, WS)
      end if
      if(nscalar .gt. 0) then
        call leg_b_trans_scl_sym_matprod(ncomp, nvector, nscalar,       &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, n_WR, n_WS, WR, WS)
      end if
!
      end subroutine leg_backward_trans_sym_matprod
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_sym_matprod                          &
     &         (ncomp, nvector, nscalar,                                &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm,                   &
     &          n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use legendre_fwd_sym_matmul
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
      if(nvector .gt. 0) then
        call leg_f_trans_vec_sym_matprod(ncomp, nvector,                &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, n_WR, n_WS, WR, WS)
      end if
      if(nscalar .gt. 0) then
        call leg_f_trans_scl_sym_matprod(ncomp, nvector, nscalar,       &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, n_WR, n_WS, WR, WS)
      end if
!
      end subroutine leg_forward_trans_sym_matprod
!
! -----------------------------------------------------------------------
!
      end module legendre_trans_sym_matmul
