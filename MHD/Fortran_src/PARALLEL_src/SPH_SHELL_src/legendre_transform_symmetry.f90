!>@file   legendre_transform_symmetry.f90
!!@brief  module legendre_transform_symmetry
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform considering symmetry
!!
!!
!!@verbatim
!!      subroutine leg_backward_trans_sym_org,                          &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms 
!!      subroutine leg_forward_trans_sym_org                            &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal) 
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_transform_symmetry
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_sym_org                             &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use legendre_bwd_trans_symmetry
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call finish_send_recv_rj_2_rlm
!$omp parallel workshare
      WS(1:ncomp*ntot_item_sr_rtm) = 0.0d0
!$omp end parallel workshare
!
      if(nvector .gt. 0) then
        call leg_bwd_trans_vector_sym_org(ncomp, nvector,               &
     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
      end if
      if(nscalar .gt. 0) then
        call leg_bwd_trans_scalar_sym_org(ncomp, nvector, nscalar,      &
     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
      end if
!
      end subroutine leg_backward_trans_sym_org
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_sym_org                              &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use legendre_fwd_trans_symmetry
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call finish_send_recv_rtp_2_rtm
!$omp parallel workshare
      WS(1:ncomp*ntot_item_sr_rlm) = 0.0d0
!$omp end parallel workshare
!
      if(nvector .gt. 0) then
        call leg_fwd_trans_vector_sym_org(ncomp, nvector,               &
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
      end if
      if(nscalar .gt. 0) then
        call leg_fwd_trans_scalar_sym_org(ncomp, nvector, nscalar,      &
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
      end if
!
      end subroutine leg_forward_trans_sym_org
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_symmetry

