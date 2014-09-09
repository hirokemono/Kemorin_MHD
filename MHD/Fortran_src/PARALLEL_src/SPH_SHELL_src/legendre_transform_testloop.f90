!>@file   legendre_transform_testloop.f90
!!@brief  module legendre_transform_testloop
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
!!      subroutine leg_backward_trans_test(ncomp, nvector, nscalar)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_forward_trans_test(ncomp, nvector, nscalar)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_transform_testloop
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
      subroutine leg_backward_trans_test(ncomp, nvector, nscalar)
!
      use m_work_4_sph_trans_spin
      use legendre_bwd_trans_testloop
      use ordering_schmidt_trans_spin
      use ordering_schmidt_trans_krin
      use merge_polidal_toroidal_v
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      call order_b_trans_fields_spin(ncomp, nvector, nscalar,           &
     &    sp_rlm(1), sp_rlm_wk(1))
      call clear_bwd_legendre_work(ncomp)
      call clear_bwd_legendre_trans(ncomp)
      if(nscalar .gt. 0) then
        call legendre_b_trans_scalar_test(ncomp, nvector, nscalar,      &
     &      sp_rlm_wk(1), vr_rtm_wk(1))
      end if
      if(nvector .gt. 0) then
        call legendre_b_trans_vector_test(ncomp, nvector,               &
     &      sp_rlm_wk(1), vr_rtm_wk(1))
      end if
!
      call back_b_trans_fields_spin(ncomp, nvector, nscalar,            &
     &    vr_rtm_wk(1), vr_rtm(1))
!
      end subroutine leg_backward_trans_test
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_test(ncomp, nvector, nscalar)
!
      use m_work_4_sph_trans_spin
      use legendre_fwd_trans_testloop
      use ordering_schmidt_trans_spin
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      call order_f_trans_fields_spin(ncomp, nvector, nscalar,           &
     &    vr_rtm(1), vr_rtm_wk(1))
      call clear_fwd_legendre_work(ncomp)
!
      if(nvector .gt. 0) then
        call legendre_f_trans_vector_test(ncomp, nvector,               &
     &      vr_rtm_wk(1), sp_rlm_wk(1))
      end if
      if(nscalar .gt. 0) then
        call legendre_f_trans_scalar_test(ncomp, nvector, nscalar,      &
     &      vr_rtm_wk(1), sp_rlm_wk(1))
      end if
!
      call back_f_trans_fields_spin(ncomp, nvector, nscalar,            &
     &    sp_rlm_wk(1), sp_rlm(1))
!
      end subroutine leg_forward_trans_test
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_testloop

