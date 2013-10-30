!>@file   interpolate_1pe.f90
!!@brief  module interpolate_1pe
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  interpolation on each subdomains
!!
!!@verbatim
!!      subroutine interporate_scalar_para(np_smp, numnod, numele,      &
!!     &          nnod_4_ele, ie, v_org, istack_tbl_wtype_smp,          &
!!     &          num_points, iele_gauss, itype_gauss, xi_gauss, vect)
!!      subroutine interporate_vector_para(np_smp, numnod, numele,      &
!!     &          nnod_4_ele, ie, v_org, istack_tbl_wtype_smp,          &
!!     &          num_points, iele_gauss, itype_gauss, xi_gauss, vect)
!!      subroutine interporate_tensor_para(np_smp, numnod, numele,      &
!!     &          nnod_4_ele,ie, v_org, istack_tbl_wtype_smp,           &
!!     &          num_points, iele_gauss, itype_gauss, xi_gauss, vect)
!!      subroutine interporate_fields_para(np_smp, numnod, numele,      &
!!     &          nnod_4_ele, ie, numdir, v_org, istack_tbl_wtype_smp,  &
!!     &          num_points, iele_gauss, itype_gauss, xi_gauss, vect)
!!@endverbatim
!
      module interpolate_1pe
!
      use m_precision
!
      use interpolate_1pe_linear
      use interpolate_1pe_quad
      use interpolate_1pe_lag
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine interporate_scalar_para(np_smp, numnod, numele,        &
     &          nnod_4_ele, ie, v_org, istack_tbl_wtype_smp,            &
     &          num_points, iele_gauss, itype_gauss, xi_gauss, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: nnod_4_ele
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(numnod)
!
      real (kind=kreal), intent(inout) :: vect(num_points)
!
!
      if (nnod_4_ele .eq. 8)then
        call s_interpolate_scalar_8(np_smp, numnod, numele, ie,         &
     &      v_org, istack_tbl_wtype_smp, num_points,                    &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      else if (nnod_4_ele .eq. 20)then
        call s_interpolate_scalar_20(np_smp, numnod, numele, ie,        &
     &      v_org, istack_tbl_wtype_smp, num_points,                    &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      else if (nnod_4_ele .eq. 27)then
        call s_interpolate_scalar_27(np_smp, numnod, numele, ie,        &
     &      v_org, istack_tbl_wtype_smp, num_points,                    &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      end if
!
      end subroutine interporate_scalar_para
!
!  ---------------------------------------------------------------------
!
      subroutine interporate_vector_para(np_smp, numnod, numele,        &
     &          nnod_4_ele, ie, v_org, istack_tbl_wtype_smp,            &
     &          num_points, iele_gauss, itype_gauss, xi_gauss, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: nnod_4_ele
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(3*numnod)
!
      real (kind=kreal), intent(inout) :: vect(3*num_points)
!
!
      if (nnod_4_ele .eq. 8)then
        call s_interpolate_vector_8(np_smp, numnod, numele, ie,         &
     &      v_org, istack_tbl_wtype_smp, num_points,                    &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      else if (nnod_4_ele .eq. 20)then
        call s_interpolate_vector_20(np_smp, numnod, numele, ie,        &
     &      v_org, istack_tbl_wtype_smp, num_points,                    &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      else if (nnod_4_ele .eq. 27)then
        call s_interpolate_vector_27(np_smp, numnod, numele, ie,        &
     &      v_org, istack_tbl_wtype_smp, num_points,                    &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      end if
!
      end subroutine interporate_vector_para
!
!  ---------------------------------------------------------------------
!
      subroutine interporate_tensor_para(np_smp, numnod, numele,        &
     &          nnod_4_ele,ie, v_org, istack_tbl_wtype_smp,             &
     &          num_points, iele_gauss, itype_gauss, xi_gauss, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: nnod_4_ele
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(6*numnod)
!
      real (kind=kreal), intent(inout) :: vect(6*num_points)
!
!
      if (nnod_4_ele .eq. 8)then
        call s_interpolate_tensor_8(np_smp, numnod, numele, ie,         &
     &      v_org, istack_tbl_wtype_smp, num_points,                    &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      else if (nnod_4_ele .eq. 20)then
        call s_interpolate_tensor_20(np_smp, numnod, numele, ie,        &
     &      v_org, istack_tbl_wtype_smp, num_points,                    &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      else if (nnod_4_ele .eq. 27)then
        call s_interpolate_tensor_27(np_smp, numnod, numele, ie,        &
     &      v_org, istack_tbl_wtype_smp, num_points,                    &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      end if
!
      end subroutine interporate_tensor_para
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine interporate_fields_para(np_smp, numnod, numele,        &
     &          nnod_4_ele, ie, numdir, v_org, istack_tbl_wtype_smp,    &
     &          num_points, iele_gauss, itype_gauss, xi_gauss, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: nnod_4_ele
      integer (kind = kint), intent(in) :: numnod, numele, numdir
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(numdir*numnod)
!
      real (kind=kreal), intent(inout) :: vect(numdir*num_points)
!
!
      if (nnod_4_ele .eq. 8)then
        call s_interpolate_fields_8(np_smp, numnod, numele, ie,         &
     &      numdir, v_org, istack_tbl_wtype_smp,                        &
     &      num_points, iele_gauss, itype_gauss, xi_gauss, vect)
      else if (nnod_4_ele .eq. 20)then
        call s_interpolate_fields_20(np_smp, numnod, numele, ie,        &
     &      numdir, v_org, istack_tbl_wtype_smp, num_points,            &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      else if (nnod_4_ele .eq. 27)then
        call s_interpolate_fields_27(np_smp, numnod, numele, ie,        &
     &      numdir, v_org, istack_tbl_wtype_smp, num_points,            &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      end if
!
      end subroutine interporate_fields_para
!
! ----------------------------------------------------------------------
!
      end module interpolate_1pe
