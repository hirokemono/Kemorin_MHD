!
!     module interpolate_parallel
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_interporate_scalar_para(np_smp, numnod, numele,     &
!     &          nnod_4_ele, ie, v_org, num_dest_domain,                &
!     &          istack_tbl_wtype_smp, num_points, iele_gauss,          &
!     &          itype_gauss, xi_gauss, vect)
!      subroutine s_interporate_vector_para(np_smp, numnod, numele,     &
!     &          nnod_4_ele, ie, v_org, num_dest_domain,                &
!     &          istack_tbl_wtype_smp, num_points, iele_gauss,          &
!     &          itype_gauss, xi_gauss, vect)
!      subroutine s_interporate_tensor_para(np_smp, numnod, numele,     &
!     &          nnod_4_ele,ie, v_org, num_dest_domain,                 &
!     &          istack_tbl_wtype_smp, num_points, iele_gauss,          &
!     &          itype_gauss, xi_gauss, vect)
!      subroutine s_interporate_fields_para(np_smp, numnod, numele,     &
!     &          nnod_4_ele, ie, numdir, v_org, num_dest_domain,        &
!     &          istack_tbl_wtype_smp, num_points, iele_gauss,          &
!     &          itype_gauss, xi_gauss, vect)
!
      module interpolate_parallel
!
      use m_precision
!
      use interpolate_para_linear
      use interpolate_para_quad
      use interpolate_para_lag
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_interporate_scalar_para(np_smp, numnod, numele,      &
     &          nnod_4_ele, ie, v_org, num_dest_domain,                 &
     &          istack_tbl_wtype_smp, num_points, iele_gauss,           &
     &          itype_gauss, xi_gauss, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: nnod_4_ele
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: num_dest_domain
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp*num_dest_domain)
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
        call s_interporate_scalar_para_8(np_smp, numnod, numele, ie,    &
     &      v_org, num_dest_domain, istack_tbl_wtype_smp, num_points,   &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      else if (nnod_4_ele .eq. 20)then
        call s_interporate_scalar_para_20(np_smp, numnod, numele, ie,   &
     &      v_org, num_dest_domain, istack_tbl_wtype_smp, num_points,   &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      else if (nnod_4_ele .eq. 27)then
        call s_interporate_scalar_para_27(np_smp, numnod, numele, ie,   &
     &      v_org, num_dest_domain, istack_tbl_wtype_smp, num_points,   &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      end if
!
      end subroutine s_interporate_scalar_para
!
!  ---------------------------------------------------------------------
!
      subroutine s_interporate_vector_para(np_smp, numnod, numele,      &
     &          nnod_4_ele, ie, v_org, num_dest_domain,                 &
     &          istack_tbl_wtype_smp, num_points, iele_gauss,           &
     &          itype_gauss, xi_gauss, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: nnod_4_ele
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: num_dest_domain
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp*num_dest_domain)
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
        call s_interporate_vector_para_8(np_smp, numnod, numele, ie,    &
     &      v_org, num_dest_domain, istack_tbl_wtype_smp, num_points,   &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      else if (nnod_4_ele .eq. 20)then
        call s_interporate_vector_para_20(np_smp, numnod, numele, ie,   &
     &      v_org, num_dest_domain, istack_tbl_wtype_smp, num_points,   &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      else if (nnod_4_ele .eq. 27)then
        call s_interporate_vector_para_27(np_smp, numnod, numele, ie,   &
     &      v_org, num_dest_domain, istack_tbl_wtype_smp, num_points,   &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      end if
!
      end subroutine s_interporate_vector_para
!
!  ---------------------------------------------------------------------
!
      subroutine s_interporate_tensor_para(np_smp, numnod, numele,      &
     &          nnod_4_ele,ie, v_org, num_dest_domain,                  &
     &          istack_tbl_wtype_smp, num_points, iele_gauss,           &
     &          itype_gauss, xi_gauss, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: nnod_4_ele
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: num_dest_domain
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp*num_dest_domain)
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
        call s_interporate_tensor_para_8(np_smp, numnod, numele, ie,    &
     &      v_org, num_dest_domain, istack_tbl_wtype_smp, num_points,   &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      else if (nnod_4_ele .eq. 20)then
        call s_interporate_tensor_para_20(np_smp, numnod, numele, ie,   &
     &      v_org, num_dest_domain, istack_tbl_wtype_smp, num_points,   &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      else if (nnod_4_ele .eq. 27)then
        call s_interporate_tensor_para_27(np_smp, numnod, numele, ie,   &
     &      v_org, num_dest_domain, istack_tbl_wtype_smp, num_points,   &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      end if
!
      end subroutine s_interporate_tensor_para
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_interporate_fields_para(np_smp, numnod, numele,      &
     &          nnod_4_ele, ie, numdir, v_org, num_dest_domain,         &
     &          istack_tbl_wtype_smp, num_points, iele_gauss,           &
     &          itype_gauss, xi_gauss, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: nnod_4_ele
      integer (kind = kint), intent(in) :: numnod, numele, numdir
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: num_dest_domain
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp*num_dest_domain)
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
        call s_interporate_fields_para_8(np_smp, numnod, numele, ie,    &
     &      numdir, v_org, num_dest_domain, istack_tbl_wtype_smp,       &
     &      num_points, iele_gauss, itype_gauss, xi_gauss, vect)
      else if (nnod_4_ele .eq. 20)then
        call s_interporate_fields_para_20(np_smp, numnod, numele, ie,   &
     &      numdir, v_org, num_dest_domain, istack_tbl_wtype_smp,       &
     &      num_points, iele_gauss, itype_gauss, xi_gauss, vect)
      else if (nnod_4_ele .eq. 27)then
        call s_interporate_fields_para_27(np_smp, numnod, numele, ie,   &
     &      numdir, v_org, num_dest_domain, istack_tbl_wtype_smp,       &
     &      num_points, iele_gauss, itype_gauss, xi_gauss, vect)
      end if
!
!
      end subroutine s_interporate_fields_para
!
! ----------------------------------------------------------------------
!
      end module interpolate_parallel
