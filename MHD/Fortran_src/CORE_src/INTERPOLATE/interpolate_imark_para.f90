!
!     module interpolate_imark_para
!
!     Written by H. Matsui on Sep., 2006
!
!
!      subroutine s_interporate_imark_para(np_smp, numnod, numele,      &
!     &          nnod_4_ele, ie, imark_org, num_dest_domain,            &
!     &          istack_tbl_wtype_smp, num_points, iele_gauss,          &
!     &          itype_gauss, imark)
!
!      subroutine s_interporate_imark_para_8(np_smp, numnod, numele,    &
!     &          nnod_4_ele, ie, imark_org, num_dest_domain,            &
!     &          istack_tbl_wtype_smp, num_points, iele_gauss,          &
!     &          itype_gauss, imark)
!      subroutine s_interporate_imark_para_20(np_smp, numnod, numele,   &
!     &          nnod_4_ele, ie, imark_org, num_dest_domain,            &
!     &          istack_tbl_wtype_smp, num_points, iele_gauss,          &
!     &          itype_gauss, imark)
!      subroutine s_interporate_imark_para_27(np_smp, numnod, numele,   &
!     &          nnod_4_ele, ie, imark_org, num_dest_domain,            &
!     &          istack_tbl_wtype_smp, num_points, iele_gauss,          &
!     &          itype_gauss, imark)
!
      module interpolate_imark_para
!
      use m_precision
!
      use interpolate_imark_1pe
!
      implicit none
!
      private :: s_interporate_imark_para_8
      private :: s_interporate_imark_para_20
      private :: s_interporate_imark_para_27
!
! ----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_interporate_imark_para(np_smp, numnod, numele,       &
     &          nnod_4_ele, ie, imark_org, num_dest_domain,             &
     &          istack_tbl_wtype_smp, num_points, iele_gauss,           &
     &          itype_gauss, imark)
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
      integer (kind = kint), intent(in) :: imark_org(numnod)
!
      integer (kind = kint), intent(inout) :: imark(num_points)
!
!
      if (nnod_4_ele .eq. 8)then
        call s_interporate_imark_para_8(np_smp, numnod, numele,         &
     &      nnod_4_ele, ie, imark_org, num_dest_domain,                 &
     &      istack_tbl_wtype_smp, num_points, iele_gauss,               &
     &      itype_gauss, imark)
      else if (nnod_4_ele .eq. 20)then
        call s_interporate_imark_para_20(np_smp, numnod, numele,        &
     &      nnod_4_ele, ie, imark_org, num_dest_domain,                 &
     &      istack_tbl_wtype_smp, num_points, iele_gauss,               &
     &      itype_gauss, imark)
      else if (nnod_4_ele .eq. 27)then
        call s_interporate_imark_para_27(np_smp, numnod, numele,        &
     &      nnod_4_ele, ie, imark_org, num_dest_domain,                 &
     &      istack_tbl_wtype_smp, num_points, iele_gauss,               &
     &      itype_gauss, imark)
      end if
!
      end subroutine s_interporate_imark_para
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_interporate_imark_para_8(np_smp, numnod, numele,     &
     &          nnod_4_ele, ie, imark_org, num_dest_domain,             &
     &          istack_tbl_wtype_smp, num_points, iele_gauss,           &
     &          itype_gauss, imark)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: num_dest_domain
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp*num_dest_domain)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      integer (kind = kint), intent(in) :: imark_org(numnod)
!
      integer (kind = kint), intent(inout) :: imark(num_points)
!
      integer(kind = kint) :: ip, ist
!
!
      do ip = 1, num_dest_domain
!
        ist = 4*np_smp*(ip-1)
        call s_interpolate_imark_8(np_smp, numnod, numele, nnod_4_ele,  &
     &      ie, imark_org, istack_tbl_wtype_smp(ist), num_points,       &
     &      iele_gauss, itype_gauss, imark)
!
      end do
!
      end subroutine s_interporate_imark_para_8
!
!  ---------------------------------------------------------------------
!
      subroutine s_interporate_imark_para_20(np_smp, numnod, numele,    &
     &          nnod_4_ele, ie, imark_org, num_dest_domain,             &
     &          istack_tbl_wtype_smp, num_points, iele_gauss,           &
     &          itype_gauss, imark)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: num_dest_domain
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp*num_dest_domain)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      integer (kind = kint), intent(in) :: imark_org(numnod)
!
      integer (kind = kint), intent(inout) :: imark(num_points)
!
      integer(kind = kint) :: ip, ist
!
!
      do ip = 1, num_dest_domain
!
        ist = 4*np_smp*(ip-1)
        call s_interpolate_imark_20(np_smp, numnod, numele, nnod_4_ele, &
     &      ie, imark_org, istack_tbl_wtype_smp(ist), num_points,       &
     &      iele_gauss, itype_gauss, imark)
!
      end do
!
      end subroutine s_interporate_imark_para_20
!
!  ---------------------------------------------------------------------
!
      subroutine s_interporate_imark_para_27(np_smp, numnod, numele,    &
     &          nnod_4_ele, ie, imark_org, num_dest_domain,             &
     &          istack_tbl_wtype_smp, num_points, iele_gauss,           &
     &          itype_gauss, imark)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: num_dest_domain
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp*num_dest_domain)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      integer (kind = kint), intent(in) :: imark_org(numnod)
!
      integer (kind = kint), intent(inout) :: imark(num_points)
!
      integer(kind = kint) :: ip, ist
!
!
      do ip = 1, num_dest_domain
!
        ist = 4*np_smp*(ip-1)
        call s_interpolate_imark_27(np_smp, numnod, numele, nnod_4_ele, &
     &      ie, imark_org, istack_tbl_wtype_smp(ist), num_points,       &
     &      iele_gauss, itype_gauss, imark)
!
      end do
!
      end subroutine s_interporate_imark_para_27
!
!  ---------------------------------------------------------------------
!
      end module interpolate_imark_para
