!
!     module interpolate_imark_1pe
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_interpolate_imark_8(np_smp, numnod, numele,         &
!     &          nnod_4_ele, ie, imark_org, istack_wtype_smp,           &
!     &          num_points, iele_gauss, itype_gauss, imark)
!      subroutine s_interpolate_imark_20(np_smp, numnod, numele,        &
!     &          nnod_4_ele, ie, imark_org, istack_wtype_smp,           &
!     &          num_points, iele_gauss, itype_gauss, imark)
!      subroutine s_interpolate_imark_27(np_smp, numnod, numele,        &
!     &          nnod_4_ele, ie, imark_org, istack_wtype_smp,           &
!     &          num_points, iele_gauss, itype_gauss, imark)
!
      module interpolate_imark_1pe
!
      use m_precision
!
      use interpolate_on_node
      use interpolate_imark_edge
      use interpolate_imark_surf
      use interpolate_imark_ele
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_interpolate_imark_8(np_smp, numnod, numele,          &
     &          nnod_4_ele, ie, imark_org, istack_wtype_smp,            &
     &          num_points, iele_gauss, itype_gauss, imark)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: istack_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      integer (kind = kint), intent(in) :: imark_org(numnod)
!
      integer (kind = kint), intent(inout) :: imark(num_points)
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      call s_interpolate_imark_node(np_smp, numnod, numele,             &
     &    nnod_4_ele, ie, imark_org, istack_wtype_smp(ist), num_points, &
     &    iele_gauss, itype_gauss, imark)
!
      ist = np_smp
      call s_interpolate_imark_edge2(np_smp, numnod, numele, ie,        &
     &    imark_org, istack_wtype_smp(ist), num_points, iele_gauss,     &
     &    itype_gauss, imark)
!
      ist = 2*np_smp
      call s_interpolate_imark_surf4(np_smp, numnod, numele, ie,        &
     &    imark_org, istack_wtype_smp(ist), num_points, iele_gauss,     &
     &    itype_gauss, imark)
!
      ist = 3*np_smp
      call s_interpolate_imark_ele8(np_smp, numnod, numele, ie,         &
     &    imark_org, istack_wtype_smp(ist), num_points, iele_gauss,     &
     &    imark)
!
      end subroutine s_interpolate_imark_8
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_imark_20(np_smp, numnod, numele,         &
     &          nnod_4_ele, ie, imark_org, istack_wtype_smp,            &
     &          num_points, iele_gauss, itype_gauss, imark)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: istack_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      integer (kind = kint), intent(in) :: imark_org(numnod)
!
      integer (kind = kint), intent(inout) :: imark(num_points)
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      call s_interpolate_imark_node(np_smp, numnod, numele,             &
     &    nnod_4_ele, ie, imark_org, istack_wtype_smp(ist), num_points, &
     &    iele_gauss, itype_gauss, imark)
!
      ist = np_smp
      call s_interpolate_imark_edge3(np_smp, numnod, numele, ie,        &
     &    imark_org, istack_wtype_smp(ist), num_points, iele_gauss,     &
     &    itype_gauss, imark)
!
      ist = 2*np_smp
      call s_interpolate_imark_surf8(np_smp, numnod, numele, ie,        &
     &    imark_org, istack_wtype_smp(ist), num_points, iele_gauss,     &
     &    itype_gauss, imark)
!
      ist = 3*np_smp
      call s_interpolate_imark_ele20(np_smp, numnod, numele, ie,        &
     &    imark_org, istack_wtype_smp(ist), num_points, iele_gauss,     &
     &    imark)
!
      end subroutine s_interpolate_imark_20
!
!  ---------------------------------------------------------------------
!
      subroutine s_interpolate_imark_27(np_smp, numnod, numele,         &
     &          nnod_4_ele, ie, imark_org, istack_wtype_smp,            &
     &          num_points, iele_gauss, itype_gauss, imark)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: istack_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      integer (kind = kint), intent(in) :: imark_org(numnod)
!
      integer (kind = kint), intent(inout) :: imark(num_points)
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      call s_interpolate_imark_node(np_smp, numnod, numele,             &
     &    nnod_4_ele, ie, imark_org, istack_wtype_smp(ist), num_points, &
     &    iele_gauss, itype_gauss, imark)
!
      ist = np_smp
      call s_interpolate_imark_edge3(np_smp, numnod, numele, ie,        &
     &    imark_org, istack_wtype_smp(ist), num_points, iele_gauss,     &
     &    itype_gauss, imark)
!
      ist = 2*np_smp
      call s_interpolate_imark_surf9(np_smp, numnod, numele, ie,        &
     &    imark_org, istack_wtype_smp(ist), num_points, iele_gauss,     &
     &    itype_gauss, imark)
!
      ist = 3*np_smp
      call s_interpolate_imark_ele27(np_smp, numnod, numele, ie,        &
     &    imark_org, istack_wtype_smp(ist), num_points, iele_gauss,     &
     &    imark)
!
      end subroutine s_interpolate_imark_27
!
!  ---------------------------------------------------------------------
!
      end module interpolate_imark_1pe
