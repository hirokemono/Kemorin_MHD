!
!      module set_ele_import_items_peri
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine count_import_ele_peri(internal_node, numele,          &
!     &          nnod_4_ele, ie, num_import_ele, istack_import_ele,     &
!     &          ntot_import_ele, num_export_ele, istack_export_ele,    &
!     &          ntot_export_ele)
!      subroutine set_import_ele_peri(internal_node, numele,            &
!     &          nnod_4_ele, ie, istack_import_ele, ntot_import_ele,    &
!     &          item_import_ele)
!
      module set_ele_import_items_peri
!
      use m_precision
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
!
      subroutine count_import_ele_peri(internal_node, numele,           &
     &          nnod_4_ele, ie, num_import_ele, istack_import_ele,      &
     &          ntot_import_ele, num_export_ele, istack_export_ele,     &
     &          ntot_export_ele)
!
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(inout) :: ntot_import_ele
      integer(kind = kint), intent(inout) :: num_import_ele(1)
      integer(kind = kint), intent(inout) :: istack_import_ele(0:1)
      integer(kind = kint), intent(inout) :: ntot_export_ele
      integer(kind = kint), intent(inout) :: num_export_ele(1)
      integer(kind = kint), intent(inout) :: istack_export_ele(0:1)
!
      integer(kind = kint) :: iele
!
      num_import_ele = 0
      do iele = 1, numele
        if (ie(iele,1) .gt. internal_node) then
          num_import_ele(1) = num_import_ele(1) + 1
        end if
      end do
      istack_import_ele(0) = 0
      istack_import_ele(1) = num_import_ele(1)
      ntot_import_ele =      num_import_ele(1)
!
      num_export_ele(1) =   num_import_ele(1)
      istack_export_ele(0) = 0
      istack_export_ele(1) = num_export_ele(1)
      ntot_export_ele =      ntot_import_ele
!
      end subroutine count_import_ele_peri
!
!------------------------------------------------------------------
!
      subroutine set_import_ele_peri(internal_node, numele,             &
     &          nnod_4_ele, ie, istack_import_ele, ntot_import_ele,     &
     &          item_import_ele)
!
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: ntot_import_ele
      integer(kind = kint), intent(in) :: istack_import_ele(0:1)
!
      integer(kind = kint), intent(inout)                               &
     &              :: item_import_ele(ntot_import_ele)
!
      integer(kind = kint) :: iele, icou
!
!
      icou = istack_import_ele(0)
      do iele = 1, numele
        if (ie(iele,1) .gt. internal_node) then
          icou = icou + 1
          item_import_ele(icou) = iele
        end if
      end do
!
      end subroutine set_import_ele_peri
!
!------------------------------------------------------------------
!
      end module set_ele_import_items_peri
