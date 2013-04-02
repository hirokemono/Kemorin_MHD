!
!      module m_element_list_4_filter
!
      module m_element_list_4_filter
!
!     Written by H. Matsui on Oct., 2006
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: nele_4_filter
      integer(kind = kint), allocatable :: iele_4_filter(:)
!
      integer(kind = kint), allocatable :: iele_filter_smp_stack(:)
!     number of element on this PE
      integer( kind=kint )  ::  maxele_filter_4_smp = 0
!     number of node on this PE
!
!
!      subroutine allocate_ele_list_4_filter
!      subroutine allocate_ele_smp_stk_filter(np_smp)
!      subroutine deallocate_ele_liset_4_filter
!      subroutine deallocate_ele_smp_stk_filter
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_ele_list_4_filter
!
      allocate(iele_4_filter(nele_4_filter) )
      iele_4_filter = 0
!
      end subroutine allocate_ele_list_4_filter
!
! ----------------------------------------------------------------------
!
      subroutine allocate_ele_smp_stk_filter(np_smp)
!
      integer(kind = kint), intent(in) :: np_smp
!
      allocate(iele_filter_smp_stack(0:np_smp) )
      iele_filter_smp_stack = 0
!
      end subroutine allocate_ele_smp_stk_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_ele_liset_4_filter
!
      deallocate(iele_4_filter)
!
      end subroutine deallocate_ele_liset_4_filter
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_ele_smp_stk_filter
!
      deallocate(iele_filter_smp_stack )
!
      end subroutine deallocate_ele_smp_stk_filter
!
! ----------------------------------------------------------------------
!
      end module m_element_list_4_filter
