!m_2nd_filter_ele_length.f90
!      module m_2nd_filter_ele_length
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine allocate_2nd_ele_length(nele_2nd)
!      subroutine deallocate_2nd_ele_length
!
      module m_2nd_filter_ele_length
!
      use m_precision
      use m_constants
      use t_filter_elength
!
      implicit none
!
      type(elen_on_ele_type), save :: elen_2
      type(elen_diffs_type), save :: diff1_2
      type(elen_diffs_type), save :: diff2_2
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_2nd_ele_length(nele_2nd)
!
      integer(kind = kint), intent(in) :: nele_2nd
!
!
      call alloc_elen_on_ele_type(nele_2nd, elen_2)
      call alloc_elen_diffs_type(nele_2nd, diff1_2)
      call alloc_elen_diffs_type(nele_2nd, diff2_2)
!
      end subroutine allocate_2nd_ele_length
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_2nd_ele_length
!
!
      call dealloc_elen_on_ele_type(elen_2)
      call dealloc_elen_diffs_type(diff1_2)
      call dealloc_elen_diffs_type(diff2_2)
!
      end subroutine deallocate_2nd_ele_length
!
!   --------------------------------------------------------------------
!
      end module m_2nd_filter_ele_length
