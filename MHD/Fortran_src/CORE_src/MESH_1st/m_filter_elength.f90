!
!      module m_filter_elength
!
!     Written by H. Matsui
!
!       subroutine allocate_ele_length
!       subroutine allocate_nodal_ele_length
!       subroutine allocate_ref_1d_moment
!
!       subroutine deallocate_filter_moments
!
!       subroutine deallocate_ele_length
!       subroutine deallocate_nodal_ele_length
!       subroutine deallocate_ref_1d_moment
!
      module m_filter_elength
!
      use m_precision
      use t_filter_elength
!
      implicit none
!
!
      type(elen_on_ele_type), save ::  elen_n
      type(elen_diffs_type), save ::  diff1_n
!
      type(elen_ele_diffs_type), save ::  elen1
!   elen1%diff2

      type(filter_config_type), save :: filter_conf1
!
      integer (kind = kint) :: nnod_filter_mom, nele_filter_mom
!
!   filter_conf1%isgs_4_div
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ele_length
!
!
      call alloc_elen_ele_type(nele_filter_mom, elen1)
!
      end subroutine allocate_ele_length
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_nodal_ele_length
!
      call alloc_elen_on_ele_type(nnod_filter_mom, elen_n)
      call alloc_elen_diffs_type(nnod_filter_mom, diff1_n)
!
      end subroutine allocate_nodal_ele_length
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_ref_1d_moment
!
      call alloc_ref_1d_mom_type(filter_conf1)
!
      end subroutine allocate_ref_1d_moment
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine deallocate_filter_moments
!
        call deallocate_ele_length
        call deallocate_nodal_ele_length
        call deallocate_ref_1d_moment
!
       end subroutine deallocate_filter_moments
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine deallocate_ele_length
!
      call dealloc_elen_type(elen1)
!
       end subroutine deallocate_ele_length
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_nodal_ele_length
!
!
      call dealloc_elen_on_ele_type(elen_n)
      call dealloc_elen_diffs_type(diff1_n)
!
       end subroutine deallocate_nodal_ele_length
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ref_1d_moment
!
      call dealloc_ref_1d_mom_type(filter_conf1)
!
      end subroutine deallocate_ref_1d_moment
!
!  ---------------------------------------------------------------------
!
      end module m_filter_elength
