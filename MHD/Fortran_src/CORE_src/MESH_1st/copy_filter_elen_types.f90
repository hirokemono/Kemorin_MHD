!
!      module copy_filter_elen_types
!
!     Written by H. Matsui
!
!      subroutine copy_filter_elen_ele_from_type(elen_e)
!      subroutine copy_filter_elen_ele_to_type(elen_e)
!        type(ele_mom_diffs_type), intent(in)  :: elen_e(num_filter_moms)
!
      module copy_filter_elen_types
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_elen_ele_from_type(elen_e)
!
      use t_filter_elength
      use m_filter_elength
!
      type(ele_mom_diffs_type), intent(in)  :: elen_e
!
!
      call copy_elength_type(nele_filter_mom, elen_e%moms,  elen1%moms)
      call copy_elen_diffs_type(nele_filter_mom, elen_e%diff, elen1%diff)
      call copy_elen_diffs_type(nele_filter_mom, elen_e%diff2, elen1%diff2)
!
      end subroutine copy_filter_elen_ele_from_type
!
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_elen_ele_to_type(elen_e)
!
      use t_filter_elength
      use m_filter_elength
!
      type(ele_mom_diffs_type), intent(inout) :: elen_e
!
!
      call copy_elength_type(nele_filter_mom, elen1%moms, elen_e%moms)
      call copy_elen_diffs_type(nele_filter_mom, elen1%diff, elen_e%diff)
      call copy_elen_diffs_type(nele_filter_mom, elen1%diff2, elen_e%diff2)
!
      end subroutine copy_filter_elen_ele_to_type
!
!  ---------------------------------------------------------------------
!
      end module copy_filter_elen_types
