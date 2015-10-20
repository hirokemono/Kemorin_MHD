!
!      module m_filter_elength
!
!     Written by H. Matsui
!
!      subroutine copy_filter_elen_ele_from_type(elen_e)
!      subroutine copy_filter_elen_ele_to_type(elen_e)
!
      module m_filter_elength
!
      use m_precision
      use t_filter_elength
!
      implicit none
!
      type(gradient_model_data_type), save :: FEM1_elen
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_elen_ele_from_type(elen_e)
!
      type(elen_ele_diffs_type), intent(in)  :: elen_e
!
!
      call copy_elength_type(FEM1_elen%nele_filter_mom,                 &
     &    elen_e%moms,  FEM1_elen%elen_ele%moms)
      call copy_elen_diffs_type(FEM1_elen%nele_filter_mom,              &
     &    elen_e%diff, FEM1_elen%elen_ele%diff)
      call copy_elen_diffs_type(FEM1_elen%nele_filter_mom,              &
     &    elen_e%diff2, FEM1_elen%elen_ele%diff2)
!
      end subroutine copy_filter_elen_ele_from_type
!
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_elen_ele_to_type(elen_e)
!
      type(elen_ele_diffs_type), intent(inout) :: elen_e
!
!
      call copy_elength_type(FEM1_elen%nele_filter_mom,                 &
     &    FEM1_elen%elen_ele%moms, elen_e%moms)
      call copy_elen_diffs_type(FEM1_elen%nele_filter_mom,              &
     &    FEM1_elen%elen_ele%diff, elen_e%diff)
      call copy_elen_diffs_type(FEM1_elen%nele_filter_mom,              &
     &    FEM1_elen%elen_ele%diff2, elen_e%diff2)
!
      end subroutine copy_filter_elen_ele_to_type
!
!  ---------------------------------------------------------------------
!
      end module m_filter_elength
