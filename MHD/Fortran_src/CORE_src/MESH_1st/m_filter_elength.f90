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
      subroutine allocate_ele_length
!
!
      call alloc_elen_ele_type                                          &
     &   (FEM1_elen%nele_filter_mom, FEM1_elen%elen_ele)
!
      end subroutine allocate_ele_length
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_nodal_ele_length
!
      call alloc_nodal_elen_type                   &
     &   (FEM1_elen%nnod_filter_mom, FEM1_elen%elen_nod)
!
      end subroutine allocate_nodal_ele_length
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_ref_1d_moment
!
      call alloc_ref_1d_mom_type(FEM1_elen%filter_conf)
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
      call dealloc_elen_type(FEM1_elen%elen_ele)
!
       end subroutine deallocate_ele_length
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_nodal_ele_length
!
!
      call dealloc_nodal_elen_type(FEM1_elen%elen_nod)
!
       end subroutine deallocate_nodal_ele_length
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ref_1d_moment
!
      call dealloc_ref_1d_mom_type(FEM1_elen%filter_conf)
!
      end subroutine deallocate_ref_1d_moment
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_elen_ele_from_type(elen_e)
!
      use copy_filter_moment_type
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
      use copy_filter_moment_type
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
