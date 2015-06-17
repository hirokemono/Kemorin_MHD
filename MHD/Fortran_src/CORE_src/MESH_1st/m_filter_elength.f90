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
      type(gradient_model_data_type), save :: FEM1_elen
!
      type(elen_nod_diffs_type), save ::  elenn
      type(elen_ele_diffs_type), save ::  elen1
!
      type(filter_config_type), save :: filter_conf1
!
      integer (kind = kint) :: nnod_filter_mom
!
!   FEM1_elen%nele_filter_mom
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
      call alloc_elen_ele_type(FEM1_elen%nele_filter_mom, elen1)
!
      end subroutine allocate_ele_length
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_nodal_ele_length
!
      call alloc_nodal_elen_type(nnod_filter_mom, elenn)
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
      call dealloc_nodal_elen_type(elenn)
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
