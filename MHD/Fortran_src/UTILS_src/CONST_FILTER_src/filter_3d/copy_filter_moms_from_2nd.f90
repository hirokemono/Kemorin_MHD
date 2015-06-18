!copy_filter_moms_from_2nd.f90
!      module copy_filter_moms_from_2nd
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine copy_elength_ele_from_2nd(nnod, nele, elen2_ele)
!      subroutine copy_filter_moments_from_2nd(new_node, new_ele)
!
      module copy_filter_moms_from_2nd
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine copy_elength_ele_from_2nd(nnod, nele, elen2_ele)
!
      use t_filter_elength
      use m_filter_elength
      use copy_filter_moment_type
!
      integer (kind = kint), intent(in) :: nnod, nele
      type(elen_ele_diffs_type), intent(in) :: elen2_ele
!
!
      FEM1_elen%nnod_filter_mom = nnod
      FEM1_elen%nele_filter_mom = nele
      call alloc_elen_ele_type                                          &
     &   (FEM1_elen%nele_filter_mom, FEM1_elen%elen_ele)
      call copy_filter_elen_ele_from_type(elen2_ele)
!
      end subroutine copy_elength_ele_from_2nd
!
!   --------------------------------------------------------------------
!
      subroutine copy_filter_moments_from_2nd(new_node, new_ele)
!
      use t_geometry_data
      use m_2nd_filter_moments
      use m_filter_moments
!
      type(node_data), intent(in) :: new_node
      type(element_data), intent(in) :: new_ele
!
!
      mom1%nnod_fmom = new_node%numnod
      call alloc_filter_moms_ele_type(new_ele%numele, mom1)
!
      call copy_filter_moms_ele                                         &
     &   (new_ele%numele, mom1%num_filter_moms, mom2_ele, mom1%mom_ele)
!
      call dealloc_filter_mom_ele_items                                 &
     &    (num_filter_moms_2nd, mom2_ele)
      deallocate(mom2_ele)
!
      end subroutine copy_filter_moments_from_2nd
!
!   --------------------------------------------------------------------
!
      end module copy_filter_moms_from_2nd
