!
!     module set_area_4_searching
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_set_area_4_searching(new_ele)
!      subroutine deallocate_search_area
!
!
      module set_area_4_searching
!
      use m_precision
!
      use m_data_4_interpolate_org
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_area_4_searching(new_node, new_ele)
!
      use t_geometry_data
!
      use set_minmax_4_each_2nd_ele
      use set_org_ele_4_each_bin
!
      type(node_data), intent(in) :: new_node
      type(element_data), intent(in) :: new_ele
!
!
      call allocate_minmax_4_inter_org(new_ele%numele)
!
!      write(*,*) 's_set_minmax_sph_each_2nd_ele'
      call s_set_minmax_sph_each_2nd_ele(new_node, new_ele)
!        call check_minmax_sph_org(12, new_ele%numele)
!
      call allocate_nele_in_search_bin
!
!      write(*,*) 's_count_num_org_ele_4_each_bin'
      call s_count_num_org_ele_4_each_bin(new_node, new_ele)
!      write(*,*) 's_set_bin_stack_4_org_ele'
      call s_set_bin_stack_4_org_ele
!
!      write(*,*) 'allocate_iele_in_search_bin'
      call allocate_iele_in_search_bin
!       write(*,*) 's_set_org_ele_4_each_bin'
      call s_set_org_ele_4_each_bin(new_node, new_ele)
!
!
      end subroutine s_set_area_4_searching
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_search_area
!
      call deallocate_iele_in_search_bin
      call deallocate_minmax_4_inter_org
!
      end subroutine deallocate_search_area
!
! ----------------------------------------------------------------------
!
      end module set_area_4_searching
