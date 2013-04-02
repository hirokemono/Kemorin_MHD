!
!     module set_area_4_searching
!
      module set_area_4_searching
!
!     Written by H. Matsui on Sep., 2006
!
      use m_precision
!
      use m_data_4_interpolate_org
!
      implicit none
!
!      subroutine s_set_area_4_searching
!      subroutine deallocate_search_area
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_area_4_searching
!
      use set_minmax_4_each_2nd_ele
      use set_org_ele_4_each_bin
!
!
      call allocate_minmax_4_inter_org
!
!      write(*,*) 's_set_minmax_sph_each_2nd_ele'
      call s_set_minmax_sph_each_2nd_ele
!        call check_minmax_sph_org(12)
!
      call allocate_nele_in_search_bin
!
!      write(*,*) 's_count_num_org_ele_4_each_bin'
      call s_count_num_org_ele_4_each_bin
!      write(*,*) 's_set_bin_stack_4_org_ele'
      call s_set_bin_stack_4_org_ele
!
!      write(*,*) 'allocate_iele_in_search_bin'
      call allocate_iele_in_search_bin
!       write(*,*) 's_set_org_ele_4_each_bin'
      call s_set_org_ele_4_each_bin
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
