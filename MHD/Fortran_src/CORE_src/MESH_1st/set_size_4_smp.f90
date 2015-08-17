!> @file  set_size_4_smp.f90
!!      module set_size_4_smp
!!
!! @author  H. Matsui
!! @date Programmed on Sep. 2002
!
!      subroutine count_overlap_element
!
!> @brief set numbers for SMP parallelization
!
      module set_size_4_smp
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_overlap_element
!
      use m_geometry_data
      use count_overlap
!
!
      call set_overlap_flag                                             &
     &   (np_smp, ele1%istack_ele_smp, node1%internal_node,             &
     &    ele1%numele, ele1%ie(1:ele1%numele,1), ele1%internal_ele,     &
     &    ele1%interior_ele)
      interior_ele = ele1%interior_ele
!
      end subroutine count_overlap_element
!
! ----------------------------------------------------------------------
!
      end module set_size_4_smp
