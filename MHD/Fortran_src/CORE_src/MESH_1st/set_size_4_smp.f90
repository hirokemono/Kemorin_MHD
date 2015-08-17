!> @file  set_size_4_smp.f90
!!      module set_size_4_smp
!!
!! @author  H. Matsui
!! @date Programmed on Sep. 2002
!
!      subroutine count_size_4_sheard_para
!      subroutine count_surf_size_4_smp
!      subroutine count_edge_size_4_smp
!
!      subroutine count_overlap_element
!      subroutine count_overlap_surface
!      subroutine count_overlap_edge
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
      subroutine set_smp_data_4_node
!
      use m_geometry_data
      use cal_minmax_and_stacks
!
      call allocate_node_param_smp_type(node1)
!
      call count_number_4_smp( np_smp, ione, node1%numnod,              &
     &       node1%istack_nod_smp, node1%max_nod_smp)
      call count_number_4_smp(np_smp, ione, node1%internal_node,        &
     &       node1%istack_internal_smp, node1%max_internal_nod_smp)
!
      end subroutine set_smp_data_4_node
!
!-----------------------------------------------------------------------
!
      subroutine count_size_4_sheard_para
!
      use m_geometry_data
      use cal_minmax_and_stacks
!
!
      call set_smp_data_4_node
!
      call allocate_ele_param_smp_type(ele1)
!
      call count_number_4_smp(np_smp, ione, ele1%numele,                &
     &    ele1%istack_ele_smp, ele1%max_ele_smp)
!
      end subroutine count_size_4_sheard_para
!
!-----------------------------------------------------------------------
!
      subroutine count_surf_size_4_smp
!
      use m_geometry_data
      use cal_minmax_and_stacks
!
!
      call allocate_surf_param_smp_type(surf1)
!
      call count_number_4_smp(np_smp, ione, surf1%numsurf,              &
     &    surf1%istack_surf_smp, surf1%max_surf_smp)
!
      end subroutine count_surf_size_4_smp
!
!-----------------------------------------------------------------------
!
      subroutine count_edge_size_4_smp
!
      use m_geometry_data
      use cal_minmax_and_stacks
!
!
      call allocate_edge_param_smp_type(edge1)
!
      call count_number_4_smp(np_smp, ione, edge1%numedge,              &
     &    edge1%istack_edge_smp, edge1%max_edge_smp)
!
      end subroutine count_edge_size_4_smp
!
!-----------------------------------------------------------------------
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
     &    interior_ele)
!
      call copy_real_overlap_flag(np_smp, ele1%istack_ele_smp,          &
     &    ele1%numele, interior_ele, e_multi)
!
      end subroutine count_overlap_element
!
! ----------------------------------------------------------------------
!
      subroutine count_overlap_surface
!
      use m_geometry_data
      use count_overlap
!
      call set_overlap_flag                                             &
     &   (np_smp, surf1%istack_surf_smp, node1%internal_node,           &
     &    surf1%numsurf, surf1%ie_surf, surf1%internal_surf,            &
     &    interior_surf)
!
      end subroutine count_overlap_surface
!
! ----------------------------------------------------------------------
!
      subroutine count_overlap_edge
!
      use m_geometry_data
      use count_overlap
!
      call set_overlap_flag                                             &
     &   (np_smp, edge1%istack_edge_smp, node1%internal_node,           &
     &    edge1%numedge, edge1%ie_edge, edge1%internal_edge,            &
     &    interior_edge)
!
      end subroutine count_overlap_edge
!
! ----------------------------------------------------------------------
!
      end module set_size_4_smp
