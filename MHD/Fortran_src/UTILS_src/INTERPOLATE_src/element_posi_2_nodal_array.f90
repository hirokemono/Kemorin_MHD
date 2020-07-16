!>@file  element_posi_2_nodal_array.f90
!!       module element_posi_2_nodal_array
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Copy element position into node structure
!!
!!@verbatim
!!@verbatim
!!      subroutine s_element_posi_2_nodal_array(ele, node)
!!      subroutine s_2nd_ele_posi_2_nodal_array(newmesh)
!!@endverbatim
!
      module element_posi_2_nodal_array
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_geometry_data
!
      implicit none
!
      private :: ele_point_to_node_type, ele_sph_point_to_node_type
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_element_posi_2_nodal_array(ele, node)
!
      use cal_minmax_and_stacks
!
      type(element_data), intent(in) :: ele
      type(node_data), intent(inout) :: node
!
!
      call dealloc_node_geometry_w_sph(node)
!
      node%numnod =        ele%numele
      node%internal_node = ele%numele
!
      call alloc_node_geometry_w_sph(node)
!
      call ele_point_to_node_type(ele, node)
      call ele_sph_point_to_node_type(ele, node)
!
      call count_number_4_smp( np_smp, ione, node%numnod,               &
     &    node%istack_nod_smp, node%max_nod_smp)
!
      call count_number_4_smp( np_smp, ione, node%internal_node,        &
     &    node%istack_internal_smp, node%max_internal_nod_smp)
!
      end subroutine s_element_posi_2_nodal_array
!
! ----------------------------------------------------------------------
!
      subroutine s_2nd_ele_posi_2_nodal_array(newmesh)
!
      use t_mesh_data
      use const_mesh_information
!
      type(mesh_geometry), intent(inout) :: newmesh
!
!
      call set_nod_and_ele_infos(newmesh%node, newmesh%ele)
      call s_element_posi_2_nodal_array(newmesh%ele, newmesh%node)
!
      end subroutine s_2nd_ele_posi_2_nodal_array
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine ele_point_to_node_type(ele, node)
!
      type(element_data), intent(in) :: ele
      type(node_data), intent(inout) :: node
!
!$omp parallel workshare
      node%inod_global(1:node%numnod) = ele%iele_global(1:node%numnod)
      node%xx(1:node%numnod,1) =        ele%x_ele(1:node%numnod,1)
      node%xx(1:node%numnod,2) =        ele%x_ele(1:node%numnod,2)
      node%xx(1:node%numnod,3) =        ele%x_ele(1:node%numnod,3)
!$omp end parallel workshare
!
      end subroutine ele_point_to_node_type
!
! ----------------------------------------------------------------------
!
      subroutine ele_sph_point_to_node_type(ele, node)
!
      type(element_data), intent(in) :: ele
      type(node_data), intent(inout) :: node
!
!$omp parallel workshare
      node%rr(1:node%numnod) =    ele%r_ele(1:node%numnod)
      node%a_r(1:node%numnod) =   ele%ar_ele(1:node%numnod)
      node%ss(1:node%numnod) =    ele%s_ele(1:node%numnod)
      node%a_s(1:node%numnod) =   ele%as_ele(1:node%numnod)
      node%phi(1:node%numnod) =   ele%phi_ele(1:node%numnod)
      node%theta(1:node%numnod) = ele%theta_ele(1:node%numnod)
!$omp end parallel workshare
!
      end subroutine ele_sph_point_to_node_type
!
! ----------------------------------------------------------------------
!
      end module element_posi_2_nodal_array
