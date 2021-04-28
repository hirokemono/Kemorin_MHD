!>@file   const_node_and_element_q2l.f90
!!@brief  module const_node_and_element_q2l
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2006
!!
!>@brief node list to construct tri-linear mesh from quad mesh
!!
!!@verbatim
!!      subroutine s_const_node_element_q2l(mesh_q, q_to_l,             &
!!     &                                    node_l, ele_l)
!!        type(mesh_geometry), intent(in) :: mesh_q
!!        type(quad_to_linear_list), intent(in) :: q_to_l
!!        type(node_data), intent(inout) :: node_l
!!        type(element_data), intent(inout) :: ele_l
!!@endverbatim
!
      module const_node_and_element_q2l
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
      use calypso_mpi
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_quad_to_linear_list
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_const_node_element_q2l(mesh_q, q_to_l,               &
     &                                    node_l, ele_l)
!
      use set_linear_node_ele_by_quad
!
      type(mesh_geometry), intent(in) :: mesh_q
      type(quad_to_linear_list), intent(in) :: q_to_l
!
      type(node_data), intent(inout) :: node_l
      type(element_data), intent(inout) :: ele_l
!
!
      call alloc_node_geometry_w_sph(node_l)
      call set_linear_node_by_quad                                      &
     &   (mesh_q%node, mesh_q%ele, mesh_q%surf, q_to_l,                 &
     &    node_l%numnod, node_l%inod_global, node_l%xx)
      call set_spherical_position(node_l)
!
!
      ele_l%numele = 8 * mesh_q%ele%numele
      ele_l%nnod_4_ele = num_t_linear
!
      call alloc_ele_connect(ele_l)
      call alloc_overlapped_ele(ele_l)
      call alloc_ele_geometry(ele_l)
      call set_linear_ele_connect_by_quad                               &
     &   (mesh_q%ele, mesh_q%surf, q_to_l,                              &
     &    ele_l%numele, ele_l%iele_global, ele_l%ie)
!
      end subroutine s_const_node_element_q2l
!
!-----------------------------------------------------------------------
!
      end module const_node_and_element_q2l
