!>@file   const_element_comm_tables_old.f90
!!@brief  module const_element_comm_tables_old
!!
!!@author H. Matsui
!!@date Programmed in June, 2015
!
!> @brief Belonged element list for each node
!!
!!@verbatim
!!      subroutine const_ele_comm_tbl_old(node, nod_comm, ele_comm, ele)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(inout) :: ele_comm
!!        type(element_data), intent(inout) :: ele
!!      subroutine const_surf_comm_table_old                                &
!!     &         (node, nod_comm, surf_comm, surf, fail_tbl)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(inout) :: surf_comm
!!        type(surface_data), intent(inout) :: surf
!!        type(failed_table), intent(inout) :: fail_tbl
!!      subroutine const_edge_comm_table_old                                &
!!     &         (node, nod_comm, edge_comm, edge, fail_tbl)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(inout) :: edge_comm
!!        type(edge_data), intent(inout) :: edge
!!        type(failed_table), intent(inout) :: fail_tbl
!!
!!      subroutine const_global_numnod_list(node)
!!@endverbatim
!
      module const_element_comm_tables_old
!
      use m_precision
      use calypso_mpi
      use t_next_node_ele_4_node
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_comm_table
      use t_belonged_element_4_node
      use t_next_node_ele_4_node
      use t_failed_export_list
!
      use m_machine_parameter
!
      implicit none
!
      character(len=kchara), parameter :: txt_ele =  'element'
      character(len=kchara), parameter :: txt_edge = 'edge'
      character(len=kchara), parameter :: txt_surf = 'surface'
!
      private :: txt_ele, txt_edge, txt_surf
!      private :: const_global_element_id
!      private :: const_global_surface_id, const_global_edge_id
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_ele_comm_tbl_old(node, nod_comm, ele_comm, ele)
!
      use set_ele_id_4_node_type
      use const_element_comm_table
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(inout) :: ele_comm
      type(element_data), intent(inout) :: ele
!
      type(belonged_table), save :: belongs
      type(failed_table), save :: fail_tbl_e
!
!
      call set_ele_id_4_node(node, ele, belongs%blng_ele)
      call alloc_x_ref_ele(node, belongs)
      call sort_inod_4_ele_by_position(ione, ele%numele, ele%x_ele,     &
     &    node, belongs%blng_ele, belongs%x_ref_ele)
!
      call alloc_failed_export(0, fail_tbl_e)
      call belonged_ele_id_4_node(node, ele, belongs%host_ele)
      call const_comm_table_by_connenct_old                             &
     &   (txt_ele, ele%numele, ele%nnod_4_ele, ele%ie,                  &
     &    ele%interior_ele, ele%x_ele, node, nod_comm,                  &
     &    belongs%blng_ele, belongs%x_ref_ele, belongs%host_ele,        &
     &    ele_comm, fail_tbl_e)
      call dealloc_failed_export(fail_tbl_e)
      call dealloc_iele_belonged(belongs%host_ele)
      call dealloc_x_ref_ele(belongs)
      call dealloc_iele_belonged(belongs%blng_ele)
!
      call const_global_element_id(ele_comm, ele)
!
      end subroutine const_ele_comm_tbl_old
!
!-----------------------------------------------------------------------
!
      subroutine const_surf_comm_table_old                                  &
     &         (node, nod_comm, surf_comm, surf, fail_tbl)
!
      use set_ele_id_4_node_type
      use const_element_comm_table
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(inout) :: surf_comm
      type(surface_data), intent(inout) :: surf
      type(failed_table), intent(inout) :: fail_tbl
!
      type(belonged_table), save :: belongs
!
!
      call set_surf_id_4_node(node, surf, belongs%blng_surf)
      call alloc_x_ref_surf(node, belongs)
      call sort_inod_4_ele_by_position(ione, surf%numsurf, surf%x_surf, &
     &    node, belongs%blng_surf, belongs%x_ref_surf)
!
      call belonged_surf_id_4_node(node, surf, belongs%host_surf)
      call const_comm_table_by_connenct_old                             &
     &   (txt_surf, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,       &
     &    surf%interior_surf, surf%x_surf, node, nod_comm,              &
     &    belongs%blng_surf, belongs%x_ref_surf, belongs%host_surf,     &
     &    surf_comm, fail_tbl)
      call dealloc_iele_belonged(belongs%host_surf)
      call dealloc_x_ref_surf(belongs)
      call dealloc_iele_belonged(belongs%blng_surf)
!
      call const_global_surface_id(surf_comm, surf)
!
      end subroutine const_surf_comm_table_old
!
!-----------------------------------------------------------------------
!
      subroutine const_edge_comm_table_old                                  &
     &         (node, nod_comm, edge_comm, edge, fail_tbl)
!
      use set_ele_id_4_node_type
      use const_element_comm_table
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      type(communication_table), intent(inout) :: edge_comm
      type(edge_data), intent(inout) :: edge
      type(failed_table), intent(inout) :: fail_tbl
!
      type(belonged_table), save :: belongs
!
!
      if(iflag_debug.gt.0) write(*,*) ' set_edge_id_4_node in edge'
      call set_edge_id_4_node(node, edge, belongs%blng_edge)
      call alloc_x_ref_edge(node, belongs)
      call sort_inod_4_ele_by_position(ione, edge%numedge, edge%x_edge, &
     &    node, belongs%blng_edge, belongs%x_ref_edge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' belonged_edge_id_4_node in edge'
      call belonged_edge_id_4_node(node, edge, belongs%host_edge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' const_comm_table_by_connenct_old in edge'
      call const_comm_table_by_connenct_old                             &
     &    (txt_edge, edge%numedge, edge%nnod_4_edge, edge%ie_edge,      &
     &    edge%interior_edge, edge%x_edge, node, nod_comm,              &
     &    belongs%blng_edge, belongs%x_ref_edge, belongs%host_edge,     &
     &    edge_comm, fail_tbl)
!
      call dealloc_iele_belonged(belongs%host_edge)
      call dealloc_x_ref_edge(belongs)
      call dealloc_iele_belonged(belongs%blng_edge)
!
      call const_global_edge_id(edge_comm, edge)
!
      end subroutine const_edge_comm_table_old
!
!-----------------------------------------------------------------------
!
      end module const_element_comm_tables_old
