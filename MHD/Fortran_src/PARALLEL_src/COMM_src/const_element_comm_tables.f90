!>@file   const_element_comm_tables.f90
!!@brief  module const_element_comm_tables
!!
!!@author H. Matsui
!!@date Programmed in June, 2015
!
!> @brief Belonged element list for each node
!!
!!@verbatim
!!      subroutine const_global_mesh_infos(mesh)
!!      subroutine const_element_comm_tbl_only(mesh, ele_comm)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(communication_table), intent(inout) :: ele_comm
!!      subroutine dealloc_ele_comm_tbls_gl_nele(mesh)
!!        type(mesh_geometry), intent(inout) ::    mesh
!!
!!      subroutine const_ele_comm_tbl(node, nod_comm, ele_comm, ele)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(inout) :: ele_comm
!!        type(element_data), intent(inout) :: ele
!!      subroutine const_surf_comm_table                                &
!!     &         (node, nod_comm, surf_comm, surf)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(inout) :: surf_comm
!!        type(surface_data), intent(inout) :: surf
!!      subroutine const_edge_comm_table                                &
!!     &         (node, nod_comm, edge_comm, edge)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(inout) :: edge_comm
!!        type(edge_data), intent(inout) :: edge
!!
!!      subroutine const_global_numnod_list(node)
!!@endverbatim
!
      module const_element_comm_tables
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
      private :: const_global_element_id, const_global_surface_id
      private :: const_global_edge_id
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_global_mesh_infos(mesh)
!
      use set_ele_id_4_node_type
      use const_global_element_ids
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      if(iflag_debug.gt.0) write(*,*)' const_global_numnod_list'
      call const_global_numnod_list(mesh%node)
!
      if(iflag_debug.gt.0) write(*,*) ' find_position_range'
      call find_position_range(mesh%node)
!
      end subroutine const_global_mesh_infos
!
!-----------------------------------------------------------------------
!
      subroutine const_element_comm_tbl_only(mesh, ele_comm)
!
      use set_ele_id_4_node_type
!
      type(mesh_geometry), intent(inout) :: mesh
      type(communication_table), intent(inout) :: ele_comm
!
!
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_tbl'
      call const_ele_comm_tbl(mesh%node, mesh%nod_comm,                 &
     &                        ele_comm, mesh%ele)
!
      end subroutine const_element_comm_tbl_only
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_ele_comm_tbls_gl_nele(mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call dealloc_numnod_stack(mesh%node)
      call dealloc_numele_stack(mesh%ele)
      call dealloc_numedge_stack(mesh%edge)
!
      end subroutine dealloc_ele_comm_tbls_gl_nele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_global_numnod_list(node)
!
      use const_global_element_ids
!
      type(node_data), intent(inout) :: node
!
!
      call alloc_numnod_stack(nprocs, node)
!
      call count_number_of_node_stack(node%numnod, node%istack_numnod)
      call count_number_of_node_stack                                   &
     &   (node%internal_node, node%istack_internod)
!
      end subroutine const_global_numnod_list
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_numele_list(ele)
!
      use const_global_element_ids
!
      type(element_data), intent(inout) :: ele
!
!
      call alloc_numele_stack(nprocs, ele)
!
      call count_number_of_node_stack(ele%numele, ele%istack_numele)
      call count_number_of_node_stack                                   &
     &   (ele%internal_ele, ele%istack_interele)
!
      end subroutine const_global_numele_list
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_element_id(ele_comm, ele)
!
      use const_global_element_ids
!
      type(communication_table), intent(in) :: ele_comm
      type(element_data), intent(inout) :: ele
!
!
      call const_global_numele_list(ele)
      call set_global_ele_id(txt_ele, ele%numele, ele%istack_interele,  &
     &   ele%interior_ele, ele_comm, ele%iele_global)
!
      end subroutine const_global_element_id
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_surface_id(sf_comm, surf)
!
      use const_global_element_ids
!
      type(communication_table), intent(in) :: sf_comm
      type(surface_data), intent(inout) :: surf
!
!
      call alloc_numsurf_stack(nprocs, surf)
!
      call count_number_of_node_stack                                   &
     &  (surf%numsurf, surf%istack_numsurf)
      call count_number_of_node_stack                                   &
     &  (surf%internal_surf, surf%istack_intersurf)
!
      call set_global_ele_id                                            &
     &   (txt_surf, surf%numsurf, surf%istack_intersurf,                &
     &    surf%interior_surf, sf_comm, surf%isurf_global)
!
      call dealloc_numsurf_stack(surf)
!
      end subroutine const_global_surface_id
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_edge_id(ed_comm, edge)
!
      use const_global_element_ids
!
      type(communication_table), intent(in) :: ed_comm
      type(edge_data), intent(inout) :: edge
!
!
      call alloc_numedge_stack(nprocs, edge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' count_number_of_node_stack in edge'
      call count_number_of_node_stack                                   &
     &  (edge%numedge, edge%istack_numedge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' count_number_of_node_stack in internal edge'
      call count_number_of_node_stack                                   &
     &  (edge%internal_edge, edge%istack_interedge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' set_global_ele_id in edge'
      call set_global_ele_id                                            &
     &   (txt_edge, edge%numedge, edge%istack_interedge,                &
     &    edge%interior_edge, ed_comm, edge%iedge_global)
!
      end subroutine const_global_edge_id
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_ele_comm_tbl(node, nod_comm, ele_comm, ele)
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
!
!
      call set_ele_id_4_node(node, ele, belongs%blng_ele)
      call alloc_x_ref_ele(node, belongs)
      call sort_inod_4_ele_by_position(ione, ele%numele, ele%x_ele,     &
     &    node, belongs%blng_ele, belongs%x_ref_ele)
!
      call belonged_ele_id_4_node(node, ele, belongs%host_ele)
      call const_comm_table_by_connenct                                 &
     &   (txt_ele, ele%numele, ele%nnod_4_ele, ele%ie,                  &
     &    ele%interior_ele, ele%x_ele, node, nod_comm,                  &
     &    belongs%blng_ele, belongs%x_ref_ele, belongs%host_ele,        &
     &    ele_comm)
      call dealloc_iele_belonged(belongs%host_ele)
      call dealloc_x_ref_ele(belongs)
      call dealloc_iele_belonged(belongs%blng_ele)
!
      call const_global_element_id(ele_comm, ele)
!
      end subroutine const_ele_comm_tbl
!
!-----------------------------------------------------------------------
!
      subroutine const_surf_comm_table                                  &
     &         (node, nod_comm, surf_comm, surf)
!
      use set_ele_id_4_node_type
      use const_element_comm_table
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(inout) :: surf_comm
      type(surface_data), intent(inout) :: surf
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
      call const_comm_table_by_connenct                                 &
     &   (txt_surf, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,       &
     &    surf%interior_surf, surf%x_surf, node, nod_comm,              &
     &    belongs%blng_surf, belongs%x_ref_surf, belongs%host_surf,     &
     &    surf_comm)
      call dealloc_iele_belonged(belongs%host_surf)
      call dealloc_x_ref_surf(belongs)
      call dealloc_iele_belonged(belongs%blng_surf)
!
      call const_global_surface_id(surf_comm, surf)
!
      end subroutine const_surf_comm_table
!
!-----------------------------------------------------------------------
!
      subroutine const_edge_comm_table                                  &
     &         (node, nod_comm, edge_comm, edge)
!
      use set_ele_id_4_node_type
      use const_element_comm_table
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      type(communication_table), intent(inout) :: edge_comm
      type(edge_data), intent(inout) :: edge
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
     &          ' const_comm_table_by_connenct in edge'
      call const_comm_table_by_connenct                                 &
     &    (txt_edge, edge%numedge, edge%nnod_4_edge, edge%ie_edge,      &
     &    edge%interior_edge, edge%x_edge, node, nod_comm,              &
     &    belongs%blng_edge, belongs%x_ref_edge, belongs%host_edge,     &
     &    edge_comm)
!
      call dealloc_iele_belonged(belongs%host_edge)
      call dealloc_x_ref_edge(belongs)
      call dealloc_iele_belonged(belongs%blng_edge)
!
      call const_global_edge_id(edge_comm, edge)
!
      end subroutine const_edge_comm_table
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine find_position_range(node)
!
      use t_geometry_data
      use calypso_mpi_real
      use transfer_to_long_integers
!
      type(node_data), intent(inout) :: node
!
!
!  Evaluate range in local domain
      call calypso_mpi_allreduce_real(node%xyz_max_lc, node%xyz_max_gl, &
     &                                cast_long(3), MPI_MAX)
      call calypso_mpi_allreduce_real(node%xyz_min_lc, node%xyz_min_gl, &
     &                                cast_long(3), MPI_MIN)
!
      if(iflag_debug .gt. 0) then
        write(*,*)  'x range: ', node%xyz_min_gl(1), node%xyz_max_gl(1)
        write(*,*)  'y range: ', node%xyz_min_gl(2), node%xyz_max_gl(2)
        write(*,*)  'z range: ', node%xyz_min_gl(3), node%xyz_max_gl(3)
      end if
!
      end subroutine find_position_range
!
! ----------------------------------------------------------------------
!
      end module const_element_comm_tables
