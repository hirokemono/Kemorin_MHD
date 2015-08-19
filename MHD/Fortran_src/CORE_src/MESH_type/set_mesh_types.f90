!set_mesh_types.f90
!      module set_mesh_types
!
!        programmed by H. Matsui on Dec., 2008
!
!      subroutine set_mesh_data_types(femmesh)
!      subroutine set_mesh_data_type_to_IO(my_rank, femmesh)
!       type(mesh_data), intent(mesh_groups) :: femmesh
!
!      subroutine set_geometry_types_data(mesh)
!      subroutine set_mesh_type_to_IO(my_rank, mesh)
!       type(mesh_geometry), intent(inout) :: mesh
!
!      subroutine set_ele_comm_tbl_type_data(ele_mesh)
!        type(element_comms) :: ele_mesh
!      subroutine set_surf_connect_type_data(surf_mesh, mesh)
!      subroutine set_surf_mesh_type_to_IO(my_rank, numele, surf_mesh)
!        type(mesh_geometry), intent(in) ::    mesh
!        type(surface_geometry), intent(inout) :: surf_mesh
!      subroutine set_edge_connect_type_data(edge_mesh, surf_mesh, mesh)
!      subroutine set_edge_mesh_type_to_IO                              &
!     &         (my_rank, numele, numsurf, edge_mesh)
!        type(mesh_geometry),    intent(in) :: mesh
!        type(edge_geometry), intent(inout) ::  edge_mesh
!
!      subroutine set_nnod_surf_edge_for_type(surf_mesh, edge_mesh,     &
!     &          mesh)
!        type(mesh_geometry),    intent(in) :: mesh
!        type(surface_geometry), intent(inout) :: surf_mesh
!        type(edge_geometry), intent(inout) ::  edge_mesh
!
      module set_mesh_types
!
      use m_precision
!
      use t_mesh_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_data_types(femmesh)
!
      use set_group_types_4_IO
!
      type(mesh_data), intent(inout) :: femmesh
!
!
      call set_geometry_types_data(femmesh%mesh)
      call set_grp_data_type_from_IO(femmesh%group)
!
      end subroutine set_mesh_data_types
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_data_type_to_IO(my_rank, femmesh)
!
      use set_group_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_data), intent(inout) :: femmesh
!
!
      call set_mesh_type_to_IO(my_rank, femmesh%mesh)
      call set_grp_data_type_to_IO(femmesh%group)
!
      end subroutine set_mesh_data_type_to_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_geometry_types_data(mesh)
!
      use set_comm_table_4_IO
      use set_node_data_4_IO
      use set_element_data_4_IO
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call copy_comm_tbl_type_from_IO(mesh%nod_comm)
!
      call copy_node_geometry_from_IO(mesh%node)
      call copy_ele_connect_from_IO(mesh%ele)
!
      call allocate_sph_node_geometry(mesh%node)
!
      end subroutine set_geometry_types_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_type_to_IO(my_rank, mesh)
!
      use t_mesh_data
      use set_comm_table_4_IO
      use set_element_data_4_IO
      use set_node_data_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call copy_comm_tbl_type_to_IO(my_rank, mesh%nod_comm)
      call copy_node_geometry_to_IO(mesh%node)
      call copy_ele_connect_to_IO(mesh%ele)
!
      end subroutine set_mesh_type_to_IO
!
!   --------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_comm_tbl_type_data(ele_mesh)
!
      use set_comm_table_4_IO
!
      type(element_comms) :: ele_mesh
!
      call copy_comm_tbl_type_from_IO(ele_mesh%ele_comm)
!
      end subroutine set_ele_comm_tbl_type_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_surf_connect_type_data(surf_mesh, mesh)
!
      use t_surface_data
      use set_comm_table_4_IO
      use set_surface_data_4_IO
!
!
      type(mesh_geometry), intent(in) ::    mesh
      type(surface_geometry), intent(inout) :: surf_mesh
!
!
      call copy_comm_tbl_type_from_IO(surf_mesh%surf_comm)
      call copy_surf_connect_from_IO(surf_mesh%surf, mesh%ele%numele)
!
      end subroutine set_surf_connect_type_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_edge_connect_type_data(edge_mesh, surf_mesh, mesh)
!
      use t_surface_data
      use t_edge_data
      use set_comm_table_4_IO
      use set_edge_data_4_IO
!
      type(mesh_geometry),    intent(in) :: mesh
      type(surface_geometry), intent(in) ::  surf_mesh
      type(edge_geometry), intent(inout) ::  edge_mesh
!
!
      call copy_comm_tbl_type_from_IO(edge_mesh%edge_comm)
      call copy_edge_connect_from_IO(edge_mesh%edge, mesh%ele%numele,   &
      &   surf_mesh%surf%numsurf)
!
      end subroutine set_edge_connect_type_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_surf_mesh_type_to_IO(my_rank, numele, surf_mesh)
!
      use t_mesh_data
      use set_comm_table_4_IO
      use set_surface_data_4_IO
!
      integer(kind = kint), intent(in) :: my_rank, numele
      type(surface_geometry), intent(in) :: surf_mesh
!
!
      call copy_comm_tbl_type_to_IO(my_rank, surf_mesh%surf_comm)
      call copy_surf_connect_to_IO(surf_mesh%surf, numele)
!
      end subroutine set_surf_mesh_type_to_IO
!
!   --------------------------------------------------------------------
!
      subroutine set_edge_mesh_type_to_IO                               &
     &         (my_rank, numele, numsurf, edge_mesh)
!
      use t_mesh_data
      use set_comm_table_4_IO
      use set_edge_data_4_IO
      use edge_IO_select
!
      integer(kind = kint), intent(in) :: my_rank, numele, numsurf
      type(edge_geometry), intent(in) :: edge_mesh
!
!
!
      call copy_comm_tbl_type_to_IO(my_rank, edge_mesh%edge_comm)
      call copy_edge_connect_to_IO(edge_mesh%edge, numele, numsurf)
!
      end subroutine set_edge_mesh_type_to_IO
!
!   --------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_nnod_surf_edge_for_type(surf_mesh, edge_mesh,      &
     &          mesh)
!
      use t_mesh_data
      use t_surface_data
      use t_edge_data
      use set_nnod_4_ele_by_type
!
!
      type(mesh_geometry),    intent(in) :: mesh
      type(surface_geometry), intent(inout) :: surf_mesh
      type(edge_geometry), intent(inout) ::  edge_mesh
!
!
      call set_3D_nnod_4_sfed_by_ele(mesh%ele%nnod_4_ele,               &
     &    surf_mesh%surf%nnod_4_surf, edge_mesh%edge%nnod_4_edge)
!
      end subroutine set_nnod_surf_edge_for_type
!
!  ---------------------------------------------------------------------
!
      end module set_mesh_types
