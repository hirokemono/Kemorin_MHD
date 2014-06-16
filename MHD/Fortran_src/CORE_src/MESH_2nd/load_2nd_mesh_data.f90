!
!      module load_2nd_mesh_data
!
!     Written by H. Matsui on July, 2007
!
!      subroutine input_2nd_mesh(my_rank, new_femmesh,                  &
!     &          new_surf_mesh, new_edge_mesh)
!      subroutine input_2nd_mesh_geometry(my_rank, newmesh,             &
!     &          new_surf_mesh, new_edge_mesh)
!
!      subroutine output_2nd_mesh(my_rank)
!
      module load_2nd_mesh_data
!
      use m_precision
      use m_machine_parameter
!
      use mesh_IO_select
!
      implicit none
!
      private :: set_mesh_type_to_IO
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine input_2nd_mesh(my_rank, new_femmesh,                   &
     &          new_surf_mesh, new_edge_mesh)
!
      use set_mesh_types
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_data), intent(inout) :: new_femmesh
      type(surface_geometry), intent(inout) :: new_surf_mesh
      type(edge_geometry), intent(inout) ::  new_edge_mesh
!
!       set second mesh informations
!
      call sel_read_mesh(my_rank)
      call set_mesh_data_types(new_femmesh)
      call set_nnod_surf_edge_for_type(new_surf_mesh, new_edge_mesh,    &
     &    new_femmesh%mesh%ele%nnod_4_ele)
!
      call allocate_overlaped_ele_type(new_femmesh%mesh%ele)
!
      end subroutine input_2nd_mesh
!
! -----------------------------------------------------------------------
!
      subroutine input_2nd_mesh_geometry(my_rank, newmesh,              &
     &          new_surf_mesh, new_edge_mesh)
!
      use t_mesh_data
      use m_read_boundary_data
      use set_mesh_types
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_geometry), intent(inout) :: newmesh
      type(surface_geometry), intent(inout) :: new_surf_mesh
      type(edge_geometry), intent(inout) ::  new_edge_mesh
!
!
      call sel_read_mesh(my_rank)
      call set_geometry_types_data(newmesh)
!
      call set_nnod_surf_edge_for_type(new_surf_mesh, new_edge_mesh,    &
     &    newmesh%ele%nnod_4_ele)
!
      call deallocate_boundary_arrays
!
      end subroutine input_2nd_mesh_geometry
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_2nd_mesh(my_rank, newmesh, newgroup)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_geometry), intent(inout) :: newmesh
      type(mesh_groups), intent(inout) :: newgroup
!
!       save mesh information
!
      call set_mesh_type_to_IO(my_rank, newmesh, newgroup)
      call sel_write_mesh_file(my_rank)
!
      end subroutine output_2nd_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_mesh_type_to_IO(my_rank, newmesh, newgroup)
!
      use set_comm_tbl_type_4_IO
      use set_element_types_4_IO
      use set_group_types_4_IO
      use set_node_types_4_IO
!
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_geometry), intent(inout) :: newmesh
      type(mesh_groups), intent(inout) :: newgroup
!
!
      call copy_comm_tbl_type_to_IO(my_rank, newmesh%nod_comm)
      call copy_node_type_to_IO(newmesh%node)
      call copy_ele_connect_type_to_IO(newmesh%ele)
!
      call set_node_grp_type_to_IO(newgroup%nod_grp)
      call set_ele_grp_type_to_IO(newgroup%ele_grp)
      call set_surface_grp_type_to_IO(newgroup%surf_grp)
!
      end subroutine set_mesh_type_to_IO
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_new_mesh(newmesh, newgroup)
!
      use t_mesh_data
!
      type(mesh_geometry), intent(inout) :: newmesh
      type(mesh_groups), intent(inout) :: newgroup
!
!
      call deallocate_grp_type_num(newgroup%nod_grp)
      call deallocate_grp_type_item(newgroup%nod_grp)
!
      call deallocate_grp_type_num(newgroup%ele_grp)
      call deallocate_grp_type_item(newgroup%ele_grp)
!
      call deallocate_grp_type_num(newgroup%nod_grp)
      call deallocate_grp_type_item(newgroup%nod_grp)
!
!
      call deallocate_ele_connect_type(newmesh%ele)
      call deallocate_node_geometry_type(newmesh%node)
      call deallocate_type_comm_tbl(newmesh%nod_comm)
!
      end subroutine deallocate_new_mesh
!
!   --------------------------------------------------------------------
!
      end module load_2nd_mesh_data
