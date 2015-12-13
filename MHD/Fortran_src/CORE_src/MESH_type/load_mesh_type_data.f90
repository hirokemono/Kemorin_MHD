!
!      module load_mesh_type_data
!
!     Written by H. Matsui on July, 2007
!
!      subroutine input_mesh_data_type                                  &
!     &         (my_rank, femmesh, surf_mesh, edge_mesh)
!      subroutine input_mesh_geometry_type(my_rank, newmesh)
!
!      subroutine output_mesh_type(my_rank)
!
      module load_mesh_type_data
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine input_mesh_data_type                                   &
     &         (my_rank, femmesh, nnod_4_surf, nnod_4_edge)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: nnod_4_surf, nnod_4_edge
      type(mesh_data), intent(inout) :: femmesh
!
!
      call input_mesh(my_rank, femmesh%mesh%nod_comm,                   &
     &    femmesh%mesh%node, femmesh%mesh%ele, femmesh%group%nod_grp,   &
     &    femmesh%group%ele_grp, femmesh%group%surf_grp,                &
     &    nnod_4_surf, nnod_4_edge)
!
      end subroutine input_mesh_data_type
!
! -----------------------------------------------------------------------
!
      subroutine input_mesh(my_rank, nod_comm, node, ele,               &
     &          nod_grp, ele_grp, surf_grp, nnod_4_surf, nnod_4_edge)
!
      use t_comm_table
      use t_geometry_data
      use t_group_data
!
      use mesh_IO_select
      use set_nnod_4_ele_by_type
      use set_mesh_types
      use set_group_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
      integer(kind = kint), intent(inout) :: nnod_4_surf, nnod_4_edge
      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
!
      type(group_data), intent(inout) :: nod_grp
      type(group_data), intent(inout) :: ele_grp
      type(surface_group_data), intent(inout) :: surf_grp
!
!
!       set mesh informations
      call sel_read_mesh(my_rank)
!
      call set_mesh_geometry_data(nod_comm, node, ele)
      call set_grp_data_from_IO(nod_grp, ele_grp, surf_grp)
!
      call set_3D_nnod_4_sfed_by_ele                                   &
     &   (ele%nnod_4_ele, nnod_4_surf, nnod_4_edge)
!
      end subroutine input_mesh
!
! -----------------------------------------------------------------------
!
      subroutine input_mesh_geometry_type(my_rank, mesh)
!
      use t_mesh_data
      use m_read_boundary_data
      use set_mesh_types
      use mesh_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call sel_read_mesh(my_rank)
      call set_mesh_geometry_data(mesh%nod_comm, mesh%node, mesh%ele)
!
      call deallocate_boundary_arrays
!
      end subroutine input_mesh_geometry_type
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_mesh_type(my_rank, femmesh)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_data), intent(inout) :: femmesh
!
!
      call output_mesh(my_rank, femmesh%mesh%nod_comm,                  &
     &    femmesh%mesh%node, femmesh%mesh%ele, femmesh%group%nod_grp,   &
     &    femmesh%group%ele_grp, femmesh%group%surf_grp)
!
      end subroutine output_mesh_type
!
! -----------------------------------------------------------------------
!
      subroutine output_mesh(my_rank, nod_comm, node, ele,              &
     &                       nod_grp, ele_grp, surf_grp)
!
      use t_comm_table
      use t_geometry_data
      use t_group_data
!
      use set_mesh_types
      use mesh_IO_select
      use set_group_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
!
      type(group_data), intent(inout) :: nod_grp
      type(group_data), intent(inout) :: ele_grp
      type(surface_group_data), intent(inout) :: surf_grp
!
!
      call set_mesh_data_to_IO(my_rank, nod_comm, node, ele)
      call set_grp_data_to_IO(nod_grp, ele_grp, surf_grp)
!
!       save mesh information
      call sel_write_mesh_file(my_rank)
!
      call dealloc_mesh_infos                                     &
     &   (nod_comm, node, ele, nod_grp, ele_grp, surf_grp)
!
      end subroutine output_mesh
!
! -----------------------------------------------------------------------
!
      end module load_mesh_type_data
