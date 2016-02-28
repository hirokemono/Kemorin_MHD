!>@file   load_mesh_data.f90
!!@brief  module load_mesh_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy FEM mesh data from IO structure
!!
!!@verbatim
!!      subroutine input_mesh                                           &
!!     &         (my_rank, mesh, group, nnod_4_surf, nnod_4_edge)
!!      subroutine input_mesh_geometry(my_rank, mesh)
!!      subroutine output_mesh(my_rank, mesh, group)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!@endverbatim
!
      module load_mesh_data
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
!
      implicit none
!
      private :: set_mesh_geometry_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine input_mesh                                             &
     &         (my_rank, mesh, group, nnod_4_surf, nnod_4_edge)
!
      use mesh_IO_select
      use set_nnod_4_ele_by_type
      use set_group_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      integer(kind = kint), intent(inout) :: nnod_4_surf, nnod_4_edge
!
!
      call sel_read_mesh(my_rank)
!
      call set_mesh_geometry_data(mesh)
      call set_grp_data_from_IO                                        &
     &   (group%nod_grp, group%ele_grp, group%surf_grp)
!
      call set_3D_nnod_4_sfed_by_ele                                   &
     &   (mesh%ele%nnod_4_ele, nnod_4_surf, nnod_4_edge)
!
      end subroutine input_mesh
!
! -----------------------------------------------------------------------
!
      subroutine input_mesh_geometry(my_rank, mesh)
!
      use m_read_boundary_data
      use mesh_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call sel_read_mesh(my_rank)
      call set_mesh_geometry_data(mesh)
!
      call deallocate_boundary_arrays
!
      end subroutine input_mesh_geometry
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_mesh(my_rank, mesh, group)
!
      use mesh_IO_select
      use set_comm_table_4_IO
      use set_element_data_4_IO
      use set_node_data_4_IO
      use set_group_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
!
      call copy_comm_tbl_type_to_IO(my_rank, mesh%nod_comm)
      call copy_node_geometry_to_IO(mesh%node)
      call copy_ele_connect_to_IO(mesh%ele)
!
      call set_grp_data_to_IO                                           &
     &   (group%nod_grp, group%ele_grp, group%surf_grp)
!
!       save mesh information
      call sel_write_mesh_file(my_rank)
!
      call dealloc_mesh_infos(mesh%nod_comm, mesh%node, mesh%ele,       &
     &    group%nod_grp, group%ele_grp, group%surf_grp)
!
      end subroutine output_mesh
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_geometry_data(mesh)
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
      end subroutine set_mesh_geometry_data
!
!  ---------------------------------------------------------------------
!
      end module load_mesh_data
