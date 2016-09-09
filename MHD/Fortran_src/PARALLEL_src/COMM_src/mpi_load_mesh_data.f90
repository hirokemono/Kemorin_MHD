!>@file   mpi_load_mesh_data.f90
!!@brief  module mpi_load_mesh_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy FEM mesh data from IO structure
!!
!!@verbatim
!!      subroutine mpi_input_mesh(mesh, group, nnod_4_surf, nnod_4_edge)
!!      subroutine mpi_input_mesh_geometry(mesh)
!!      subroutine mpi_output_mesh(mesh, group)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!@endverbatim
!
      module mpi_load_mesh_data
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
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
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine mpi_input_mesh(mesh, group, nnod_4_surf, nnod_4_edge)
!
      use m_read_boundary_data
      use mesh_MPI_IO_select
      use set_nnod_4_ele_by_type
      use load_mesh_data
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      integer(kind = kint), intent(inout) :: nnod_4_surf, nnod_4_edge
!
!
      call sel_mpi_read_mesh
      call set_mesh(mesh, group, nnod_4_surf, nnod_4_edge)
!
      end subroutine mpi_input_mesh
!
! -----------------------------------------------------------------------
!
      subroutine mpi_input_mesh_geometry(mesh)
!
      use m_read_boundary_data
      use mesh_MPI_IO_select
      use load_mesh_data
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call sel_mpi_read_mesh
      call set_mesh_geometry_data(mesh)
!
      call deallocate_para_mesh_groups_IO
!
      end subroutine mpi_input_mesh_geometry
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_output_mesh(mesh, group)
!
      use m_read_boundary_data
      use mesh_MPI_IO_select
      use set_comm_table_4_IO
      use set_element_data_4_IO
      use set_node_data_4_IO
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
      call sel_mpi_write_mesh_file
!
      call deallocate_mesh_groups_IO
!
      end subroutine mpi_output_mesh
!
! -----------------------------------------------------------------------
!
      end module mpi_load_mesh_data
