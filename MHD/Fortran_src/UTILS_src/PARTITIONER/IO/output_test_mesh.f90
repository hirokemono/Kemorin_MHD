!>@file   output_test_mesh.f90
!!@brief  module output_test_mesh
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!> @brief Construct mesh strucuture informations
!!
!!@verbatim
!!      subroutine output_test_mesh_informations                        &
!!     &         (id_rank, mesh, mesh_IO, ele_mesh_IO)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!@endverbatim
!
      module output_test_mesh
!
      use m_precision
!
      use m_machine_parameter
      use m_default_file_prefix
      use t_mesh_data
      use t_read_mesh_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine output_test_mesh_informations                          &
     &         (id_rank, mesh, mesh_IO, ele_mesh_IO)
!
      use copy_mesh_structures
      use mesh_file_IO
      use set_element_data_4_IO
      use set_surface_data_4_IO
      use set_edge_data_4_IO
      use element_file_IO
      use element_geometry_file_IO
      use const_surface_comm_table
!
      integer, intent(in) :: id_rank
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_geometry), intent(inout) :: mesh_IO
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
      type(communication_table) :: surf_comm1
      type(communication_table) :: edge_comm1
      type(global_surface_data) :: surf_gl1
      type(global_edge_data) :: edge_gl1
!
!  ---------------------------------------------
!     output node data
!      spherical and cylindrical coordinate
!  ---------------------------------------------
!
      mesh_IO%nod_comm%num_neib = 0
      call alloc_neighbouring_id(mesh_IO%nod_comm)
      call copy_node_sph_to_xx(mesh%node, mesh_IO%node)
      call write_node_position_sph(id_rank, def_sph_mesh_head, mesh_IO)
!
      mesh_IO%nod_comm%num_neib = 0
      call alloc_neighbouring_id(mesh_IO%nod_comm)
      call copy_node_cyl_to_xx(mesh%node, mesh_IO%node)
      call write_node_position_cyl(id_rank, def_cyl_mesh_head, mesh_IO)
!
!  -------------------------------
!     output element data
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'copy_ele_geometry_to_IO'
      call empty_comm_table(ele_mesh_IO%comm)
      call copy_ele_geometry_to_IO                                      &
     &   (mesh%ele, ele_mesh_IO%node, ele_mesh_IO%sfed)
      call output_element_xyz_file                                      &
     &   (id_rank, def_ele_mesh_head, ele_mesh_IO)
!
!  -------------------------------
!     output surface data
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'copy_surf_geometry_to_IO'
      call const_surf_comm_table                                        &
     &   (mesh%node, mesh%surf, mesh%nod_comm, surf_comm1, surf_gl1)
      call copy_comm_tbl_type(surf_comm1, ele_mesh_IO%comm)
      call copy_surf_connect_to_IO(mesh%surf, surf_gl1,                 &
     &    mesh%ele%numele, ele_mesh_IO%ele, ele_mesh_IO%sfed)
      call copy_surf_geometry_to_IO                                     &
     &   (mesh%surf, surf_gl1, ele_mesh_IO%node, ele_mesh_IO%sfed)
      if (iflag_debug.gt.0) write(*,*) 'output_surface_sph_file'
      call output_surface_xyz_file                                      &
     &   (id_rank, def_surf_mesh_head, ele_mesh_IO)
      call dealloc_surf_comm_table(surf_comm1, surf_gl1)
!
!  -------------------------------
!     output edge data
!  -------------------------------
!
      call s_const_edge_comm_table                                      &
     &   (mesh%node, mesh%nod_comm, mesh%edge, edge_comm1, edge_gl1)
      call copy_comm_tbl_type(edge_comm1, ele_mesh_IO%comm)
      call empty_comm_table(ele_mesh_IO%comm)
      call copy_edge_connect_to_IO                                      &
     &   (mesh%edge, edge_gl1, mesh%ele%numele, mesh%surf%numsurf,      &
     &    ele_mesh_IO%ele, ele_mesh_IO%sfed)
      call copy_edge_geometry_to_IO(mesh%edge, edge_gl1,                &
     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      if (iflag_debug.gt.0) write(*,*) 'output_edge_sph_file'
      call output_edge_xyz_file                                         &
     &   (id_rank, def_edge_mesh_head, ele_mesh_IO)
      call dealloc_edge_comm_table(edge_comm1, edge_gl1)
!
       end subroutine output_test_mesh_informations
!
! ----------------------------------------------------------------------
!
      end module output_test_mesh
