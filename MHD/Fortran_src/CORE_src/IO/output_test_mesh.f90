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
!!     &         (my_rank, mesh, ele_mesh, mesh_IO, ele_mesh_IO)
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
     &         (my_rank, mesh, ele_mesh, mesh_IO, ele_mesh_IO)
!
      use copy_mesh_structures
      use mesh_file_IO
      use set_element_data_4_IO
      use set_surface_data_4_IO
      use set_edge_data_4_IO
      use element_file_IO
      use element_geometry_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_geometry), intent(inout) :: mesh_IO
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!  ---------------------------------------------
!     output node data
!      spherical and cylindrical coordinate
!  ---------------------------------------------
!
      mesh_IO%nod_comm%num_neib = 0
      call allocate_type_neib_id(mesh_IO%nod_comm)
      call copy_node_sph_to_xx(mesh%node, mesh_IO%node)
      call write_node_position_sph(my_rank, def_sph_mesh_head, mesh_IO)
!
      mesh_IO%nod_comm%num_neib = 0
      call allocate_type_neib_id(mesh_IO%nod_comm)
      call copy_node_cyl_to_xx(mesh%node, mesh_IO%node)
      call write_node_position_cyl(my_rank, def_cyl_mesh_head, mesh_IO)
!
!  -------------------------------
!     output element data
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'copy_ele_geometry_to_IO'
      call copy_comm_tbl_type(ele_mesh%ele_comm, ele_mesh_IO%comm)
      call copy_ele_geometry_to_IO                                      &
     &   (mesh%ele, ele_mesh_IO%node, ele_mesh_IO%sfed)
      call output_element_xyz_file                                      &
     &   (my_rank, def_ele_mesh_head, ele_mesh_IO)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_ele_sph_geom_to_IO'
      call copy_comm_tbl_type(ele_mesh%ele_comm, ele_mesh_IO%comm)
      call copy_ele_sph_geom_to_IO                                      &
     &   (mesh%ele, ele_mesh_IO%node, ele_mesh_IO%sfed)
      call output_element_sph_file                                      &
     &   (my_rank, def_ele_mesh_head, ele_mesh_IO)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_ele_cyl_geom_to_IO'
      call copy_comm_tbl_type(ele_mesh%ele_comm, ele_mesh_IO%comm)
      call copy_ele_cyl_geom_to_IO                                      &
     &    (mesh%ele, ele_mesh_IO%node, ele_mesh_IO%sfed)
      call output_element_cyl_file                                      &
     &   (my_rank, def_ele_mesh_head, ele_mesh_IO)
!
!  -------------------------------
!     output surface data
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'copy_surf_geometry_to_IO'
      call copy_comm_tbl_type(ele_mesh%surf_comm, ele_mesh_IO%comm)
      call copy_surf_connect_to_IO(ele_mesh%surf, mesh%ele%numele,      &
     &    ele_mesh_IO%ele, ele_mesh_IO%sfed)
      call copy_surf_geometry_to_IO                                     &
     &   (ele_mesh%surf, ele_mesh_IO%node, ele_mesh_IO%sfed)
      if (iflag_debug.gt.0) write(*,*) 'output_surface_sph_file'
      call output_surface_xyz_file                                      &
     &   (my_rank, def_surf_mesh_head, ele_mesh_IO)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_surf_geometry_to_IO_sph'
      call copy_comm_tbl_type(ele_mesh%surf_comm, ele_mesh_IO%comm)
      call copy_surf_connect_to_IO(ele_mesh%surf, mesh%ele%numele,      &
     &    ele_mesh_IO%ele, ele_mesh_IO%sfed)
      call copy_surf_geometry_to_IO_sph                                 &
     &   (ele_mesh%surf, ele_mesh_IO%node, ele_mesh_IO%sfed)
      if (iflag_debug.gt.0) write(*,*) 'output_surface_sph_file'
      call output_surface_sph_file                                      &
     &   (my_rank, def_surf_mesh_head, ele_mesh_IO)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_surf_geometry_to_IO_cyl'
      call copy_comm_tbl_type(ele_mesh%surf_comm, ele_mesh_IO%comm)
      call copy_surf_connect_to_IO(ele_mesh%surf, mesh%ele%numele,      &
     &    ele_mesh_IO%ele, ele_mesh_IO%sfed)
      call copy_surf_geometry_to_IO_cyl                                 &
     &   (ele_mesh%surf, ele_mesh_IO%node, ele_mesh_IO%sfed)
      if (iflag_debug.gt.0) write(*,*) 'output_surface_cyl_file'
      call output_surface_cyl_file                                      &
      &  (my_rank, def_surf_mesh_head, ele_mesh_IO)
!
!  -------------------------------
!     output edge data
!  -------------------------------
!
      call copy_comm_tbl_type(ele_mesh%edge_comm, ele_mesh_IO%comm)
      call copy_edge_connect_to_IO                                      &
     &   (ele_mesh%edge, mesh%ele%numele, ele_mesh%surf%numsurf,        &
     &    ele_mesh_IO%ele, ele_mesh_IO%sfed)
      call copy_edge_geometry_to_IO(ele_mesh%edge,                      &
     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      if (iflag_debug.gt.0) write(*,*) 'output_edge_sph_file'
      call output_edge_xyz_file                                         &
     &   (my_rank, def_edge_mesh_head, ele_mesh_IO)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_edge_geometry_to_IO_sph'
      call copy_comm_tbl_type(ele_mesh%edge_comm, ele_mesh_IO%comm)
      call copy_edge_connect_to_IO                                      &
     &   (ele_mesh%edge, mesh%ele%numele, ele_mesh%surf%numsurf,        &
     &    ele_mesh_IO%ele, ele_mesh_IO%sfed)
      call copy_edge_geometry_to_IO_sph(ele_mesh%edge,                  &
     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      if (iflag_debug.gt.0) write(*,*) 'output_edge_sph_file'
      call output_edge_sph_file                                         &
     &   (my_rank, def_edge_mesh_head, ele_mesh_IO)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_edge_geometry_to_IO_cyl'
      call copy_comm_tbl_type(ele_mesh%edge_comm, ele_mesh_IO%comm)
      call copy_edge_connect_to_IO                                      &
     &   (ele_mesh%edge, mesh%ele%numele, ele_mesh%surf%numsurf,        &
     &    ele_mesh_IO%ele, ele_mesh_IO%sfed)
      call copy_edge_geometry_to_IO_cyl(ele_mesh%edge,                  &
     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      if (iflag_debug.gt.0) write(*,*) 'output_edge_cyl_file'
      call output_edge_cyl_file                                         &
     &   (my_rank, def_edge_mesh_head, ele_mesh_IO)
!
       end subroutine output_test_mesh_informations
!
! ----------------------------------------------------------------------
!
      end module output_test_mesh
