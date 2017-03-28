!
!      module analyzer
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine init_analyzer
!      subroutine analyze
!
!..................................................
!
      module analyzer
!
      use m_precision
!
      use m_machine_parameter
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit none
!
      type(mesh_geometry), save :: mesh
      type(mesh_groups), save :: group
      type(element_geometry), save :: ele_mesh
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use calypso_mpi
!
      use m_file_format_switch
      use m_default_file_prefix
!
      use copy_mesh_structures
      use set_element_data_4_IO
      use set_surface_data_4_IO
      use set_edge_data_4_IO
      use check_jacobians
      use int_volume_of_domain
      use set_surf_grp_vectors
      use check_surface_groups
      use set_normal_vectors
      use set_edge_vectors
      use const_mesh_information
      use mesh_file_IO
      use sum_normal_4_surf_group
      use set_parallel_file_name
      use sum_normal_4_surf_group
      use const_jacobians_3d
!
      use t_mesh_data
      use t_read_mesh_data
      use t_jacobians
!
!>     Stracture for Jacobians
      type(jacobians_type), save :: jacobians1
!
      type(mesh_geometry) :: mesh_IO
      type(surf_edge_IO_file) :: ele_mesh_IO
      character(len=kchara) :: file_prefix
!
!     --------------------- 
!
      if (my_rank.eq.0) then
        write(*,*) 'Construct commutation filter'
        write(*,*) 'Input file: mesh data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.gt.0) write(*,*) 's_input_control_test_MG'
      call s_input_control_test_MG                                      &
     &   (mesh, group, ele_mesh%surf, ele_mesh%edge)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank, mesh, group, ele_mesh)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'pick_surface_group_geometry'
      call pick_surface_group_geometry(ele_mesh%surf,                   &
     &   group%surf_grp, group%tbls_surf_grp, group%surf_grp_geom)
!
!  -------------------------------
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*) 'const_jacobian_volume_normals'
      call max_int_point_by_etype(mesh%ele%nnod_4_ele)
      call const_jacobian_volume_normals(my_rank, nprocs,               &
     &    mesh, ele_mesh%surf, group, jacobians1)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_normal_vector_spherical'
      call s_cal_normal_vector_spherical(ele_mesh%surf)
      if (iflag_debug.gt.0) write(*,*) 's_cal_normal_vector_cylindrical'
      call s_cal_normal_vector_cylindrical(ele_mesh%surf)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_edge_vector'
      call const_edge_vector(my_rank, nprocs,                           &
     &    mesh%node, ele_mesh%edge, jacobians1)
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_edge_vector_spherical'
      call s_cal_edge_vector_spherical(ele_mesh%edge)
      if (iflag_debug.gt.0) write(*,*) 's_cal_edge_vector_cylindrical'
      call s_cal_edge_vector_cylindrical(ele_mesh%edge)
!
!  ---------------------------------------------
!     output node data
!      spherical and cylindrical coordinate
!  ---------------------------------------------
!
      mesh_IO%nod_comm%num_neib = 0
      call allocate_type_neib_id(mesh_IO%nod_commm)
      call copy_node_sph_to_xx(mesh%node, mesh_IO%node)
      call write_node_position_sph(my_rank, def_sph_mesh_head, mesh_IO)
!
      mesh_IO%nod_commm%num_neib = 0
      call allocate_type_neib_id(mesh_IO%nod_commm)
      call copy_node_cyl_to_xx(mesh%node, mesh_IO%node)
      call write_node_position_cyl(my_rank, def_cyl_mesh_head, mesh_IO)
!
!  -------------------------------
!     output element data
!  -------------------------------
!
      file_prefix = def_ele_mesh_head
!
      if (iflag_debug.gt.0) write(*,*) 'copy_ele_geometry_to_IO'
      call copy_ele_geometry_to_IO(mesh%ele, nod_IO, sfed_IO)
      call output_element_file(my_rank, file_prefix, ele_mesh_IO)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_ele_sph_geom_to_IO'
      write(file_prefix,'(a,a4)') def_ele_mesh_head, '_sph'
      call copy_ele_sph_geom_to_IO(mesh%ele, nod_IO, sfed_IO)
      call output_element_sph_file(my_rank, file_prefix, ele_mesh_IO)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_ele_cyl_geom_to_IO'
      write(file_prefix,'(a,a4)') def_ele_mesh_head, '_cyl'
      call copy_ele_cyl_geom_to_IO(mesh%ele, nod_IO, sfed_IO)
      call output_element_cyl_file(my_rank, file_prefix, ele_mesh_IO)
!
!  -------------------------------
!     output surface data
!  -------------------------------
!
      file_prefix = def_surf_mesh_head
      if (iflag_debug.gt.0) write(*,*) 'copy_surf_geometry_to_IO'
      call copy_surf_connect_to_IO(ele_mesh%surf, mesh%ele%numele,      &
     &    ele_mesh_IO%ele, ele_mesh_IO%sfed)
      call copy_surf_geometry_to_IO                                     &
     &   (ele_mesh%surf, ele_mesh_IO%node, ele_mesh_IO%sfed)
!
      if (iflag_debug.gt.0) write(*,*) 'output_surface_file'
      call output_surface_file(my_rank, file_prefix, ele_mesh_IO)
!
      write(file_prefix,'(a,a4)') def_surf_mesh_head, '_sph'
      if (iflag_debug.gt.0) write(*,*) 'copy_surf_geometry_to_IO_sph'
      call copy_surf_connect_to_IO(ele_mesh%surf, mesh%ele%numele,      &
     &    ele_mesh_IO%ele, ele_mesh_IO%sfed)
      call copy_surf_geometry_to_IO_sph                                 &
     &   (ele_mesh%surf, ele_mesh_IO%node, ele_mesh_IO%sfed)
!
      if (iflag_debug.gt.0) write(*,*) 'output_surface_sph_file'
      call output_surface_sph_file                                      &
     &   (my_rank, file_prefix, ele_mesh_IO)
!
      write(file_prefix,'(a,a4)') def_surf_mesh_head, '_cyl'
      if (iflag_debug.gt.0) write(*,*) 'copy_surf_geometry_to_IO_cyl'
      call copy_surf_connect_to_IO(ele_mesh%surf, mesh%ele%numele,      &
     &    ele_mesh_IO%ele, ele_mesh_IO%sfed)
      call copy_surf_geometry_to_IO_cyl                                 &
     &   (ele_mesh%surf, ele_mesh_IO%node, ele_mesh_IO%sfed)
!
      if (iflag_debug.gt.0) write(*,*) 'output_surface_cyl_file'
      call output_surface_cyl_file                                      &
      &  (my_rank, file_prefix, ele_mesh_IO)
!
!  -------------------------------
!     output edge data
!  -------------------------------
!
      file_prefix = def_edge_mesh_head
      if (iflag_debug.gt.0) write(*,*) 'copy_edge_geometry_to_IO'
      call copy_edge_connect_to_IO                                      &
     &   (ele_mesh%edge, mesh%ele%numele, ele_mesh%surf%numsurf,        &
     &    ele_mesh_IO%ele, ele_mesh_IO%sfed)
      call copy_edge_geometry_to_IO(ele_mesh%edge,                      &
     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
!
      if (iflag_debug.gt.0) write(*,*) 'output_edge_geometries'
      call output_edge_geometries(my_rank, file_prefix, ele_mesh_IO)
!
      write(file_prefix,'(a,a4)') def_edge_mesh_head, '_sph'
      if (iflag_debug.gt.0) write(*,*) 'copy_edge_geometry_to_IO_sph'
      call copy_edge_connect_to_IO                                      &
     &   (ele_mesh%edge, mesh%ele%numele, ele_mesh%surf%numsurf,        &
     &    ele_mesh_IO%ele, ele_mesh_IO%sfed)
      call copy_edge_geometry_to_IO_sph(ele_mesh%edge,                  &
     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
!
      if (iflag_debug.gt.0) write(*,*) 'output_edge_geometries_sph'
      call output_edge_geometries_sph                                   &
     &   (my_rank, file_prefix, ele_mesh_IO)
!
      write(file_prefix,'(a,a4)') def_edge_mesh_head, '_cyl'
      if (iflag_debug.gt.0) write(*,*) 'copy_edge_geometry_to_IO_cyl'
      call copy_edge_connect_to_IO                                      &
     &   (ele_mesh%edge, mesh%ele%numele, ele_mesh%surf%numsurf,        &
     &    ele_mesh_IO%ele, ele_mesh_IO%sfed)
      call copy_edge_geometry_to_IO_cyl(ele_mesh%edge,                  &
     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      if (iflag_debug.gt.0) write(*,*) 'output_edge_geometries_cyl'
      call output_edge_geometries_cyl                                   &
     &   (my_rank, file_prefix, ele_mesh_IO)
!
       end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
        subroutine analyze
!
        use calypso_mpi
!
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
        end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer
