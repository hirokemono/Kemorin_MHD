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
      use m_fem_gauss_int_coefs
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
      use const_mesh_information
!
      use t_mesh_data
      use t_read_mesh_data
      use t_shape_functions
      use t_jacobians
!
!>     Stracture for Jacobians
      type(jacobians_type), save :: jacobians_T
      type(shape_finctions_at_points), save :: spfs_T
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
     &    mesh, ele_mesh%surf, group, spfs_T, jacobians_T)
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
     &    mesh%node, ele_mesh%edge, spfs_T%spf_1d, jacobians_T)
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_edge_vector_spherical'
      call s_cal_edge_vector_spherical(ele_mesh%edge)
      if (iflag_debug.gt.0) write(*,*) 's_cal_edge_vector_cylindrical'
      call s_cal_edge_vector_cylindrical(ele_mesh%edge)
!
!
      if (iflag_debug.gt.0) write(*,*) 'output_test_mesh_informations'
      call output_test_mesh_informations                                &
     &   (my_rank, mesh, ele_mesh, mesh_IO, ele_mesh_IO)
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
