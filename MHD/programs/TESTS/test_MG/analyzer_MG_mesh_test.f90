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
      use t_mesh_SR
      use m_solver_SR
!
      implicit none
!
      type(mesh_data), save :: fem_MG
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
      call s_input_control_test_MG(fem_MG)
!
!     --------------------- 
!
      if (iflag_debug.gt.0) write(*,*) 'const_nod_ele_infos'
      call const_nod_ele_infos(my_rank, fem_MG%mesh, fem_MG%group)
      if (iflag_debug.eq.1) write(*,*) 'const_surface_infos'
      call const_surface_infos                                          &
     &   (my_rank, fem_MG%mesh%node, fem_MG%mesh%ele,                   &
     &    fem_MG%group%surf_grp, fem_MG%mesh%surf,                      &
     &    fem_MG%group%surf_nod_grp)
      if (iflag_debug.gt.0) write(*,*) 'const_para_edge_infos'
      call const_para_edge_infos                                        &
     &   (fem_MG%mesh%nod_comm, fem_MG%mesh%node, fem_MG%mesh%ele,      &
     &    fem_MG%mesh%surf, fem_MG%mesh%edge, SR_sig1, SR_i1)
!
!  -------------------------------
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*) 'jacobian_and_element_volume'
      call sel_max_int_point_by_etype                                   &
     &   (fem_MG%mesh%ele%nnod_4_ele, jacobians_T%g_FEM)
      call jacobian_and_element_volume(my_rank, nprocs,                 &
     &    fem_MG%mesh, fem_MG%group, spfs_T, jacobians_T)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &   'surf_jacobian_sf_grp_normal'
      call surf_jacobian_sf_grp_normal(my_rank, nprocs,                 &
     &    fem_MG%mesh, fem_MG%grou, spfs_T, jacobians_T)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_edge_vector'
      call const_edge_vector(my_rank, nprocs,                           &
     &    fem_MG%mesh%node, fem_MG%mesh%edge, spfs_T%spf_1d,            &
     &    jacobians_T)
!
      if (iflag_debug.gt.0) write(*,*) 'output_test_mesh_informations'
      call output_test_mesh_informations                                &
     &   (my_rank, fem_MG%mesh, mesh_IO, ele_mesh_IO)
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
