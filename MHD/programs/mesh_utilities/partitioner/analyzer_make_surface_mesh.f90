!analyzer_make_surface_mesh.f90
!
!      module analyzer_make_surface_mesh
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine initialize_make_surface_mesh
!      subroutine analyze_make_surface_mesh
!
!..................................................
!
      module analyzer_make_surface_mesh
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_edge_data
      use m_work_time
!
      implicit none
!
      type(mesh_data), save :: fem_T
      type(element_geometry), save :: ele_mesh
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_make_surface_mesh
!
      use m_array_for_send_recv
      use m_default_file_prefix
      use t_ctl_data_mesh_test
      use t_control_param_mesh_test
!
      use copy_mesh_structures
      use mesh_file_IO
      use nod_phys_send_recv
      use sum_normal_4_surf_group
      use set_parallel_file_name
!
      use mpi_load_mesh_data
      use parallel_FEM_mesh_init
      use load_element_mesh_data
      use output_test_mesh
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_read_mesh_data
      use t_shape_functions
!
      type(mesh_test_control), save :: mesh_tctl1
      type(mesh_test_files_param) ::  T_meshes
      type(mesh_geometry) :: mesh_IO
      type(surf_edge_IO_file) :: ele_mesh_IO
!
!
      num_elapsed = 11
      call allocate_elapsed_times
!
      elapse_labels(1) = 'Total time                  '
      elapse_labels(2) = 'const_element_comm_tbls'
!
      elapse_labels(3) = 'const_ele_comm_tbl'
      elapse_labels(4) = '          '
      elapse_labels(5) = 'const_surf_comm_table'
      elapse_labels(6) = 'start_elapsed_time'
      elapse_labels(7) = 'const_edge_comm_table'
      elapse_labels(8) = '          '
!
      elapse_labels(9) = 'const_comm_table_by_connenct2'
      elapse_labels(10) = 's_set_element_export_item'
      elapse_labels(11) = 'search_target_element3'
!
!     --------------------- 
!
      if (my_rank.eq.0) then
        write(*,*) 'Test mesh commnucations'
        write(*,*) 'Input file: mesh data'
      end if
!
!     ----- read control data
!
      call start_elapsed_time(1)
      call read_control_4_mesh_test(mesh_tctl1)
!
      call set_ctl_params_4_test_mesh(mesh_tctl1, T_meshes)
!
!  --  read geometry
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh                                               &
     &   (T_meshes%mesh_file_IO, nprocs, fem_T, ele_mesh)
!
!  -------------------------------
!
      call start_elapsed_time(1)
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_mesh_init_with_IO'
      T_meshes%iflag_output_SURF = 1
      call FEM_mesh_init_with_IO(T_meshes%iflag_output_SURF,            &
     &    T_meshes%mesh_file_IO, fem_T%mesh, fem_T%group, ele_mesh)
      call end_elapsed_time(1)
!
      end subroutine initialize_make_surface_mesh
!
! ----------------------------------------------------------------------
!
      subroutine analyze_make_surface_mesh
!
!
      call output_elapsed_times
      call calypso_MPI_barrier
!
      if (iflag_debug.gt.0) write(*,*) 'exit analyze'
!
      end subroutine analyze_make_surface_mesh
!
! ----------------------------------------------------------------------
!
      end module analyzer_make_surface_mesh
