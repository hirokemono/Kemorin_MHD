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
!
      use set_control_platform_data
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
      use const_element_comm_table
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_read_mesh_data
      use t_shape_functions
!
      integer(kind = kint) :: iflag_output_SURF = 0
      type(mesh_test_control) :: mesh_tctl1
      type(field_IO_params) :: mesh_file_name
!
!
      call init_elapse_time_by_TOTAL
      call elapsed_label_4_ele_comm_tbl
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
      call start_elapsed_time(ied_total_elapsed)
      call read_control_4_mesh_test(mesh_tctl1)
!
      call set_minimum_fem_platform_def                                 &
     &   (my_rank, mesh_tctl1%plt, mesh_tctl1%Fmesh_ctl,                &
     &    mesh_file_name, iflag_output_SURF)
!
!  --  read geometry
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(mesh_file_name, nprocs, fem_T)
!
!  -------------------------------
!
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_mesh_initialization'
      call FEM_comm_initialization(fem_T%mesh, vect1)
      call FEM_mesh_initialization(fem_T%mesh, fem_T%group)
      call end_elapsed_time(ied_total_elapsed)
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
