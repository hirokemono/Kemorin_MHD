!analyzer_repartition_test.f90
!
!      module analyzer_repartition_test
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine initialize_repartition_test
!      subroutine analyze_repartition_test
!
!..................................................
!
      module analyzer_repartition_test
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
      subroutine initialize_repartition_test
!
      use m_default_file_prefix
      use t_ctl_data_mesh_test
!
      use copy_mesh_structures
      use mesh_file_IO
      use nod_phys_send_recv
      use set_parallel_file_name
      use set_control_platform_data
!
      use mpi_load_mesh_data
      use const_jacobians_3d
      use parallel_FEM_mesh_init
      use output_test_mesh
      use set_table_4_RHS_assemble
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_read_mesh_data
      use t_shape_functions
      use t_jacobians
      use t_fem_gauss_int_coefs
      use t_const_comm_table
!
      use t_repartition_test
!
      use int_volume_of_single_domain
!
!>     Stracture for Jacobians
!
      type(mesh_test_control), save :: mesh_tctl1
      type(field_IO_params) :: mesh_file_name
      integer(kind = kint) :: iflag_output_SURF = 0
!
      type(next_nod_ele_table), save :: next_tbl_T
!
      type(jacobians_type), save :: jacobians_T
      type(shape_finctions_at_points), save :: spfs_T
!
      real(kind = kreal), allocatable :: node_volume(:)
!
      integer i, ist, ied, inod, inum
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
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_mesh_initialization'
      call init_nod_send_recv(fem_T%mesh)
      call FEM_mesh_initialization(fem_T%mesh, fem_T%group)
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_single_vol'
      call const_jacobian_and_single_vol                                &
     &   (fem_T%mesh, fem_T%group, spfs_T, jacobians_T)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (fem_T%mesh, next_tbl_T%neib_ele, next_tbl_T%neib_nod)
!
      call const_repartition_table                                      &
     &   (3, fem_T%mesh, next_tbl_T%neib_nod)
!
      end subroutine initialize_repartition_test
!
! ----------------------------------------------------------------------
!
      subroutine analyze_repartition_test
!
!
      call calypso_MPI_barrier
!
      if (iflag_debug.gt.0) write(*,*) 'exit analyze'
!
      end subroutine analyze_repartition_test
!
! ----------------------------------------------------------------------
!
      end module analyzer_repartition_test
