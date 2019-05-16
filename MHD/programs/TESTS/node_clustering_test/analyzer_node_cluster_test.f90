!analyzer_node_cluster_test.f90
!
!      module analyzer_node_cluster_test
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine initialize_node_cluster_test
!      subroutine analyze_node_cluster_test
!
!..................................................
!
      module analyzer_node_cluster_test
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
      subroutine initialize_node_cluster_test
!
      use m_array_for_send_recv
      use m_default_file_prefix
      use t_ctl_data_mesh_test
      use t_control_param_mesh_test
!
      use copy_mesh_structures
      use mesh_file_IO
      use nod_phys_send_recv
      use set_parallel_file_name
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
      use t_multi_node_clustering
!
      use const_element_comm_table
      use int_volume_of_single_domain
!
!>     Stracture for Jacobians
!
      type(mesh_test_control), save :: mesh_tctl1
      type(mesh_test_files_param) ::  T_meshes
!
      type(next_nod_ele_table), save :: next_tbl_T
      type(mul_node_clusterings) :: clusters_T
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
      call set_ctl_params_4_test_mesh(mesh_tctl1, T_meshes)
!
!  --  read geometry
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(T_meshes%mesh_file_IO, nprocs, fem_T)
!
!  -------------------------------
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(fem_T%mesh, fem_T%group)
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_single_vol'
      allocate(jacobians_T%g_FEM)
      call sel_max_int_point_by_etype                                   &
     &   (fem_T%mesh%ele%nnod_4_ele, jacobians_T%g_FEM)
      call const_jacobian_and_single_vol                                &
     &   (fem_T%mesh, fem_T%group, spfs_T, jacobians_T)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (fem_T%mesh, next_tbl_T%neib_ele, next_tbl_T%neib_nod)
      call const_multi_node_clusters                                    &
     &   (3, fem_T%mesh, next_tbl_T%neib_nod, clusters_T)
!
      write(*,*) 'num_gruped_nod', fem_T%mesh%node%internal_node,       &
     &          clusters_T%cluster(1)%num_gruped_nod,                   &
     & fem_T%mesh%node%internal_node / clusters_T%cluster(1)%num_gruped_nod  
      write(*,*) 'num_gruped_nod', fem_T%mesh%node%internal_node,       &
     &          clusters_T%cluster(2)%num_gruped_nod, &
     & fem_T%mesh%node%internal_node / clusters_T%cluster(2)%num_gruped_nod  
      write(*,*) 'num_gruped_nod', fem_T%mesh%node%internal_node,       &
     &          clusters_T%cluster(3)%num_gruped_nod,   &
     & fem_T%mesh%node%internal_node / clusters_T%cluster(3)%num_gruped_nod  
!
      do i = 1, clusters_T%cluster(3)%num_gruped_nod
        write(*,*) 'posisiton: ', i,   &
     &    clusters_T%cluster(3)%cluster_nod%xx(i,1:3), &
     &    clusters_T%cluster(3)%volume_grp(i), &
     &    clusters_T%cluster(3)%istack_grouped(i)-clusters_T%cluster(3)%istack_grouped(i-1)
      end do
!
!
      end subroutine initialize_node_cluster_test
!
! ----------------------------------------------------------------------
!
      subroutine analyze_node_cluster_test
!
!
!      call output_elapsed_times
      call calypso_MPI_barrier
!
      if (iflag_debug.gt.0) write(*,*) 'exit analyze'
!
      end subroutine analyze_node_cluster_test
!
! ----------------------------------------------------------------------
!
      end module analyzer_node_cluster_test
