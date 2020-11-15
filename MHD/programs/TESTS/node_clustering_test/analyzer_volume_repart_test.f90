!>@file   analyzer_volume_repart_test.f90
!!@brief  module analyzer_volume_repart_test
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine initialize_volume_repartition
!!      subroutine analyze_volume_grouping
!!@endverbatim
!
      module analyzer_volume_repart_test
!
      use m_precision
!
      use m_constants
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
      type(mesh_data), save :: new_fem
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_volume_repartition
!
      use m_array_for_send_recv
      use m_default_file_prefix
      use t_ctl_data_volume_grouping
      use t_control_param_vol_grping
      use t_1d_repartitioning_work
      use t_repartition_by_volume
      use set_istack_4_domain_block
      use repart_in_xyz_by_volume
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_comm_table
      use t_read_mesh_data
      use t_shape_functions
      use t_jacobians
      use t_fem_gauss_int_coefs
      use t_next_node_ele_4_node
      use t_calypso_comm_table
      use t_repart_double_numberings
!
      use mpi_load_mesh_data
      use mesh_file_IO
      use copy_mesh_structures
      use append_group_data
!
      use calypso_mpi_int
      use calypso_mpi_real
      use const_jacobians_3d
      use const_element_comm_tables
      use parallel_FEM_mesh_init
      use output_test_mesh
      use set_table_4_RHS_assemble
      use nod_phys_send_recv
      use solver_SR_type
      use transfer_to_long_integers
!
      use set_parallel_file_name
      use int_volume_of_single_domain
      use set_nnod_4_ele_by_type
!
      use set_mesh_file_names
      use mesh_file_IO
!
      use calypso_SR_type
      use select_copy_from_recv
      use nod_phys_send_recv
!
      use const_repart_nod_and_comm
      use const_repart_ele_connect
      use redistribute_groups
!
!>     Stracture for Jacobians
!
      type(new_patition_test_control) :: part_tctl1
      type(mesh_test_files_param) ::  T_meshes
!
      type(communication_table) :: ele_comm
      type(next_nod_ele_table) :: next_tbl_T
!
      type(jacobians_type) :: jacobians_T
      type(shape_finctions_at_points) :: spfs_T
!
      type(group_data) :: part_grp
!
      type(calypso_comm_table) :: part_tbl, ext_tbl
      type(calypso_comm_table) :: ele_tbl
!
      type(double_numbering_data) :: new_ids_on_org
!
!      integer(kind = kint_gl), allocatable :: inod_gl_test(:)
!      integer(kind = kint) :: inod, iele, k1, inum, jnod
!
      character(len=kchara) :: file_name
!
!     --------------------- 
!
      call init_elapse_time_by_TOTAL
!      call elapsed_label_4_ele_comm_tbl
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
      call read_control_new_partition(part_tctl1)
!
      call s_set_ctl_params_4_test_mesh(part_tctl1, T_meshes)
!
!  --  read geometry
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
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbl_only'
      call const_element_comm_tbl_only(fem_T%mesh, ele_comm)
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
      call init_send_recv(fem_T%mesh%nod_comm)
      if(iflag_debug .gt. 0) write(*,*) 'estimate node volume'
!
!  -------------------------------
!
      if(iflag_debug .gt. 0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (fem_T%mesh, next_tbl_T%neib_ele, next_tbl_T%neib_nod)
!
!       Re-partitioning
      call s_repartition_by_volume(fem_T%mesh, T_meshes, part_grp)
!
!
      call alloc_double_numbering_data                                  &
     &   (fem_T%mesh%node%numnod, new_ids_on_org)
      call s_const_nod_and_comm_repart                                  &
     &   (fem_T%mesh, next_tbl_T%neib_nod, T_meshes, part_grp,          &
     &    new_ids_on_org, new_fem%mesh%nod_comm,                        &
     &    new_fem%mesh%node, part_tbl, ext_tbl)
!
!
      call s_const_repart_ele_connect(fem_T%mesh, ele_comm, part_tbl,   &
     &    new_ids_on_org, ele_tbl, new_fem%mesh)
      call dealloc_double_numbering_data(new_ids_on_org)
!
      call s_redistribute_groups(fem_T%mesh, fem_T%group, ele_comm,     &
     &    new_fem%mesh, part_tbl, ele_tbl, new_fem%group)
!
!      allocate(inod_gl_test(new_fem%mesh%node%numnod))
!      inod_gl_test(1:new_fem%mesh%node%numnod) = 0
!
!      call calypso_SR_type_int8(iflag_import_item, part_tbl,           &
!     &    fem_T%mesh%node%numnod, new_fem%mesh%node%internal_node,     &
!     &    fem_T%mesh%node%inod_global(1), inod_gl_test(1))
!      call SOLVER_SEND_RECV_int8_type                                  &
!     &  (new_fem%mesh%node%numnod, new_fem%mesh%nod_comm, inod_gl_test)
!
!      write(*,*) my_rank, 'Check node transfer'
!      do inod = new_fem%mesh%node%internal_node+1,                     &
!     &         new_fem%mesh%node%numnod
!        if(inod_gl_test(inod)                                          &
!     &       .ne. new_fem%mesh%node%inod_global(inod)) then
!          write(*,*) my_rank, 'Wrong transfer at', inod,    &
!     &        new_fem%mesh%node%inod_global(inod), inod_gl_test(inod)
!        end if
!      end do
!
!       Output appended mesh
      file_name = set_mesh_file_name                                    &
     &          (T_meshes%new_mesh_file_IO%file_prefix,                 &
     &           T_meshes%new_mesh_file_IO%iflag_format, my_rank)
      call write_mesh_file                                             &
     &   (my_rank, file_name, new_fem%mesh, new_fem%group)
!      call dealloc_node_geometry_base(new_fem%mesh%node)
!
      end subroutine initialize_volume_repartition
!
! ----------------------------------------------------------------------
!
      subroutine analyze_volume_repartition
!
!
!      call output_elapsed_times
      call calypso_MPI_barrier
!
      if(iflag_debug.gt.0) write(*,*) 'exit analyze_volume_repartition'
!
      end subroutine analyze_volume_repartition
!
! ----------------------------------------------------------------------
!
      end module analyzer_volume_repart_test
