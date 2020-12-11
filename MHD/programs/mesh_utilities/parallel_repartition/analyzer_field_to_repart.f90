!>@file   analyzer_field_to_repart.f90
!!@brief  module analyzer_field_to_repart
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine initialize_field_to_repart
!!      subroutine analyze_field_to_repart
!!@endverbatim
!
      module analyzer_field_to_repart
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
      subroutine initialize_field_to_repart
!
      use m_array_for_send_recv
      use m_default_file_prefix
      use m_file_format_switch
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
      use t_interpolate_table
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
      use mesh_repartition_by_volume
      use copy_repart_and_itp_table
      use parallel_itp_tbl_IO_select
!
!>     Stracture for Jacobians
!
      type(new_patition_test_control) :: part_tctl1
      type(volume_partioning_param) ::  part_param
!
      type(communication_table) :: ele_comm
      type(next_nod_ele_table) :: next_tbl_T
!
      type(jacobians_type) :: jacobians_T
      type(shape_finctions_at_points) :: spfs_T
!
      type(calypso_comm_table) :: org_to_new_tbl
      type(interpolate_table) :: itp_tbl_IO
!
      type(calypso_comm_table) :: part_tbl_2
      type(interpolate_table) :: itp_tbl_IO2
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: irank_read
      integer(kind = kint) :: ierr
      integer :: i
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
      call s_set_ctl_params_4_test_mesh(part_tctl1, part_param)
!
!  --  read geometry
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(part_param%mesh_file_IO, nprocs, fem_T)
!
!  -------------------------------
!
      call init_send_recv(fem_T%mesh%nod_comm)
      if(iflag_debug .gt. 0) write(*,*) 'estimate node volume'
!
!  -------------------------------
!
!       Output appended mesh
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(part_param%new_mesh_file_IO, nprocs, new_fem)
!
!
!
      call sel_mpi_read_interpolate_table(my_rank, nprocs,              &
     &    part_param%transfer_iable_IO, itp_tbl_IO2, ierr)
      call calypso_MPI_barrier
!
      irank_read = my_rank
      call copy_itp_table_to_repart_tbl(irank_read,                     &
     &    fem_T%mesh, new_fem%mesh, itp_tbl_IO2, part_tbl_2)
      call calypso_MPI_barrier
!
      end subroutine initialize_field_to_repart
!
! ----------------------------------------------------------------------
!
      subroutine analyze_field_to_repart
!
!
!      call output_elapsed_times
      call calypso_MPI_barrier
!
      if(iflag_debug.gt.0) write(*,*) 'exit analyze_field_to_repart'
!
      end subroutine analyze_field_to_repart
!
! ----------------------------------------------------------------------
!
      end module analyzer_field_to_repart
