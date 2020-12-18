!>@file   analyzer_repart_by_volume.f90
!!@brief  module analyzer_repart_by_volume
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine initialize_reapart_by_vol
!!      subroutine analyze_reapart_by_vol
!!@endverbatim
!
      module analyzer_repart_by_volume
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
!
      use t_calypso_comm_table
      use t_interpolate_table
      use t_control_param_repartition
!
      use m_work_time
!
      implicit none
!
      type(vol_partion_prog_param), save ::  part_p1
      type(mesh_data), save :: fem_T
      type(mesh_data), save :: new_fem
!
      type(calypso_comm_table), save :: org_to_new_tbl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_reapart_by_vol
!
      use m_array_for_send_recv
      use m_default_file_prefix
      use m_file_format_switch
      use t_ctl_file_volume_grouping
      use t_1d_repartitioning_work
      use t_repartition_by_volume
      use set_istack_4_domain_block
      use repart_in_xyz_by_volume
!
      use t_file_IO_parameter
      use t_read_mesh_data
      use t_shape_functions
      use t_jacobians
      use t_fem_gauss_int_coefs
      use t_next_node_ele_4_node
      use t_repart_double_numberings
!
      use mpi_load_mesh_data
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
      use mesh_MPI_IO_select
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
!
      type(communication_table) :: ele_comm
      type(next_nod_ele_table) :: next_tbl_T
!
      type(jacobians_type) :: jacobians_T
      type(shape_finctions_at_points) :: spfs_T
!
      type(interpolate_table) :: itp_tbl_IO
!
!     --------------------- 
!
      call init_elapse_time_by_TOTAL
!
!     ----- read control data
!
      call read_control_new_partition(part_tctl1)
!
      call set_control_param_repartition(part_tctl1, part_p1)
!
!  --  read geometry
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(part_p1%mesh_file, nprocs, fem_T)
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
      if (iflag_debug.gt.0) write(*,*) 'init_send_recv'
      call init_send_recv(fem_T%mesh%nod_comm)
!
!  -------------------------------
!
      if(iflag_debug .gt. 0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (fem_T%mesh, next_tbl_T%neib_ele, next_tbl_T%neib_nod)
!
      call s_mesh_repartition_by_volume                                 &
     &   (fem_T, ele_comm, next_tbl_T%neib_nod,                         &
     &    part_p1%part_param, new_fem, org_to_new_tbl)
!
      if(part_p1%new_mesh_file%iflag_format                             &
     &     .eq. id_no_file) then
        if(my_rank .eq. 0) write(*,*) 'Set repartition mesh output'
        return
      end if
!
!       Output new mesh file
      call sel_mpi_write_mesh_file(part_p1%new_mesh_file,               &
     &                             new_fem%mesh, new_fem%group)
      call calypso_MPI_barrier
!
!
      call copy_repart_tbl_to_itp_table                                 &
     &   (fem_T%mesh, next_tbl_T%neib_ele, org_to_new_tbl, itp_tbl_IO)
!
      if(part_p1%part_param%trans_tbl_file%iflag_format                 &
     &     .eq. id_no_file) then
        if(my_rank .eq. 0) write(*,*) 'Set data transfer file output'
        return
      end if
!
      call sel_mpi_write_interpolate_table(my_rank,                     &
     &    part_p1%part_param%trans_tbl_file, itp_tbl_IO)
      call calypso_MPI_barrier
!
!
      end subroutine initialize_reapart_by_vol
!
! ----------------------------------------------------------------------
!
      subroutine analyze_reapart_by_vol
!
      use m_file_format_switch
      use parallel_itp_tbl_IO_select
      use copy_repart_and_itp_table
      use check_data_for_repartition
!
      type(calypso_comm_table) :: part_tbl_2
      type(interpolate_table) :: itp_tbl_IO2
!
      integer(kind = kint) :: irank_read
      integer(kind = kint) :: i, ierr
!
!
      if(part_p1%part_param%trans_tbl_file%iflag_format                 &
     &     .eq. id_no_file) then
        if(my_rank .eq. 0) write(*,*) 'No file to check data transfer'
        return
      end if
!
!
      call sel_mpi_read_interpolate_table(my_rank, nprocs,              &
     &    part_p1%part_param%trans_tbl_file, itp_tbl_IO2, ierr)
!
      irank_read = my_rank
      call copy_itp_table_to_repart_tbl(irank_read,                     &
     &    fem_T%mesh, new_fem%mesh, itp_tbl_IO2, part_tbl_2)
      call calypso_MPI_barrier
!
      if(my_rank .eq. 0) write(*,*) 'check table reading...'
      call compare_calypso_comm_tbls(org_to_new_tbl, part_tbl_2)
      call calypso_MPI_barrier
      if(my_rank .eq. 0) write(*,*) 'check table reading end!'
!
      end subroutine analyze_reapart_by_vol
!
! ----------------------------------------------------------------------
!
      end module analyzer_repart_by_volume
