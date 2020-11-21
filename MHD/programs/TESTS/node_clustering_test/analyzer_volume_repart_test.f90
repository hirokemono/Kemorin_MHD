!>@file   analyzer_volume_repart_test.f90
!!@brief  module analyzer_volume_repart_test
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
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
      call s_mesh_repartition_by_volume(fem_T, ele_comm,                &
     &    next_tbl_T%neib_nod, part_param, new_fem, org_to_new_tbl)
!
!       Output appended mesh
      file_name = set_mesh_file_name                                    &
     &          (part_param%new_mesh_file_IO%file_prefix,               &
     &           part_param%new_mesh_file_IO%iflag_format, my_rank)
      call write_mesh_file                                              &
     &   (my_rank, file_name, new_fem%mesh, new_fem%group)
      call calypso_MPI_barrier
!
!
      call copy_repart_tbl_to_itp_table                                 &
     &   (fem_T%mesh, next_tbl_T%neib_ele, org_to_new_tbl, itp_tbl_IO)
!
      call sel_mpi_write_interpolate_table                              &
     &    (my_rank, part_param%transfer_iable_IO, itp_tbl_IO)
      call calypso_MPI_barrier
!
!
      call sel_mpi_read_interpolate_table(my_rank, nprocs,              &
     &    part_param%transfer_iable_IO, itp_tbl_IO2, i)
!
      irank_read = my_rank
      call copy_itp_table_to_repart_tbl(irank_read,                     &
     &    fem_T%mesh, new_fem%mesh, itp_tbl_IO2, part_tbl_2)
      call calypso_MPI_barrier
!
!
      if(my_rank .eq. 0) write(*,*) 'check table reading...'
      if(part_tbl_2%iflag_self_copy.ne.org_to_new_tbl%iflag_self_copy)  &
     &     write(*,*) 'iflag_self_copy is wrong', my_rank
      if(part_tbl_2%nrank_import .ne. org_to_new_tbl%nrank_import)      &
     &     write(*,*) 'nrank_import is wrong', my_rank
      if(part_tbl_2%ntot_import .ne. org_to_new_tbl%ntot_import)        &
     &     write(*,*) 'ntot_import is wrong', my_rank
!
      do i = 1, part_tbl_2%nrank_import
        if(part_tbl_2%irank_import(i)                                   &
     &        .ne.org_to_new_tbl%irank_import(i))                       &
     &     write(*,*) 'irank_import is wrong', my_rank, i
      end do
      go to 101
      do i = 0, part_tbl_2%nrank_import
        if(part_tbl_2%istack_import(i)                                  &
     &        .ne.org_to_new_tbl%istack_import(i))                      &
     &     write(*,*) 'istack_import is wrong', my_rank, i
      end do
      do i = 1, part_tbl_2%ntot_import
        if(part_tbl_2%item_import(i)                                    &
     &        .ne. org_to_new_tbl%item_import(i))                       &
     &     write(*,*) 'item_import is wrong', my_rank, i
      end do
      do i = 1, part_tbl_2%ntot_import
        if(part_tbl_2%irev_import(i)                                    &
     &        .ne. org_to_new_tbl%irev_import(i))                       &
     &     write(*,*) 'irev_import is wrong', my_rank, i
      end do
!
      if(part_tbl_2%nrank_export .ne. org_to_new_tbl%nrank_export)      &
     &     write(*,*) 'nrank_export is wrong', my_rank
      if(part_tbl_2%ntot_export .ne. org_to_new_tbl%ntot_export)        &
     &     write(*,*) 'ntot_export is wrong', my_rank
      do i = 1, part_tbl_2%nrank_export
        if(part_tbl_2%irank_export(i)                                   &
     &        .ne. org_to_new_tbl%irank_export(i))                      &
     &     write(*,*) 'irank_export is wrong', my_rank
      end do
      do i = 0, part_tbl_2%nrank_export
        if(part_tbl_2%istack_export(i)                                  &
     &        .ne. org_to_new_tbl%istack_export(i))                     &
     &     write(*,*) 'istack_export is wrong', my_rank, i
      end do
!
      do i = 1, part_tbl_2%ntot_export
        if(part_tbl_2%item_export(i)                                    &
     &        .ne. org_to_new_tbl%item_export(i))                       &
     &     write(*,*) 'item_export is wrong', my_rank, i
      end do
!
 101  continue
      call calypso_MPI_barrier
      if(my_rank .eq. 0) write(*,*) 'check table reading end!'
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
