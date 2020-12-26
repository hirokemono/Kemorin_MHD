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
      use m_work_time
      use calypso_mpi
!
      use t_mesh_data
      use t_calypso_comm_table
      use t_control_param_repartition
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
      use t_ctl_file_volume_grouping
!
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use repartiton_by_volume
!
!>     Stracture for Jacobians
!
      type(new_patition_test_control) :: part_tctl1
!
!     --------------------- 
!
      call init_elapse_time_by_TOTAL
!
!     ----- read control data
!
      call read_control_new_partition(part_tctl1)
      call set_control_param_repartition(part_tctl1, part_p1)
!
!  --  read geometry
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(part_p1%mesh_file, nprocs, fem_T)
!
!  -------------------------------
!
      call init_send_recv(fem_T%mesh%nod_comm)
      if(iflag_debug .gt. 0) write(*,*) 'estimate node volume'
!
!  -------------------------------
      call s_repartiton_by_volume(part_p1, fem_T, new_fem,              &
     &                            org_to_new_tbl)
!
      end subroutine initialize_reapart_by_vol
!
! ----------------------------------------------------------------------
!
      subroutine analyze_reapart_by_vol
!
      use t_interpolate_table
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
