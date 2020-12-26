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
      use t_calypso_comm_table
      use t_control_param_repartition
      use t_time_data
      use t_VIZ_only_step_parameter
!
      use m_elapsed_labels_SEND_RECV
      use m_elapsed_labels_4_REPART
      use m_work_time
!
      implicit none
!
      type(mesh_data), save :: fem_T
      type(mesh_data), save :: new_fem
      type(calypso_comm_table), save :: org_to_new_tbl
!
      type(vol_partion_prog_param), save ::  part_p1
!>        Structure for new time stepping
      type(time_step_param_w_viz), save :: t_VIZ1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_field_to_repart
!
      use t_ctl_file_volume_grouping
!
      use m_error_IDs
      use m_file_format_switch
!
      use calypso_mpi_logical
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use mesh_file_name_by_param
      use set_interpolate_file_name
      use repartiton_by_volume
!
!
!>     Stracture for Jacobians
!
      type(new_patition_test_control) :: part_tctl1
!
      integer(kind = kint) :: irank_read
      integer(kind = kint) :: ierr
      logical :: flag
!
!     --------------------- 
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_repartition
      call elpsed_label_field_send_recv
!
!     ----- read control data
!
      call read_control_new_partition(part_tctl1)
!
      call set_control_param_repartition(part_tctl1, part_p1)
      
      call set_fixed_t_step_params_w_viz                                &
     &   (part_tctl1%t_viz_ctl, t_VIZ1, ierr, e_message)
      call copy_delta_t(t_VIZ1%init_d, t_VIZ1%time_d)
!
!  --  read geometry
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(part_p1%mesh_file, nprocs, fem_T)
      call calypso_MPI_barrier
!
!  -------------------------------
!
      call init_send_recv(fem_T%mesh%nod_comm)
      if(iflag_debug .gt. 0) write(*,*) 'estimate node volume'
!
!  -------------------------------
!
      if(my_rank .eq. 0) then
        flag =  (check_exist_mesh(my_rank, part_p1%new_mesh_file))      &
     &    .and. (check_exist_interpolate_file(my_rank,                  &
     &                              part_p1%part_param%trans_tbl_file))
      end if
      call calypso_MPI_barrier
      call calypso_mpi_bcast_one_logical(flag, 0)
!
      if(flag) then
        if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+6)
        call load_repartitoned_file(part_p1, fem_T, new_fem,            &
     &                              org_to_new_tbl)
        if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+6)
      else
        write(e_message,*)                                              &
     &        'Construct repartitioned mesh and transfer table'
        if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+1)
        call s_repartiton_by_volume(part_p1, fem_T, new_fem,            &
     &                              org_to_new_tbl)
        if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+1)
      end if
!
      end subroutine initialize_field_to_repart
!
! ----------------------------------------------------------------------
!
      subroutine analyze_field_to_repart
!
      use t_IO_step_parameter
      use t_VIZ_step_parameter
      use t_ucd_data
!
      use parallel_ucd_IO_select
      use set_ucd_data
!
      use select_copy_from_recv
      use udt_to_new_partition
!
!
      type(time_data) :: t_IO
      type(ucd_data) ::  org_ucd, new_ucd
!
      integer(kind = kint) :: i_step, ist, ied
      integer(kind = kint) :: istep_ucd = 0
!
!
      call init_udt_to_new_partition(part_p1%new_ucd_file,              &
     &                               new_fem%mesh, new_ucd)
!
      ist = t_VIZ1%init_d%i_time_step
      ied = t_VIZ1%finish_d%i_end_step
      do i_step = ist, ied
        if(output_IO_flag(i_step,t_VIZ1%ucd_step) .eqv. .FALSE.) cycle
!        if(iflag_vizs_w_fix_step(i_step, t_VIZ1%viz_step)
!     &        .eqv. .FALSE.) cycle
!
        istep_ucd = IO_step_exc_zero_inc(i_step, t_VIZ1%ucd_step)
        call alloc_merged_ucd_nod_stack(nprocs, org_ucd)
        call sel_read_alloc_para_udt_file                               &
     &     (istep_ucd, part_p1%org_ucd_file, t_IO, org_ucd)
!
        if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+4)
        call udt_field_to_new_partition                                 &
     &     (iflag_import_item, istep_ucd, part_p1%new_ucd_file,         &
     &      t_IO, new_fem%mesh, org_to_new_tbl, org_ucd, new_ucd)
        if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+4)
!
        call deallocate_ucd_phys_data(org_ucd)
        call deallocate_ucd_phys_name(org_ucd)
      end do
      call finalize_udt_to_new_partition(new_ucd)
!
!      if(my_rank .eq. 0) then
!        write(*,*) 't_VIZ1%viz_step%PSF_t', t_VIZ1%viz_step%PSF_t
!        write(*,*) 't_VIZ1%viz_step%ISO_t', t_VIZ1%viz_step%ISO_t
!        write(*,*) 't_VIZ1%viz_step%PVR_t', t_VIZ1%viz_step%PVR_t
!        write(*,*) 't_VIZ1%viz_step%FLINE_t', t_VIZ1%viz_step%FLINE_t
!        write(*,*) 't_VIZ1%viz_step%LIC_t', t_VIZ1%viz_step%LIC_t
!      end if
!
      call output_elapsed_times
!
      if(iflag_debug.gt.0) write(*,*) 'exit analyze_field_to_repart'
!
      end subroutine analyze_field_to_repart
!
! ----------------------------------------------------------------------
!
      end module analyzer_field_to_repart
