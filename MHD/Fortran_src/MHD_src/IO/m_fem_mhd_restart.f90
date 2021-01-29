!>@file   m_fem_mhd_restart.f90
!!@brief  module m_fem_mhd_restart
!!
!!@author H. Matsui
!!@date   programmed by H.Matsui and H.Okuda
!!@n                           on July 2000 (ver 1.1)
!!@n      modified by H. Matsui on Sep., 2006
!!@n      modified by H. Matsui on Dec., 2007
!
!> @brief Call restart data IO routines
!!
!!@verbatim
!!      subroutine init_MHD_restart_output(node, nod_fld, fem_fst_IO)
!!      subroutine init_restart_4_snapshot                              &
!!     &         (i_step, fst_file_IO, node, t_IO, rst_step, fem_fst_IO)
!!        type(node_data), intent(in) :: node
!!        type(field_IO_params), intent(in) :: fst_file_IO
!!        type(time_data), intent(inout) :: t_IO
!!        type(IO_step_param), intent(inout) :: rst_step
!!        type(field_IO), intent(inout) :: fem_fst_IO
!!
!!      subroutine output_restart_files(index_rst, fst_file_IO, time_d, &
!!     &          node, nod_comm, iphys, nod_fld, fem_fst_IO, v_sol)
!!        type(time_data), intent(in) :: time_d
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(phys_address), intent(in) :: iphys
!!        type(field_IO_params), intent(in) :: fst_file_IO
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!      subroutine input_restart_files(istep_rst, fst_file_IO,          &
!!     &          node, nod_fld, init_d, time_d, flex_p)
!!        type(node_data), intent(in) :: node
!!        type(field_IO_params), intent(in) :: fst_file_IO
!!        type(time_data), intent(inout) :: init_d, time_d
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(flexible_stepping_parameter), intent(inout) :: flex_p
!!
!!      subroutine input_restart_4_snapshot(i_step, fst_file_IO,        &
!!     &          node, nod_fld, t_IO, rst_step, fem_fst_IO)
!!        type(node_data), intent(in) :: node
!!        type(field_IO_params), intent(in) :: fst_file_IO
!!        type(time_data), intent(inout) :: t_IO
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(IO_step_param), intent(inout) :: rst_step
!!        type(field_IO), intent(inout) :: fem_fst_IO
!!@endverbatim
!
      module m_fem_mhd_restart
!
      use m_precision
!
      use calypso_mpi
!
      use t_time_data
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_file_IO_parameter
      use t_field_data_IO
      use t_flex_delta_t_parameter
      use t_IO_step_parameter
      use t_vector_for_solver
!
      implicit  none
!
      type(time_data), save, private :: fem_time_IO
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_MHD_restart_output(node, nod_fld, fem_fst_IO)
!
      use set_field_to_restart
      use const_global_element_ids
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
      type(field_IO), intent(inout) :: fem_fst_IO
!
!
      call count_field_num_to_restart(nod_fld, fem_fst_IO)
      call alloc_phys_name_IO(fem_fst_IO)
!
      call copy_field_name_to_restart(node, nod_fld, fem_fst_IO)
      call alloc_phys_data_IO(fem_fst_IO)
!
      call alloc_merged_field_stack(nprocs, fem_fst_IO)
      call count_number_of_node_stack                                   &
     &   (fem_fst_IO%nnod_IO, fem_fst_IO%istack_numnod_IO)
!
      end subroutine init_MHD_restart_output
!
! -----------------------------------------------------------------------
!
      subroutine init_restart_4_snapshot                                &
     &         (i_step, fst_file_IO, node, t_IO, rst_step, fem_fst_IO)
!
      use const_global_element_ids
      use field_IO_select
      use set_field_to_restart
!
      integer(kind = kint), intent(in) :: i_step
      type(node_data), intent(in) :: node
      type(field_IO_params), intent(in) :: fst_file_IO
!
      type(time_data), intent(inout) :: t_IO
      type(IO_step_param), intent(inout) :: rst_step
      type(field_IO), intent(inout) :: fem_fst_IO
!
      integer(kind = kint) :: istep_rst
!
!
      istep_rst = IO_step_exc_zero_inc(i_step, rst_step)
      call sel_read_alloc_FEM_fld_head(nprocs, my_rank,                 &
     &    istep_rst, fst_file_IO, t_IO, fem_fst_IO)
!
      fem_fst_IO%nnod_IO = node%numnod
      call alloc_phys_data_IO(fem_fst_IO)
!
      call alloc_merged_field_stack(nprocs, fem_fst_IO)
      call count_number_of_node_stack                                   &
     &       (fem_fst_IO%nnod_IO, fem_fst_IO%istack_numnod_IO)
!
      end subroutine init_restart_4_snapshot
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_restart_files(index_rst, fst_file_IO, time_d,   &
     &          node, nod_comm, iphys, nod_fld, fem_fst_IO, v_sol)
!
      use field_IO_select
      use set_field_to_restart
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: index_rst
      type(time_data), intent(in) :: time_d
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(phys_address), intent(in) :: iphys
      type(field_IO_params), intent(in) :: fst_file_IO
!
      type(phys_data), intent(inout) :: nod_fld
      type(field_IO), intent(inout) :: fem_fst_IO
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      if(iphys%exp_work%i_pre_mom .gt. 0) then
        call vector_send_recv                                           &
     &     (iphys%exp_work%i_pre_mom, nod_comm, nod_fld, v_sol)
      end if
      if(iphys%exp_work%i_pre_uxb .gt. 0) then
        call vector_send_recv                                           &
     &     (iphys%exp_work%i_pre_uxb, nod_comm, nod_fld, v_sol)
      end if
      if(iphys%exp_work%i_pre_heat .gt. 0) then
        call scalar_send_recv                                           &
     &     (iphys%exp_work%i_pre_heat, nod_comm, nod_fld, v_sol)
      end if
      if(iphys%exp_work%i_pre_composit .gt. 0) then
        call scalar_send_recv                                           &
     &     (iphys%exp_work%i_pre_composit, nod_comm, nod_fld, v_sol)
      end if
!
      call copy_time_step_size_data(time_d, fem_time_IO)
      call copy_field_data_to_restart(node, nod_fld, fem_fst_IO)
!
      call sel_write_step_FEM_field_file                                &
     &   (index_rst, fst_file_IO, fem_time_IO, fem_fst_IO)
!
      end subroutine output_restart_files
!
! -----------------------------------------------------------------------
!
      subroutine input_restart_files(istep_rst, fst_file_IO,            &
     &          node, nod_fld, init_d, time_d, flex_p)
!
      use m_file_format_switch
!
      use field_IO_select
      use set_field_to_restart
      use cal_num_digits
!
      integer(kind = kint), intent(in) :: istep_rst
      type(node_data), intent(in) :: node
      type(field_IO_params), intent(in) :: fst_file_IO
!
      type(time_data), intent(inout) :: init_d, time_d
      type(phys_data), intent(inout) :: nod_fld
      type(flexible_stepping_parameter), intent(inout) :: flex_p
!
      type(field_IO) :: fem_fst_IO
!
!
      if(check_step_FEM_field_file(my_rank, istep_rst, fst_file_IO))    &
     &      call calypso_MPI_abort(ierr_file,'No restart file.')
!
      call sel_read_alloc_step_FEM_file(nprocs, my_rank,                &
     &    istep_rst, fst_file_IO, fem_time_IO, fem_fst_IO)
!
      call copy_field_data_from_restart(node, fem_fst_IO, nod_fld)
      call dealloc_phys_data_IO(fem_fst_IO)
      call dealloc_phys_name_IO(fem_fst_IO)
!
      if(flex_p%iflag_flexible_step .eq. iflag_flex_step) then
        call copy_time_steps_from_restart(fem_time_IO, init_d)
        call copy_delta_t(init_d, time_d)
        call cal_num_digit_real                                         &
     &     (time_d%dt, flex_p%dt_fact, flex_p%idt_digit)
      else
        call copy_time_step_data(fem_time_IO, init_d)
      end if
!
      if(my_rank .eq. 0)  write(*,*) 'delta t ', time_d%dt,             &
     &                     flex_p%dt_fact, flex_p%idt_digit
!
      end subroutine input_restart_files
!
! -----------------------------------------------------------------------
!
      subroutine input_restart_4_snapshot(i_step, fst_file_IO,          &
     &          node, nod_fld, t_IO, rst_step, fem_fst_IO)
!
      use set_field_to_restart
      use field_IO_select
!
      integer(kind = kint), intent(in) :: i_step
      type(node_data), intent(in) :: node
      type(field_IO_params), intent(in) :: fst_file_IO
!
      type(time_data), intent(inout) :: t_IO
      type(phys_data), intent(inout) :: nod_fld
      type(IO_step_param), intent(inout) :: rst_step
      type(field_IO), intent(inout) :: fem_fst_IO
!
      integer(kind = kint) :: istep_rst
!
!
      if(output_IO_flag(i_step, rst_step) .eqv. .FALSE.) return
!
      istep_rst = set_IO_step(i_step, rst_step)
      call sel_read_step_FEM_field_file                                 &
     &   (nprocs, my_rank, istep_rst, fst_file_IO, t_IO, fem_fst_IO)
!
      call copy_field_data_from_restart(node, fem_fst_IO, nod_fld)
!
      end subroutine input_restart_4_snapshot
!
! -----------------------------------------------------------------------
!
      end module m_fem_mhd_restart
