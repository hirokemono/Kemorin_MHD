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
!!      subroutine set_ctl_restart_4_fem_mhd(plt)
!!        type(platform_data_control), intent(in) :: plt
!!
!!      subroutine init_MHD_restart_output(node, nod_fld)
!!      subroutine init_restart_4_snapshot(i_step, node, t_IO, rst_step)
!!
!!      subroutine output_restart_files                                 &
!!     &         (index_rst, time_d, node, nod_comm, iphys, nod_fld)
!!        type(time_data), intent(in) :: time_d
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!      subroutine input_restart_files                                  &
!!     &         (istep_rst, node, nod_fld, init_d, time_d, flex_p)
!!        type(node_data), intent(in) :: node
!!        type(time_data), intent(inout) :: init_d, time_d
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(flexible_stepping_parameter), intent(inout) :: flex_p
!!
!!      subroutine input_restart_4_snapshot                             &
!!     &         (i_step, node, nod_fld, t_IO, rst_step)
!!        type(IO_step_param), intent(in) :: rst_step
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
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
      use t_field_data_IO
      use t_flex_delta_t_data
      use t_IO_step_parameter
!
      implicit  none
!
      type(time_data), save, private :: fem_time_IO
      type(field_IO), save, private :: fem_fst_IO
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_restart_4_fem_mhd(plt)
!
      use t_ctl_data_4_platforms
      use set_control_platform_data
!
      type(platform_data_control), intent(in) :: plt
!
!
      call set_control_restart_file_def(plt, fem_fst_IO)
!
      end subroutine set_ctl_restart_4_fem_mhd
!
! -----------------------------------------------------------------------
!
      subroutine init_MHD_restart_output(node, nod_fld)
!
      use set_field_to_restart
      use const_global_element_ids
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
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
      subroutine init_restart_4_snapshot(i_step, node, t_IO, rst_step)
!
      use const_global_element_ids
      use field_IO_select
      use set_field_to_restart
!
      integer(kind = kint), intent(in) :: i_step
      type(node_data), intent(in) :: node
      type(time_data), intent(inout) :: t_IO
      type(IO_step_param), intent(inout) :: rst_step
!
!
      rst_step%istep_file = i_step / rst_step%increment
      call sel_read_alloc_FEM_fld_head                                  &
     &   (nprocs, my_rank, rst_step%istep_file, t_IO, fem_fst_IO)
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
      subroutine output_restart_files                                   &
     &         (index_rst, time_d, node, nod_comm, iphys, nod_fld)
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
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if(iphys%i_pre_mom .gt. 0) then
        call vector_send_recv(iphys%i_pre_mom, nod_comm, nod_fld)
      end if
      if(iphys%i_pre_uxb .gt. 0) then
        call vector_send_recv(iphys%i_pre_uxb, nod_comm, nod_fld)
      end if
      if(iphys%i_pre_heat .gt. 0) then
        call scalar_send_recv(iphys%i_pre_heat, nod_comm, nod_fld)
      end if
      if(iphys%i_pre_composit .gt. 0) then
        call scalar_send_recv(iphys%i_pre_composit, nod_comm, nod_fld)
      end if
!
      call copy_time_step_size_data(time_d, fem_time_IO)
      call copy_field_data_to_restart(node, nod_fld, fem_fst_IO)
!
      call sel_write_step_FEM_field_file                                &
     &   (nprocs, my_rank, index_rst, fem_time_IO, fem_fst_IO)
!
      end subroutine output_restart_files
!
! -----------------------------------------------------------------------
!
      subroutine input_restart_files                                    &
     &         (istep_rst, node, nod_fld, init_d, time_d, flex_p)
!
      use m_file_format_switch
!
      use field_IO_select
      use set_field_to_restart
      use cal_num_digits
!
      integer(kind = kint), intent(in) :: istep_rst
      type(node_data), intent(in) :: node
!
      type(time_data), intent(inout) :: init_d, time_d
      type(phys_data), intent(inout) :: nod_fld
      type(flexible_stepping_parameter), intent(inout) :: flex_p
!
      integer(kind = kint) :: ierr
!
!
      ierr = check_step_FEM_field_file(my_rank, istep_rst, fem_fst_IO)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr,'No restart file.')
!
      call sel_read_alloc_step_FEM_file                                 &
     &   (nprocs, my_rank, istep_rst, fem_time_IO, fem_fst_IO)
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
      subroutine input_restart_4_snapshot                               &
     &         (i_step, node, nod_fld, t_IO, rst_step)
!
      use set_field_to_restart
      use field_IO_select
!
      integer(kind = kint), intent(in) :: i_step
      type(node_data), intent(in) :: node
      type(time_data), intent(inout) :: t_IO
      type(phys_data), intent(inout) :: nod_fld
      type(IO_step_param), intent(inout) :: rst_step
!
!
      if (set_IO_step_flag(i_step, rst_step) .ne. 0) return
!
      call sel_read_step_FEM_field_file                                 &
     &    (nprocs, my_rank, rst_step%istep_file, t_IO, fem_fst_IO)
!
      call copy_field_data_from_restart(node, fem_fst_IO, nod_fld)
!
      end subroutine input_restart_4_snapshot
!
! -----------------------------------------------------------------------
!
      end module m_fem_mhd_restart
