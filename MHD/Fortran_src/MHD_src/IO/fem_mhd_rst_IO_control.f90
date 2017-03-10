!>@file   fem_mhd_rst_IO_control.f90
!!@brief  module fem_mhd_rst_IO_control
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
!!      subroutine init_MHD_restart_output
!!      subroutine init_restart_4_snapshot(i_step, node, t_IO, rst_step)
!!
!!      subroutine output_MHD_restart_file_ctl                          &
!!     &         (i_step, SGS_par, node, nod_comm, iphys,               &
!!     &          wk_sgs, wk_diff, nod_fld, rst_step)
!!      subroutine elspased_MHD_restart_ctl(SGS_par, node, nod_comm,    &
!!     &          iphys, wk_sgs, wk_diff, nod_fld)
!!
!!      subroutine input_MHD_restart_file_ctl(rst_step, layer_tbl,      &
!!     &          node, ele, fluid, SGS_par, wk_sgs, wk_diff,           &
!!     &          sgs_coefs, diff_coefs, nod_fld, flex_p)
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
      module fem_mhd_rst_IO_control
!
      use m_precision
!
      use calypso_mpi
!
      use t_SGS_control_parameter
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_time_data_IO
      use t_field_data_IO
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_flex_delta_t_data
      use t_IO_step_parameter
!
      use m_t_step_parameter
!
      implicit  none
!
      private :: output_restart_files, input_restart_files
      private :: input_model_coef_file, output_model_coef_file
!
      type(time_params_IO), save, private :: fem_time_IO
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
     &       (fem_fst_IO%nnod_IO, fem_fst_IO%istack_numnod_IO)
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
      type(time_params_IO), intent(inout) :: t_IO
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
      subroutine output_MHD_restart_file_ctl                            &
     &         (i_step, SGS_par, node, nod_comm, iphys,                 &
     &          wk_sgs, wk_diff, nod_fld, rst_step)
!
      integer(kind = kint), intent(in) :: i_step
      type(SGS_paremeters), intent(in) :: SGS_par
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(phys_address), intent(in) :: iphys
      type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!
      type(phys_data), intent(inout) :: nod_fld
      type(IO_step_param), intent(inout) :: rst_step
!
!
      if (output_IO_flag(i_step,rst_step) .ne. 0) return
!
      rst_step%istep_file = i_step / rst_step%increment
      call output_restart_files                                         &
     &   (rst_step%istep_file, node, nod_comm, iphys, nod_fld)
      call output_model_coef_file(rst_step%istep_file,                  &
     &    SGS_par%i_step_sgs_coefs, SGS_par%model_p, SGS_par%commute_p, &
     &    wk_sgs, wk_diff)
!
      end subroutine output_MHD_restart_file_ctl
!
! -----------------------------------------------------------------------
!
      subroutine elspased_MHD_restart_ctl(SGS_par, node, nod_comm,      &
     &          iphys, wk_sgs, wk_diff, nod_fld)
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(phys_address), intent(in) :: iphys
!
      type(dynamic_model_data), intent(inout) :: wk_sgs, wk_diff
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint), parameter :: index_rst = -1
!
!
      call output_restart_files                                         &
     &   (index_rst, node, nod_comm, iphys, nod_fld)
      call output_model_coef_file(index_rst,                            &
     &    SGS_par%i_step_sgs_coefs, SGS_par%model_p, SGS_par%commute_p, &
     &    wk_sgs, wk_diff)
!
      end subroutine elspased_MHD_restart_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine input_MHD_restart_file_ctl(rst_step, layer_tbl,        &
     &          node, ele, fluid, SGS_par, wk_sgs, wk_diff,             &
     &          sgs_coefs, diff_coefs, nod_fld, flex_p)
!
      use t_geometry_data_MHD
      use t_SGS_model_coefs
!
      type(IO_step_param), intent(in) :: rst_step
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(dynamic_model_data), intent(inout) :: wk_sgs, wk_diff
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
      type(phys_data), intent(inout) :: nod_fld
      type(flexible_stepping_parameter), intent(inout) :: flex_p
!
      integer(kind = kint) :: istep_rst
!
!
      call set_step_4_restart(rst_step, i_step_init, istep_rst)
      call input_restart_files(istep_rst, node, nod_fld, flex_p)
      call input_model_coef_file(istep_rst,                             &
     &    SGS_par%model_p, SGS_par%commute_p, ele, fluid, layer_tbl,    &
     &    SGS_par%i_step_sgs_coefs, wk_sgs, wk_diff,                    &
     &    sgs_coefs, diff_coefs)
!
      end subroutine input_MHD_restart_file_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_restart_files                                   &
     &         (index_rst, node, nod_comm, iphys, nod_fld)
!
      use field_IO_select
      use copy_time_steps_4_restart
      use set_field_to_restart
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: index_rst
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
      call copy_time_steps_to_restart(fem_time_IO)
      call copy_field_data_to_restart(node, nod_fld, fem_fst_IO)
!
      call sel_write_step_FEM_field_file                                &
     &   (nprocs, my_rank, index_rst, fem_time_IO, fem_fst_IO)
!
      end subroutine output_restart_files
!
! -----------------------------------------------------------------------
!
      subroutine input_restart_files(istep_rst, node, nod_fld, flex_p)
!
      use m_file_format_switch
!
      use field_IO_select
      use set_field_to_restart
      use copy_time_steps_4_restart
      use cal_num_digits
!
      integer(kind = kint), intent(in) :: istep_rst
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
      type(flexible_stepping_parameter), intent(inout) :: flex_p
!
      integer(kind = kint) :: ierr
!
!
      call check_step_FEM_field_file                                    &
     &   (my_rank, istep_rst, fem_fst_IO, ierr)
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
        call copy_time_steps_from_restart(fem_time_IO)
        call cal_num_digit_real(dt, flex_p%dt_fact, flex_p%idt_digit)
      else
        call copy_init_time_from_restart(fem_time_IO)
      end if
!
      if(my_rank .eq. 0)  write(*,*) 'delta t ', dt,                    &
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
      type(time_params_IO), intent(inout) :: t_IO
      type(phys_data), intent(inout) :: nod_fld
      type(IO_step_param), intent(inout) :: rst_step
!
!
      if (output_IO_flag(i_step, rst_step) .ne. 0) return
      rst_step%istep_file = i_step / rst_step%increment
!
      call sel_read_step_FEM_field_file                                 &
     &    (nprocs, my_rank, rst_step%istep_file, t_IO, fem_fst_IO)
!
      call copy_field_data_from_restart(node, fem_fst_IO, nod_fld)
!
      end subroutine input_restart_4_snapshot
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_model_coef_file(index_rst, i_step_sgs_coefs,    &
     &          SGS_param, cmt_param, wk_sgs, wk_diff)
!
      use t_ele_info_4_dynamic
!
      use sgs_ini_model_coefs_IO
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: index_rst
      integer(kind = kint), intent(in) :: i_step_sgs_coefs
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!
      character(len=kchara) :: fn_tmp
!
!
      if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
!
      if(index_rst .lt. 0) then
        call add_elaps_postfix(rst_sgs_coef_head, fn_tmp)
      else
        call add_int_suffix(index_rst, rst_sgs_coef_head, fn_tmp)
      end if
      call add_dat_extension(fn_tmp, rst_sgs_coef_name)
!
      call output_ini_model_coefs                                       &
     &   (i_step_sgs_coefs, cmt_param, wk_sgs, wk_diff)
!
      end subroutine output_model_coef_file
!
! -----------------------------------------------------------------------
!
      subroutine input_model_coef_file                                  &
     &         (istep_rst, SGS_param, cmt_param, ele, fluid, layer_tbl, &
     &          i_step_sgs_coefs, wk_sgs, wk_diff,                      &
     &          sgs_coefs, diff_coefs)
!
      use t_geometry_data_MHD
      use t_SGS_model_coefs
      use t_ele_info_4_dynamic
!
      use set_parallel_file_name
      use sgs_ini_model_coefs_IO
!
      integer(kind = kint), intent(in) :: istep_rst
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
!
      integer(kind = kint), intent(inout) :: i_step_sgs_coefs
      type(dynamic_model_data), intent(inout) :: wk_sgs, wk_diff
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      character(len=kchara) :: fn_tmp
!
!
      if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
      if(iflag_rst_sgs_coef_code .eq. 0) return
!
      if (istep_rst .eq. -1) then
        call add_elaps_postfix(rst_sgs_coef_head, fn_tmp)
      else
        call add_int_suffix(istep_rst, rst_sgs_coef_head, fn_tmp)
      end if
!
      call add_dat_extension(fn_tmp, rst_sgs_coef_name)
      call input_ini_model_coefs(cmt_param, ele, fluid, layer_tbl,      &
     &    i_step_sgs_coefs, wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!
      end subroutine input_model_coef_file
!
! -----------------------------------------------------------------------
!
      end module fem_mhd_rst_IO_control
