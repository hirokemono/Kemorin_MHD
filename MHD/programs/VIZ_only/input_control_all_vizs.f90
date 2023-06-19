!>@file   input_control_all_vizs.f90
!!        module input_control_all_vizs
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!
!> @brief Control input routine for all visualizations
!!
!!@verbatim
!!      subroutine s_input_control_all_vizs                             &
!!     &         (ctl_file_name, vizs_ctl, FEM_viz, t_viz_param)
!!        character(len = kchara), intent(in) :: ctl_file_name
!!        type(control_data_vizs), intent(inout) :: vizs_ctl
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!
!!      subroutine bcast_vizs_control_data(vizs_ctl)
!!      subroutine set_control_params_4_viz                             &
!!     &         (vizs_ctl, FEM_viz, t_viz_param, ierr)
!!        type(control_data_vizs), intent(in) :: vizs_ctl
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!@endverbatim
!
!
      module input_control_all_vizs
!
      use m_precision
      use m_machine_parameter
      use t_control_data_all_vizs
      use t_FEM_mesh_field_4_viz
      use t_VIZ_only_step_parameter
!
      use calypso_mpi
!
      implicit  none
!
      private :: bcast_vizs_control_data, set_control_params_4_viz
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_input_control_all_vizs                               &
     &         (ctl_file_name, vizs_ctl, FEM_viz, t_viz_param)
!
      use skip_comment_f
      use viz_step_ctls_to_time_ctl
!
      character(len = kchara), intent(in) :: ctl_file_name
      type(control_data_vizs), intent(inout) :: vizs_ctl
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
!
      integer(kind = kint) :: ierr = 0
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      if(my_rank .eq. 0) then
        call read_control_file_vizs(ctl_file_name, vizs_ctl, c_buf1)
      end if
      call bcast_vizs_control_data(vizs_ctl)
!
      if(c_buf1%iend .gt. 0) then
        call calypso_MPI_abort(vizs_ctl%i_viz_only_file,                &
     &                             'control file is broken')
      end if
!
!       set control data
      call set_control_params_4_viz                                     &
     &   (vizs_ctl, FEM_viz, t_viz_param, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
      end subroutine s_input_control_all_vizs
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_vizs_control_data(vizs_ctl)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_time_step_ctl
      use bcast_control_data_vizs
      use bcast_control_arrays
!
      type(control_data_vizs), intent(inout) :: vizs_ctl
!
!
      call bcast_ctl_array_c3(vizs_ctl%viz_field_ctl)
      call bcast_ctl_data_4_platform(vizs_ctl%viz_plt)
      call bcast_ctl_data_4_time_step(vizs_ctl%t_viz_ctl)
!
      call bcast_viz_controls(vizs_ctl%viz_ctl_v)
!
      call calypso_mpi_bcast_one_int(vizs_ctl%i_viz_only_file, 0)
!
      end subroutine bcast_vizs_control_data
!
!   --------------------------------------------------------------------
!
      subroutine set_control_params_4_viz                               &
     &         (vizs_ctl, FEM_viz, t_viz_param, ierr)
!
      use t_control_data_all_vizs
      use t_VIZ_only_step_parameter
!
      use m_file_format_switch
      use m_default_file_prefix
      use set_control_platform_item
      use set_control_platform_data
      use parallel_ucd_IO_select
!
      type(control_data_vizs), intent(in) :: vizs_ctl
!
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
      integer(kind = kint), intent(inout) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank, vizs_ctl%viz_plt)
      call set_control_smp_def(my_rank, vizs_ctl%viz_plt)
      call set_control_parallel_mesh(vizs_ctl%viz_plt,                  &
     &                               FEM_viz%mesh_file_IO)
      call set_merged_ucd_file_define(vizs_ctl%viz_plt,                 &
     &                                FEM_viz%ucd_file_IO)
!
      call init_viz_field_list_control(vizs_ctl%viz_field_ctl,          &
     &                                 FEM_viz%viz_fld_list)
!
      call set_fixed_t_step_params_w_viz                                &
     &   (vizs_ctl%t_viz_ctl, t_viz_param, ierr, e_message)
      call copy_delta_t(t_viz_param%init_d, t_viz_param%time_d)
!
      end subroutine set_control_params_4_viz
!
! ----------------------------------------------------------------------
!
      end module input_control_all_vizs
