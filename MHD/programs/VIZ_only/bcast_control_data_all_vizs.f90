!>@file   bcast_control_data_all_vizs.f90
!!        module bcast_control_data_all_vizs
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!
!> @brief Control input routine for all visualizations
!!
!!@verbatim
!!      subroutine load_control_file_vizs(vizs_ctl)
!!        type(control_data_vizs), intent(inout) :: vizs_ctl
!!@endverbatim
!
!
      module bcast_control_data_all_vizs
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
      use t_control_data_all_vizs
!
      implicit  none
!
      character(len = kchara), parameter :: fname_viz_ctl = "ctl_viz"
!
      private :: fname_viz_ctl
      private :: bcast_vizs_control_data
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine load_control_file_vizs(vizs_ctl)
!
      use skip_comment_f
      use viz_step_ctls_to_time_ctl
!
      type(control_data_vizs), intent(inout) :: vizs_ctl
!
!
      if(my_rank .eq. 0) then
        call read_control_file_vizs(fname_viz_ctl, vizs_ctl)
      end if
!
      call bcast_vizs_control_data(vizs_ctl)
!
      end subroutine load_control_file_vizs
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
      end module bcast_control_data_all_vizs
