!>@file   bcast_control_four_vizs.f90
!!@brief  module bcast_control_four_vizs
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!
!>@brief Control data for visualization without repartitioning
!!
!!@verbatim
!!      subroutine load_control_file_four_vizs(viz4_c)
!!        type(control_data_four_vizs), intent(inout) :: viz4_c
!!
!!      subroutine bcast_four_vizs_control_data(viz4_c)
!!        type(control_data_four_vizs), intent(inout) :: viz4_c
!!@endverbatim
!
      module bcast_control_four_vizs
!
      use m_precision
      use m_machine_parameter
      use t_control_data_four_vizs
      use calypso_mpi_int
!
      implicit  none
!
!
      integer(kind = kint), parameter :: viz_ctl_file_code = 11
      character(len = kchara), parameter :: fname_viz_ctl = "ctl_viz"
!
      private :: bcast_four_vizs_control_data
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine load_control_file_four_vizs(viz4_c)
!
      type(control_data_four_vizs), intent(inout) :: viz4_c
!
!
      if(my_rank .eq. 0) then
        call read_control_file_four_vizs(fname_viz_ctl, viz4_c)
      end if
!
      call bcast_four_vizs_control_data(viz4_c)
!
      end subroutine load_control_file_four_vizs
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_four_vizs_control_data(viz4_c)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_time_step_ctl
      use bcast_ctl_data_viz4
      use bcast_control_arrays
!
      type(control_data_four_vizs), intent(inout) :: viz4_c
!
!
      call bcast_ctl_array_c3(viz4_c%viz_field_ctl)
      call bcast_ctl_data_4_platform(viz4_c%viz_plt)
      call bcast_ctl_data_4_time_step(viz4_c%t_viz_ctl)
!
      call bcast_viz4_controls(viz4_c%viz4_ctl)
!
      call calypso_mpi_bcast_one_int(viz4_c%i_viz_only_file, 0)
!
      end subroutine bcast_four_vizs_control_data
!
!   --------------------------------------------------------------------
!
      end module bcast_control_four_vizs
