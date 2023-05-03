!>@file   bcast_control_section_only.f90
!!@brief  module bcast_control_section_only
!!
!!@author H. Matsui
!!@date Programmed in March. 2023
!
!>@brief  Control data of node monitoring
!!
!!@verbatim
!!      subroutine load_control_file_section_only(sec_viz_ctl)
!!        character(len=kchara), intent(in) :: file_name
!!        type(control_data_section_only), intent(inout) :: sec_viz_ctl
!!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin visualizer
!!    begin data_files_def
!!      ...
!!    end data_files_def
!!
!!    begin time_step_ctl
!!      ...
!!    end time_step_ctl
!!
!!    begin visual_control
!!      ...
!!    end  visual_control
!!  end  visualizer
!!
!!    -------------------------------------------------------------------
!!@endverbatim
!
      module bcast_control_section_only
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_section_only
!
      use calypso_mpi
!
      implicit  none
!
      private :: bcast_section_control_data
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine load_control_file_section_only(sec_viz_ctl)
!
      type(control_data_section_only), intent(inout) :: sec_viz_ctl
!
!
      if(my_rank .eq. 0) then
        call read_control_file_section_only(fname_viz_ctl, sec_viz_ctl)
      end if
!
      call bcast_section_control_data(sec_viz_ctl)
!
      end subroutine load_control_file_section_only
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_section_control_data(sec_viz_ctl)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_time_step_ctl
      use bcast_ctl_data_surfacings
      use bcast_control_arrays
!
      type(control_data_section_only), intent(inout) :: sec_viz_ctl
!
!
      call bcast_ctl_array_c3(sec_viz_ctl%viz_field_ctl)
      call bcast_ctl_data_4_platform(sec_viz_ctl%sect_plt)
      call bcast_ctl_data_4_time_step(sec_viz_ctl%t_sect_ctl)
      call bcast_surfacing_controls(sec_viz_ctl%surfacing_ctls)
!
      call calypso_mpi_bcast_one_int(sec_viz_ctl%i_viz_only_file, 0)
!
      end subroutine bcast_section_control_data
!
!   --------------------------------------------------------------------
!
      end module bcast_control_section_only
