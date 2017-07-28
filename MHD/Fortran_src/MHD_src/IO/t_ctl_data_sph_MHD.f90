!>@file   t_ctl_data_sph_MHD.f90
!!@brief  module t_ctl_data_sph_MHD
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!!@verbatim
!!      subroutine read_control_4_sph_MHD(file_name, MHD_ctl)
!!@endverbatim
!
      module t_ctl_data_sph_MHD
!
      use m_precision
!
      use t_ctl_data_SGS_MHD
      use m_machine_parameter
      use m_read_control_elements
      use calypso_mpi
      use skip_comment_f
!
      implicit none
!
      integer(kind=kint), parameter :: control_file_code = 11
!
!   Top level of label
!
      character(len=kchara) :: hd_mhd_ctl = 'MHD_control'
      integer (kind=kint) :: i_mhd_ctl = 0
!
!   2nd level for MHD
!
      private :: hd_mhd_ctl, i_mhd_ctl
      private :: read_sph_mhd_control_data, bcast_sph_mhd_control_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_MHD(file_name, MHD_ctl)
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
!
!
      if(my_rank .eq. 0) then
        ctl_file_code = control_file_code
        open ( ctl_file_code, file = file_name, status='old' )
!
        call load_ctl_label_and_line
        call read_sph_mhd_control_data(MHD_ctl)
!
        close(ctl_file_code)
      end if
!
      call bcast_sph_mhd_control_data(MHD_ctl)
!
      if(MHD_ctl%psph_ctl%ifile_sph_shell .gt. 0) then
       call read_ctl_file_shell_in_MHD(MHD_ctl%psph_ctl)
      end if
!
      end subroutine read_control_4_sph_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_control_data(MHD_ctl)
!
      use m_control_data_pvrs
!
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
!
!
      if(right_begin_flag(hd_mhd_ctl) .eq. 0) return
      if (i_mhd_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_mhd_ctl, i_mhd_ctl)
        if(i_mhd_ctl .gt. 0) exit
!
!
        call read_control_platforms                                     &
     &     (hd_platform, i_platform, MHD_ctl%plt)
        call read_control_platforms                                     &
     &     (hd_org_data, i_org_data, MHD_ctl%org_plt)
        call read_control_platforms                                     &
     &     (hd_new_data, i_new_data, MHD_ctl%new_plt)
!
        call read_parallel_shell_in_MHD_ctl                             &
     &     (hd_sph_shell, MHD_ctl%psph_ctl)
!
        call read_sph_sgs_mhd_model                                     &
     &     (hd_model, i_model, MHD_ctl%model_ctl)
        call read_sph_mhd_control                                       &
     &     (hd_control, i_control, MHD_ctl%ctl_ctl)
!
        call read_monitor_data_ctl                                      &
     &     (hd_monitor_data, i_monitor_data, MHD_ctl%nmtr_ctl)
        call read_sph_monitoring_ctl                                    &
     &     (hd_pick_sph, i_pick_sph, MHD_ctl%smonitor_ctl)
!
        call read_viz_control_data
      end do
!
      end subroutine read_sph_mhd_control_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_control_data(MHD_ctl)
!
      use m_control_data_pvrs
      use bcast_4_platform_ctl
!
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
!
!
      call bcast_sph_sgs_mhd_ctl_data(MHD_ctl)
      call bcast_ctl_data_4_platform(MHD_ctl%new_plt)
!
      call bcast_viz_control_data
!
      end subroutine bcast_sph_mhd_control_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_sph_MHD
