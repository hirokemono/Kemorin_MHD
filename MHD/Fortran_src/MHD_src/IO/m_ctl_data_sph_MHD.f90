!>@file   m_ctl_data_sph_MHD.f90
!!@brief  module m_ctl_data_sph_MHD
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
!!      subroutine read_control_4_sph_MHD
!!      subroutine read_control_4_sph_snap(snap_ctl_name)
!!@endverbatim
!
      module m_ctl_data_sph_MHD
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use calypso_mpi
      use skip_comment_f
!
      implicit none
!
      integer(kind=kint), parameter :: control_file_code = 11
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
!
!   Top level of label
!
      character(len=kchara) :: hd_mhd_ctl = 'MHD_control'
      integer (kind=kint) :: i_mhd_ctl = 0
!
!   2nd level for MHD
!
      character(len=kchara), parameter                                  &
     &                    :: hd_org_data = 'org_data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_new_data = 'new_data_files_def'
      character(len=kchara), parameter                                  &
     &      :: hd_sph_shell = 'spherical_shell_ctl'
      character(len=kchara), parameter :: hd_model =   'model'
      character(len=kchara), parameter :: hd_control = 'control'
      character(len=kchara), parameter                                  &
     &                     :: hd_pick_sph = 'sph_monitor_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_monitor_data = 'monitor_data_ctl'
!
      integer (kind=kint) :: i_org_data =     0
      integer (kind=kint) :: i_new_data =     0
      integer (kind=kint) :: i_model =        0
      integer (kind=kint) :: i_control =      0
      integer (kind=kint) :: i_pick_sph =     0
      integer (kind=kint) :: i_monitor_data = 0
!
      private :: MHD_ctl_name
      private :: hd_mhd_ctl, i_mhd_ctl
      private :: hd_org_data, i_org_data
      private :: hd_new_data, i_new_data
      private :: hd_sph_shell
      private :: hd_model, hd_control, i_model, i_control
      private :: hd_pick_sph, i_pick_sph
      private :: hd_monitor_data, i_monitor_data
!
      private :: read_sph_mhd_control_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_MHD
!
      use read_ctl_data_sph_MHD
!
!
      if(my_rank .eq. 0) then
        ctl_file_code = control_file_code
        open ( ctl_file_code, file = MHD_ctl_name, status='old' )
!
        call load_ctl_label_and_line
        call read_sph_mhd_control_data
!
        close(ctl_file_code)
      end if
!
      call bcast_sph_mhd_control_data
!
      if(psph_ctl1%ifile_sph_shell .gt. 0) then
       call read_ctl_file_shell_in_MHD(psph_ctl1)
      end if
!
      end subroutine read_control_4_sph_MHD
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_snap(snap_ctl_name)
!
      use read_ctl_data_sph_MHD
!
      character(len=kchara), intent(in) :: snap_ctl_name
!
!
      if(my_rank .eq. 0) then
        ctl_file_code = control_file_code
        open ( ctl_file_code, file = snap_ctl_name, status='old' )
!
        call load_ctl_label_and_line
        call read_sph_mhd_control_data
!
        close(ctl_file_code)
      end if
!
      call bcast_sph_mhd_control_data
!
      if(psph_ctl1%ifile_sph_shell .gt. 0) then
        call read_ctl_file_shell_in_MHD(psph_ctl1)
      end if
!
      end subroutine read_control_4_sph_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_control_data
!
      use m_ctl_data_4_platforms
      use m_control_data_pvrs
      use read_ctl_data_sph_MHD
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
        call read_ctl_data_4_platform
        call read_control_platforms(hd_org_data, i_org_data, org_plt1)
        call read_control_platforms(hd_new_data, i_new_data, new_plt1)
!
        call read_parallel_shell_in_MHD_ctl(hd_sph_shell, psph_ctl1)
!
        call read_sph_mhd_model(hd_model, i_model, model_ctl1)
        call read_sph_mhd_control(hd_control, i_control, ctl_ctl1)
!
        call read_monitor_data_ctl                                      &
     &     (hd_monitor_data, i_monitor_data, nmtr_ctl1)
        call read_sph_monitoring_ctl                                    &
     &     (hd_pick_sph, i_pick_sph, smonitor_ctl1)
!
        call read_viz_control_data
      end do
!
      end subroutine read_sph_mhd_control_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_control_data
!
      use m_ctl_data_4_platforms
      use m_control_data_pvrs
      use read_ctl_data_sph_MHD
      use bcast_4_platform_ctl
      use bcast_4_field_ctl
      use bcast_4_sph_monitor_ctl
      use bcast_4_sphere_ctl
!
!
      call bcast_ctl_data_4_platform(plt1)
      call bcast_ctl_data_4_platform(org_plt1)
      call bcast_ctl_data_4_platform(new_plt1)
!
      call bcast_sph_mhd_model(model_ctl1)
      call bcast_sph_mhd_control(ctl_ctl1)
!
      call bcast_parallel_shell_ctl(psph_ctl1)
!
      call bcast_monitor_data_ctl(nmtr_ctl1)
      call bcast_sph_monitoring_ctl(smonitor_ctl1)
      call bcast_viz_control_data
!
      end subroutine bcast_sph_mhd_control_data
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_sph_MHD
