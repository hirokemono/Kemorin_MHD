!>@file   m_ctl_data_sph_MHD_psf.f90
!!@brief  module m_ctl_data_sph_MHD_psf
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
!!      subroutine read_control_4_sph_MHD_w_psf
!!      subroutine read_control_4_sph_snap_w_psf(snap_ctl_name)
!!@endverbatim
!
      module m_ctl_data_sph_MHD_psf
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
      character(len=kchara), parameter :: hd_model =   'model'
      character(len=kchara), parameter :: hd_control = 'control'
      character(len=kchara), parameter                                  &
     &      :: hd_monitor_data = 'monitor_data_ctl'
      character(len=kchara), parameter                                  &
     &                     :: hd_pick_sph = 'sph_monitor_ctl'
!
      integer(kind=kint) :: i_model =        0
      integer(kind=kint) :: i_control =      0
      integer(kind=kint) :: i_monitor_data = 0
      integer(kind=kint) :: i_pick_sph = 0
!
      private :: MHD_ctl_name
      private :: hd_mhd_ctl, i_mhd_ctl
      private :: hd_model, hd_control, i_model, i_control
      private :: hd_pick_sph, i_pick_sph
      private :: hd_monitor_data, i_monitor_data
!
      private :: read_sph_mhd_ctl_w_psf
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_MHD_w_psf
!
      use m_read_ctl_gen_sph_shell
!
!
      if(my_rank .eq. 0) then
        ctl_file_code = control_file_code
        open ( ctl_file_code, file = MHD_ctl_name, status='old' )
!
        call load_ctl_label_and_line
        call read_sph_mhd_ctl_w_psf
!
        close(ctl_file_code)
      end if
!
      call bcast_sph_mhd_ctl_w_psf
!
      if(ifile_sph_shell .gt. 0) call read_control_4_gen_shell_grids
!
      end subroutine read_control_4_sph_MHD_w_psf
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_snap_w_psf(snap_ctl_name)
!
      use m_read_ctl_gen_sph_shell
!
      character(len=kchara), intent(in) :: snap_ctl_name
!
!
      if(my_rank .eq. 0) then
        ctl_file_code = control_file_code
        open ( ctl_file_code, file = snap_ctl_name, status='old' )
!
        call load_ctl_label_and_line
        call read_sph_mhd_ctl_w_psf
!
        close(ctl_file_code)
      end if
!
      call bcast_sph_mhd_ctl_w_psf
!
      if(ifile_sph_shell .gt. 0) call read_control_4_gen_shell_grids
!
      end subroutine read_control_4_sph_snap_w_psf
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_ctl_w_psf
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
      use m_read_ctl_gen_sph_shell
      use m_control_data_sections
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
        call read_ctl_data_4_org_data
!
        call read_ctl_data_4_shell_in_MHD
!
        call read_sph_mhd_model(hd_model, i_model, model_ctl1)
        call read_sph_mhd_control(hd_control, i_control, ctl_ctl1)
!
        call read_monitor_data_ctl                                      &
     &     (hd_monitor_data, i_monitor_data, nmtr_ctl1)
        call read_sph_monitoring_ctl                                    &
     &     (hd_pick_sph, i_pick_sph, smonitor_ctl1)
!
        call read_sections_control_data
      end do
!
      end subroutine read_sph_mhd_ctl_w_psf
!
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_ctl_w_psf
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
      use m_ctl_data_4_2nd_data
      use m_read_ctl_gen_sph_shell
      use m_control_data_sections
      use read_ctl_data_sph_MHD
      use bcast_4_platform_ctl
      use bcast_4_field_ctl
      use bcast_4_sph_monitor_ctl
      use bcast_4_sphere_ctl
!
!
      call bcast_ctl_data_4_platform(plt1)
      call bcast_ctl_data_4_platform(org_plt)
!
      call bcast_sph_mhd_model(model_ctl1)
      call bcast_sph_mhd_control(ctl_ctl1)
!
      call bcast_ctl_4_shell_define(spctl1)
      call bcast_ctl_ndomain_4_shell(sdctl1)
!
      call bcast_monitor_data_ctl(nmtr_ctl1)
      call bcast_sph_monitoring_ctl(smonitor_ctl1)
      call bcast_files_4_psf_ctl
      call bcast_files_4_iso_ctl
!
      end subroutine bcast_sph_mhd_ctl_w_psf
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_sph_MHD_psf
