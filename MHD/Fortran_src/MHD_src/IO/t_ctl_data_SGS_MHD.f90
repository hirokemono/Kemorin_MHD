!>@file   t_ctl_data_SGS_MHD.f90
!!@brief  module t_ctl_data_SGS_MHD
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
!!      subroutine read_sph_mhd_control_data(MHD_ctl)
!!      subroutine bcast_sph_sgs_mhd_ctl_data(MHD_ctl)
!!@endverbatim
!
      module t_ctl_data_SGS_MHD
!
      use m_precision
!
      use m_machine_parameter
      use t_ctl_data_4_platforms
      use t_ctl_data_SGS_MHD_model
      use t_ctl_data_SPH_MHD_control
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
      use t_ctl_data_gen_sph_shell
      use t_control_data_vizs
!
      implicit none
!
!
      integer(kind=kint), parameter :: control_file_code = 11
!
      type sph_sgs_mhd_control
!>        Structure for file settings
        type(platform_data_control) :: plt
!>        Control structure for orginal file informations
        type(platform_data_control) :: org_plt
!>        Control structure for new file informations
        type(platform_data_control) :: new_plt
!
!>        Control structure for parallel spherical shell
        type(parallel_sph_shell_control) :: psph_ctl
!
!>        Control structure for MHD/model
        type(mhd_model_control) :: model_ctl
!>        Control structure for MHD/control
        type(sph_mhd_control_control) :: smctl_ctl
!
!>        Structure for spectr monitoring control
        type(sph_monitor_control) :: smonitor_ctl
!>        Structure for monitoring plave list
        type(node_monitor_control) :: nmtr_ctl
!
!>        Structures of visualization controls
        type(visualization_controls) :: viz_ctls
      end type sph_sgs_mhd_control
!
!   Top level of label
!
      character(len=kchara) :: hd_mhd_ctl = 'MHD_control'
      integer (kind=kint) :: i_mhd_ctl = 0
!
!   2nd level for MHD
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
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
      integer (kind=kint) :: i_platform =     0
      integer (kind=kint) :: i_org_data =     0
      integer (kind=kint) :: i_new_data =     0
      integer (kind=kint) :: i_model =        0
      integer (kind=kint) :: i_control =      0
      integer (kind=kint) :: i_pick_sph =     0
      integer (kind=kint) :: i_monitor_data = 0
!
      private :: hd_mhd_ctl, i_mhd_ctl
!
      private :: read_sph_mhd_control_data
      private :: bcast_sph_mhd_control_data
      private :: bcast_sph_sgs_mhd_ctl_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_SGS_MHD(file_name, MHD_ctl)
!
      character(len=kchara), intent(in) :: file_name
      type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
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
       call read_ctl_file_gen_shell_grids(MHD_ctl%psph_ctl)
      end if
!
      end subroutine read_control_4_sph_SGS_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_control_data(MHD_ctl)
!
      use t_ctl_data_SPH_MHD_control
!
      type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
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
     &     (hd_control, i_control, MHD_ctl%smctl_ctl)
!
        call read_monitor_data_ctl                                      &
     &     (hd_monitor_data, i_monitor_data, MHD_ctl%nmtr_ctl)
        call read_sph_monitoring_ctl                                    &
     &     (hd_pick_sph, i_pick_sph, MHD_ctl%smonitor_ctl)
!
        call read_viz_controls(MHD_ctl%viz_ctls)
      end do
!
      end subroutine read_sph_mhd_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_control_data(MHD_ctl)
!
      use bcast_4_platform_ctl
!
      type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
!
!
      call bcast_sph_sgs_mhd_ctl_data(MHD_ctl)
      call bcast_ctl_data_4_platform(MHD_ctl%new_plt)
!
      call bcast_viz_controls(MHD_ctl%viz_ctls)
!
      end subroutine bcast_sph_mhd_control_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_sgs_mhd_ctl_data(MHD_ctl)
!
      use t_ctl_data_SPH_MHD_control
      use bcast_4_platform_ctl
      use bcast_4_field_ctl
      use bcast_4_sph_monitor_ctl
      use bcast_4_sphere_ctl
!
      type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
!
!
      call bcast_ctl_data_4_platform(MHD_ctl%plt)
      call bcast_ctl_data_4_platform(MHD_ctl%org_plt)
!
      call bcast_sph_sgs_mhd_model(MHD_ctl%model_ctl)
      call bcast_sph_mhd_control(MHD_ctl%smctl_ctl)
!
      call bcast_parallel_shell_ctl(MHD_ctl%psph_ctl)
!
      call bcast_monitor_data_ctl(MHD_ctl%nmtr_ctl)
      call bcast_sph_monitoring_ctl(MHD_ctl%smonitor_ctl)
!
      end subroutine bcast_sph_sgs_mhd_ctl_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_SGS_MHD
