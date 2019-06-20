!>@file   t_ctl_data_FEM_MHD.f90
!!@brief  module t_ctl_data_FEM_MHD
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
!!      subroutine read_control_4_fem_MHD                               &
!!     &         (file_name, FEM_MHD_ctl, viz_ctls)
!!        type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!@endverbatim
!
      module t_ctl_data_FEM_MHD
!
      use m_precision
!
      use m_machine_parameter
      use t_ctl_data_4_platforms
      use t_ctl_data_SGS_MHD_model
      use t_ctl_data_FEM_MHD_control
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
      type fem_mhd_control
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
        type(fem_mhd_control_control) :: fmctl_ctl
!
!>        Structure for spectr monitoring control
        type(sph_monitor_control) :: smonitor_ctl
!>        Structure for monitoring plave list
        type(node_monitor_control) :: nmtr_ctl
      end type fem_mhd_control
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
      private :: read_fem_mhd_control_data, bcast_fem_mhd_ctl_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_fem_MHD                                 &
     &         (file_name, FEM_MHD_ctl, viz_ctls)
!
      character(len=kchara), intent(in) :: file_name
      type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
!
!
      if(my_rank .eq. 0) then
        ctl_file_code = control_file_code
        open (ctl_file_code, file = file_name, status='old' )
!
        call load_ctl_label_and_line
        call read_fem_mhd_control_data(FEM_MHD_ctl, viz_ctls)
!
        close(ctl_file_code)
      end if
!
      call bcast_fem_mhd_ctl_data(FEM_MHD_ctl, viz_ctls)
!
      end subroutine read_control_4_fem_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_fem_mhd_control_data(FEM_MHD_ctl, viz_ctls)
!
      use calypso_mpi
      use t_ctl_data_FEM_MHD_control
!
      type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
!
!
      if(right_begin_flag(hd_mhd_ctl) .eq. 0) return
      if (i_mhd_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_mhd_ctl = find_control_end_flag(hd_mhd_ctl)
        if(i_mhd_ctl .gt. 0) exit
!
!
        call read_control_platforms                                     &
     &     (hd_platform, i_platform, FEM_MHD_ctl%plt)
        call read_control_platforms                                     &
     &     (hd_org_data, i_org_data, FEM_MHD_ctl%org_plt)
!
        call read_sph_sgs_mhd_model                                     &
     &     (hd_model, i_model, FEM_MHD_ctl%model_ctl)
        call read_fem_mhd_control                                       &
     &     (hd_control, i_control, FEM_MHD_ctl%fmctl_ctl)
!
        call read_monitor_data_ctl                                      &
     &     (hd_monitor_data, i_monitor_data, FEM_MHD_ctl%nmtr_ctl)
        call read_viz_controls                                          &
     &     (ctl_file_code, viz_ctls, c_buf1)
      end do
!
      end subroutine read_fem_mhd_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_fem_mhd_ctl_data(FEM_MHD_ctl, viz_ctls)
!
      use t_ctl_data_FEM_MHD_control
      use bcast_4_platform_ctl
      use bcast_4_field_ctl
      use bcast_4_sph_monitor_ctl
      use bcast_4_sphere_ctl
!
      type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
!
!
      call bcast_ctl_data_4_platform(FEM_MHD_ctl%plt)
      call bcast_ctl_data_4_platform(FEM_MHD_ctl%org_plt)
!
      call bcast_sph_sgs_mhd_model(FEM_MHD_ctl%model_ctl)
      call bcast_fem_mhd_control(FEM_MHD_ctl%fmctl_ctl)
!
      call bcast_monitor_data_ctl(FEM_MHD_ctl%nmtr_ctl)
!
      call bcast_viz_controls(viz_ctls)
!
      end subroutine bcast_fem_mhd_ctl_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_FEM_MHD
