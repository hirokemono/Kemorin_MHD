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
!!      subroutine read_control_4_sph_SGS_MHD(file_name, MHD_ctl)
!!      subroutine read_sph_mhd_control_data                            &
!!     &         (id_control, hd_block, MHD_ctl, c_buf)
!!      subroutine dealloc_sph_sgs_mhd_ctl_data(MHD_ctl)
!!      subroutine bcast_sph_mhd_control_data(MHD_ctl)
!!      subroutine bcast_sph_sgs_mhd_ctl_data(MHD_ctl)
!!@endverbatim
!
      module t_ctl_data_SGS_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_SGS_MHD_model
      use t_ctl_data_SPH_MHD_control
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
      use t_ctl_data_gen_sph_shell
      use t_control_data_vizs
      use t_control_data_dynamo_vizs
!
      implicit none
!
!
      integer(kind=kint), parameter :: ctl_file_code = 11
!
      type sph_sgs_mhd_control
!>        Structure for file settings
        type(platform_data_control) :: plt
!>        Control structure for orginal file informations
        type(platform_data_control) :: org_plt
!>        Control structure for new file informations
        type(platform_data_control) :: new_plt
!
!>        file name for parallel spherical shell control
        character(len = kchara) :: fname_psph_ctl
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
!>        Structures of zonal mean controls
        type(sph_dynamo_viz_controls) :: zm_ctls
!
        integer (kind=kint) :: i_mhd_ctl = 0
      end type sph_sgs_mhd_control
!
!   Top level of label
!
      character(len=kchara) :: hd_mhd_ctl = 'MHD_control'
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
      character(len=kchara), parameter                                  &
     &                    :: hd_viz_control = 'visual_control'
      character(len=kchara), parameter                                  &
     &                    :: hd_dynamo_viz_ctl = 'dynamo_vizs_control'
!
!>      Here is the old label
      character(len=kchara), parameter                                  &
     &                    :: hd_zm_viz_ctl = 'zonal_mean_control'
!
      private :: ctl_file_code, hd_mhd_ctl
      private :: hd_viz_control, hd_dynamo_viz_ctl, hd_zm_viz_ctl
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
      use t_ctl_data_SPH_MHD_control
      use viz_step_ctls_to_time_ctl
!
      character(len=kchara), intent(in) :: file_name
      type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(ctl_file_code, file = file_name, status='old' )
!
        do
          call load_one_line_from_control(ctl_file_code, c_buf1)
          call read_sph_mhd_control_data                                &
     &       (ctl_file_code, hd_mhd_ctl, MHD_ctl, c_buf1)
          if(MHD_ctl%i_mhd_ctl .gt. 0) exit
        end do
        close(ctl_file_code)
!
        call s_viz_step_ctls_to_time_ctl                                &
     &     (MHD_ctl%viz_ctls, MHD_ctl%smctl_ctl%tctl)
        call add_fields_4_vizs_to_fld_ctl(MHD_ctl%viz_ctls,             &
     &      MHD_ctl%model_ctl%fld_ctl%field_ctl)
      end if
!
      call bcast_sph_mhd_control_data(MHD_ctl)
!
      end subroutine read_control_4_sph_SGS_MHD
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_sph_sgs_mhd_ctl_data(MHD_ctl)
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
      call dealloc_monitor_data_ctl(MHD_ctl%nmtr_ctl)
      call dealloc_parallel_shell_ctl(MHD_ctl%psph_ctl)
      call dealloc_sph_sgs_mhd_model(MHD_ctl%model_ctl)
      call dealloc_sph_monitoring_ctl(MHD_ctl%smonitor_ctl)
!
      MHD_ctl%i_mhd_ctl = 0
!
      end subroutine dealloc_sph_sgs_mhd_ctl_data
!
!   --------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_control_data                              &
     &         (id_control, hd_block, MHD_ctl, c_buf)
!
      use t_ctl_data_SPH_MHD_control
      use ctl_file_gen_sph_shell_IO
      use read_ctl_data_4_platforms
      use read_ctl_data_sph_monitor
      use read_viz_controls
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(MHD_ctl%i_mhd_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, MHD_ctl%plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_org_data, MHD_ctl%org_plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_new_data, MHD_ctl%new_plt, c_buf)
!
        call sel_read_ctl_gen_shell_grids(id_control, hd_sph_shell,     &
     &      MHD_ctl%fname_psph_ctl, MHD_ctl%psph_ctl, c_buf)
!
        call read_sph_sgs_mhd_model                                     &
     &     (id_control, hd_model, MHD_ctl%model_ctl, c_buf)
        call read_sph_mhd_control                                       &
     &     (id_control, hd_control, MHD_ctl%smctl_ctl, c_buf)
!
        call read_monitor_data_ctl                                      &
     &     (id_control, hd_monitor_data, MHD_ctl%nmtr_ctl, c_buf)
        call read_sph_monitoring_ctl                                    &
     &     (id_control, hd_pick_sph, MHD_ctl%smonitor_ctl, c_buf)
!
        call s_read_viz_controls(id_control, hd_viz_control,            &
     &                           MHD_ctl%viz_ctls, c_buf)
!
        call read_dynamo_viz_control                                    &
     &     (id_control, hd_dynamo_viz_ctl, MHD_ctl%zm_ctls, c_buf)
        call read_dynamo_viz_control                                    &
     &     (id_control, hd_zm_viz_ctl, MHD_ctl%zm_ctls, c_buf)
      end do
      MHD_ctl%i_mhd_ctl = 1
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
      call bcast_dynamo_viz_control(MHD_ctl%zm_ctls)
!
      end subroutine bcast_sph_mhd_control_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_sgs_mhd_ctl_data(MHD_ctl)
!
      use t_ctl_data_SPH_MHD_control
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
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
      call calypso_mpi_bcast_one_int(MHD_ctl%i_mhd_ctl, 0)
      call calypso_mpi_bcast_character                                  &
     &   (MHD_ctl%fname_psph_ctl, cast_long(kchara), 0)
!
      end subroutine bcast_sph_sgs_mhd_ctl_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_SGS_MHD
