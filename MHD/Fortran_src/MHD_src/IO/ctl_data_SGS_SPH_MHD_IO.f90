!>@file   ctl_data_SGS_SPH_MHD_IO.f90
!!@brief  module ctl_data_SGS_SPH_MHD_IO
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!@n        Modified by H. Matsui on Oct., 2023
!!
!!@verbatim
!!      subroutine init_sph_sgs_mhd_ctl_label(hd_block, MHD_ctl,        &
!!     &          sgs_ctl, tracer_ctls, viz_ctls, zm_ctls)
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_simulation_control), intent(inout) ::  MHD_ctl
!!        type(SGS_model_control), intent(inout) ::       sgs_ctl
!!        type(tracers_control), intent(inout) ::         tracer_ctls
!!        type(visualization_controls), intent(inout) ::  viz_ctls
!!        type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
!!      subroutine read_sph_mhd_control_data                            &
!!     &         (id_control, hd_block, MHD_ctl, sgs_ctl,               &
!!     &          tracer_ctls, viz_ctls, zm_ctls, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_simulation_control), intent(inout) ::  MHD_ctl
!!        type(SGS_model_control), intent(inout) ::       sgs_ctl
!!        type(tracers_control), intent(inout) ::         tracer_ctls
!!        type(visualization_controls), intent(inout) ::  viz_ctls
!!        type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_sph_mhd_control_data(id_control, MHD_ctl,      &
!!     &          sgs_ctl, tracer_ctls, viz_ctls, zm_ctls, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        type(mhd_simulation_control), intent(in) ::  MHD_ctl
!!        type(SGS_model_control), intent(in) ::       sgs_ctl
!!        type(visualization_controls), intent(in) ::  viz_ctls
!!        type(tracers_control), intent(in) ::         tracer_ctls
!!        type(sph_dynamo_viz_controls), intent(in) :: zm_ctls
!! ----------------------------------------------------------------------
!!
!!  begin MHD_control
!!    begin  data_files_def
!!      ...
!!    end    data_files_def
!!
!!    file   spherical_shell_ctl
!!    begin  spherical_shell_ctl
!!      ...
!!    end    spherical_shell_ctl
!!
!!    begin  model
!!      ...
!!    end    model
!!
!!    begin  control
!!      ...
!!    end    control
!!
!!    begin  sph_monitor_ctl
!!      ...
!!    end    sph_monitor_ctl
!!
!!    begin  visual_control
!!      ...
!!    end    visual_control
!!
!!    begin  dynamo_vizs_control
!!      ...
!!    end    dynamo_vizs_control
!!
!!    begin  tracer_control
!!      ...
!!    end    tracer_control
!!  end MHD_control
!!
!! ----------------------------------------------------------------------
!!@endverbatim
!
      module ctl_data_SGS_SPH_MHD_IO
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_MHD
      use t_ctl_data_4_platforms
      use t_ctl_data_MHD_model
      use t_ctl_data_SPH_MHD_control
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
      use t_ctl_data_gen_sph_shell
!
      use t_ctl_data_SGS_model
      use t_control_data_vizs
      use t_control_data_dynamo_vizs
      use t_control_data_tracers
!
      implicit none
!
!   2nd level for MHD
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_org_data = 'org_data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_new_data = 'new_data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_shell = 'spherical_shell_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_model =   'model'
      character(len=kchara), parameter, private                         &
     &                    :: hd_control = 'control'
      character(len=kchara), parameter, private                         &
     &                    :: hd_pick_sph = 'sph_monitor_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_monitor_data = 'monitor_data_ctl'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_ctl = 'visual_control'
      character(len=kchara), parameter, private                         &
     &                    :: hd_dynamo_viz_ctl = 'dynamo_vizs_control'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_tracer_ctl = 'tracers_control'
!
!>      Here is the old label
      character(len=kchara), parameter, private                         &
     &                    :: hd_zm_viz_ctl = 'zonal_mean_control'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_sph_sgs_mhd_ctl_label(hd_block, MHD_ctl,          &
     &          sgs_ctl, tracer_ctls, viz_ctls, zm_ctls)
!
      use t_ctl_data_SPH_MHD_control
      use ctl_file_gen_sph_shell_IO
      use ctl_data_platforms_IO
      use ctl_data_sph_monitor_IO
      use ctl_data_SGS_MHD_model_IO
      use ctl_data_visualiser_IO
!
      character(len=kchara), intent(in) :: hd_block
!
      type(mhd_simulation_control), intent(inout) ::  MHD_ctl
      type(SGS_model_control), intent(inout) ::       sgs_ctl
      type(tracers_control), intent(inout) ::         tracer_ctls
      type(visualization_controls), intent(inout) ::  viz_ctls
      type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
!
!
      MHD_ctl%block_name = trim(hd_block)
      call init_platforms_labels(hd_platform, MHD_ctl%plt)
      call init_platforms_labels(hd_org_data, MHD_ctl%org_plt)
      call init_platforms_labels(hd_new_data, MHD_ctl%new_plt)
      call init_parallel_shell_ctl_label(hd_sph_shell,                  &
     &                                   MHD_ctl%psph_ctl)
      call init_sph_sgs_mhd_model(hd_model, MHD_ctl%model_ctl, sgs_ctl)
      call init_sph_mhd_control_label(hd_control, MHD_ctl%smctl_ctl)
      call init_sph_monitoring_labels(hd_pick_sph,                      &
     &                                MHD_ctl%smonitor_ctl)
      call init_dynamo_viz_control(hd_dynamo_viz_ctl, zm_ctls)
      call init_viz_ctl_label(hd_viz_ctl, viz_ctls)
      call init_tracers_ctl_label(hd_tracer_ctl, tracer_ctls)
      call init_monitor_data_ctl_label(hd_monitor_data,                 &
     &                                 MHD_ctl%nmtr_ctl)
!
      end subroutine init_sph_sgs_mhd_ctl_label
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_mhd_control_data                              &
     &         (id_control, hd_block, MHD_ctl, sgs_ctl,                 &
     &          tracer_ctls, viz_ctls, zm_ctls, c_buf)
!
      use t_ctl_data_SPH_MHD_control
      use ctl_file_gen_sph_shell_IO
      use ctl_data_platforms_IO
      use ctl_data_sph_monitor_IO
      use ctl_data_SGS_MHD_model_IO
      use ctl_data_visualiser_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(mhd_simulation_control), intent(inout) ::  MHD_ctl
      type(SGS_model_control), intent(inout) ::       sgs_ctl
      type(tracers_control), intent(inout) ::         tracer_ctls
      type(visualization_controls), intent(inout) ::  viz_ctls
      type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(MHD_ctl%i_mhd_ctl .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
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
     &      MHD_ctl%fname_psph, MHD_ctl%psph_ctl, c_buf)
!
        call read_sph_sgs_mhd_model(id_control, hd_model,               &
     &                              MHD_ctl%model_ctl, sgs_ctl, c_buf)
        call read_sph_mhd_control                                       &
     &     (id_control, hd_control, MHD_ctl%smctl_ctl, c_buf)
!
        call read_monitor_data_ctl(id_control, hd_monitor_data,         &
     &                             MHD_ctl%nmtr_ctl, c_buf)
        call read_sph_monitoring_ctl(id_control, hd_pick_sph,           &
     &                               MHD_ctl%smonitor_ctl, c_buf)
!
        call s_read_viz_controls(id_control, hd_viz_ctl,                &
     &                           viz_ctls, c_buf)
        call read_tracer_controls(id_control, hd_tracer_ctl,            &
     &                           tracer_ctls, c_buf)
!
        call read_dynamo_viz_control(id_control, hd_dynamo_viz_ctl,     &
     &                               zm_ctls, c_buf)

! -----   Deprecated  ---------
        call read_dynamo_viz_control(id_control, hd_zm_viz_ctl,         &
     &                               zm_ctls, c_buf)
      end do
      MHD_ctl%i_mhd_ctl = 1
!
      end subroutine read_sph_mhd_control_data
!
!   --------------------------------------------------------------------
!
      subroutine write_sph_mhd_control_data(id_control, MHD_ctl,        &
     &          sgs_ctl, tracer_ctls, viz_ctls, zm_ctls, level)
!
      use t_ctl_data_SPH_MHD_control
      use ctl_data_SGS_MHD_model_IO
      use ctl_file_gen_sph_shell_IO
      use ctl_data_platforms_IO
      use ctl_data_sph_monitor_IO
      use ctl_data_visualiser_IO
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(mhd_simulation_control), intent(in) ::  MHD_ctl
      type(SGS_model_control), intent(in) ::       sgs_ctl
      type(visualization_controls), intent(in) ::  viz_ctls
      type(tracers_control), intent(in) ::         tracer_ctls
      type(sph_dynamo_viz_controls), intent(in) :: zm_ctls
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(MHD_ctl%i_mhd_ctl .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 MHD_ctl%block_name)
      call write_control_platforms                                      &
     &   (id_control, hd_platform, MHD_ctl%plt, level)
      call write_control_platforms                                      &
     &   (id_control, hd_org_data, MHD_ctl%org_plt, level)
      call write_control_platforms                                      &
     &   (id_control, hd_new_data, MHD_ctl%new_plt, level)
!
      call sel_write_ctl_gen_shell_grids(id_control,                    &
     &    MHD_ctl%fname_psph, MHD_ctl%psph_ctl, level)
!
      call write_sph_sgs_mhd_model(id_control, hd_model,                &
     &                             MHD_ctl%model_ctl, sgs_ctl, level)
      call write_sph_mhd_control(id_control, MHD_ctl%smctl_ctl, level)
!
      call write_monitor_data_ctl(id_control, MHD_ctl%nmtr_ctl, level)
      call write_sph_monitoring_ctl                                     &
     &   (id_control, MHD_ctl%smonitor_ctl, level)
!
      call write_viz_controls(id_control, viz_ctls, level)
      call write_tracer_controls(id_control, tracer_ctls, level)
      call write_dynamo_viz_control(id_control, zm_ctls, level)
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                MHD_ctl%block_name)
!
      end subroutine write_sph_mhd_control_data
!
!   --------------------------------------------------------------------
!
      end module ctl_data_SGS_SPH_MHD_IO
