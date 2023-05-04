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
!!      subroutine read_sph_mhd_control_data                            &
!!     &         (id_control, hd_block, MHD_ctl, viz_ctls, c_buf)
!!         integer(kind = kint), intent(in) :: id_control
!!         character(len=kchara), intent(in) :: hd_block
!!         type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!         type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_sph_mhd_control_data                           &
!!     &         (id_control, hd_block, MHD_ctl, viz_ctls, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(sph_sgs_mhd_control), intent(in) :: MHD_ctl
!!        type(visualization_controls), intent(in) :: viz_ctls
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_sph_SGS_MHD_viz_ctl(MHD_ctl, viz_ctls)
!!      subroutine dealloc_sph_sgs_mhd_ctl_data(MHD_ctl)
!!         type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!@endverbatim
!
      module t_ctl_data_SGS_MHD
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_MHD_model
      use t_ctl_data_SPH_MHD_control
      use t_ctl_data_SGS_model
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
      use t_ctl_data_gen_sph_shell
      use t_control_data_dynamo_vizs
!
      implicit none
!
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
!>        Structures for SGS controls
        type(SGS_model_control) :: sgs_ctl
!
!>        Structure for spectr monitoring control
        type(sph_monitor_control) :: smonitor_ctl
!>        Structure for monitoring plave list
        type(node_monitor_control) :: nmtr_ctl
!
!>        Structures of zonal mean controls
        type(sph_dynamo_viz_controls) :: zm_ctls
!
        integer (kind=kint) :: i_mhd_ctl = 0
      end type sph_sgs_mhd_control
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
      character(len=kchara), parameter , private                        &
     &                    :: hd_control = 'control'
      character(len=kchara), parameter, private                         &
     &                    :: hd_pick_sph = 'sph_monitor_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_monitor_data = 'monitor_data_ctl'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_control = 'visual_control'
      character(len=kchara), parameter, private                         &
     &                    :: hd_dynamo_viz_ctl = 'dynamo_vizs_control'
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
      subroutine read_sph_mhd_control_data                              &
     &         (id_control, hd_block, MHD_ctl, viz_ctls, c_buf)
!
      use t_ctl_data_SPH_MHD_control
      use t_control_data_vizs
      use ctl_file_gen_sph_shell_IO
      use ctl_data_platforms_IO
      use ctl_data_sph_monitor_IO
      use ctl_data_SGS_MHD_model_IO
      use ctl_data_viualiser_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
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
        call read_sph_sgs_mhd_model(id_control, hd_model,               &
     &      MHD_ctl%model_ctl, MHD_ctl%sgs_ctl, c_buf)
        call read_sph_mhd_control                                       &
     &     (id_control, hd_control, MHD_ctl%smctl_ctl, c_buf)
!
        call read_monitor_data_ctl                                      &
     &     (id_control, hd_monitor_data, MHD_ctl%nmtr_ctl, c_buf)
        call read_sph_monitoring_ctl                                    &
     &     (id_control, hd_pick_sph, MHD_ctl%smonitor_ctl, c_buf)
!
        call s_read_viz_controls(id_control, hd_viz_control,            &
     &                           viz_ctls, c_buf)
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
!
      subroutine write_sph_mhd_control_data                             &
     &         (id_control, hd_block, MHD_ctl, viz_ctls, level)
!
      use t_ctl_data_SPH_MHD_control
      use t_control_data_vizs
      use ctl_data_SGS_MHD_model_IO
      use ctl_file_gen_sph_shell_IO
      use ctl_data_platforms_IO
      use ctl_data_sph_monitor_IO
      use ctl_data_viualiser_IO
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(sph_sgs_mhd_control), intent(in) :: MHD_ctl
      type(visualization_controls), intent(in) :: viz_ctls
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(MHD_ctl%i_mhd_ctl .le. 0) return
      write(id_control,'(a1)') '!'
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
!
      call write_control_platforms                                      &
     &   (id_control, hd_platform, MHD_ctl%plt, level)
      call write_control_platforms                                      &
     &   (id_control, hd_org_data, MHD_ctl%org_plt, level)
      call write_control_platforms                                      &
     &   (id_control, hd_new_data, MHD_ctl%new_plt, level)
!
      call sel_write_ctl_gen_shell_grids(id_control, hd_sph_shell,      &
     &    MHD_ctl%fname_psph_ctl, MHD_ctl%psph_ctl, level)
!
      call write_sph_sgs_mhd_model(id_control, hd_model,                &
     &    MHD_ctl%model_ctl, MHD_ctl%sgs_ctl, level)
      call write_sph_mhd_control                                        &
     &   (id_control, hd_control, MHD_ctl%smctl_ctl, level)
!
      call write_monitor_data_ctl                                       &
     &   (id_control, hd_monitor_data, MHD_ctl%nmtr_ctl, level)
      call write_sph_monitoring_ctl                                     &
     &   (id_control, hd_pick_sph, MHD_ctl%smonitor_ctl, level)
!
      call write_viz_controls(id_control, hd_viz_control,               &
     &                        viz_ctls, level)
!
      call write_dynamo_viz_control                                     &
     &   (id_control, hd_dynamo_viz_ctl, MHD_ctl%zm_ctls, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_sph_mhd_control_data
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_SGS_MHD_viz_ctl(MHD_ctl, viz_ctls)
!
      use t_ctl_data_SPH_MHD_control
      use t_control_data_vizs
!
      type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
!
      call dealloc_viz_controls(viz_ctls)
      call dealloc_dynamo_viz_control(MHD_ctl%zm_ctls)
!
      end subroutine dealloc_sph_SGS_MHD_viz_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_sgs_mhd_ctl_data(MHD_ctl)
!
      use t_ctl_data_SPH_MHD_control
!
      type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
!
!
      call reset_control_platforms(MHD_ctl%plt)
      call reset_control_platforms(MHD_ctl%org_plt)
!
      call dealloc_sgs_ctl(MHD_ctl%sgs_ctl)
      call dealloc_sph_mhd_model(MHD_ctl%model_ctl)
      call reset_sph_mhd_control(MHD_ctl%smctl_ctl)
!
      call dealloc_parallel_shell_ctl(MHD_ctl%psph_ctl)
!
      call dealloc_monitor_data_ctl(MHD_ctl%nmtr_ctl)
      call dealloc_sph_monitoring_ctl(MHD_ctl%smonitor_ctl)
!
      MHD_ctl%i_mhd_ctl = 0
!
      end subroutine dealloc_sph_sgs_mhd_ctl_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_SGS_MHD
