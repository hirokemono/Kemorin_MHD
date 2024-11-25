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
!!      subroutine read_control_4_sph_SGS_MHD(file_name, MHD_ctl,       &
!!     &          sgs_ctl, tracer_ctls, viz_ctls, zm_ctls, c_buf)
!!      subroutine read_control_file_sph_SGS_MHD(file_name, MHD_ctl,    &
!!     &          sgs_ctl, tracer_ctls, viz_ctls, zm_ctls, c_buf)
!!        character(len=kchara), intent(in) :: file_name
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_simulation_control), intent(inout) :: MHD_ctl
!!        type(SGS_model_control), intent(inout) ::       sgs_ctl
!!        type(tracers_control), intent(inout) ::         tracer_ctls
!!        type(visualization_controls), intent(inout) ::  viz_ctls
!!        type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_control_file_sph_SGS_MHD(file_name, MHD_ctl,   &
!!     &          sgs_ctl, tracer_ctls, viz_ctls, zm_ctls)
!!        character(len=kchara), intent(in) :: file_name
!!        integer(kind = kint), intent(in) :: id_control
!!        type(mhd_simulation_control), intent(in) ::  MHD_ctl
!!        type(SGS_model_control), intent(in) ::       sgs_ctl
!!        type(tracers_control), intent(in) ::         tracer_ctls
!!        type(visualization_controls), intent(in) ::  viz_ctls
!!        type(sph_dynamo_viz_controls), intent(in) :: zm_ctls
!!        integer(kind = kint), intent(inout) :: level
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
      module t_ctl_data_SGS_MHD
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
!>      Control file name
      character(len=kchara), parameter, private                         &
     &                                 :: hd_mhd_ctl = 'MHD_control'
!
      integer(kind=kint), parameter, private :: id_control_file = 11
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_SGS_MHD(file_name, MHD_ctl,         &
     &          sgs_ctl, tracer_ctls, viz_ctls, zm_ctls, c_buf)
!
      use t_ctl_data_SPH_MHD_control
      use viz_step_ctls_to_time_ctl
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(SGS_model_control), intent(inout) ::       sgs_ctl
      type(tracers_control), intent(inout) ::         tracer_ctls
      type(visualization_controls), intent(inout) ::  viz_ctls
      type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      call read_control_file_sph_SGS_MHD(file_name, MHD_ctl,            &
     &    sgs_ctl, tracer_ctls, viz_ctls, zm_ctls, c_buf)
      if(c_buf%iend .gt. 0) return
!
      call s_viz_step_ctls_to_time_ctl                                  &
     &   (viz_ctls, MHD_ctl%smctl_ctl%tctl)
      call tracer_step_ctls_to_time_ctl                                 &
     &   (tracer_ctls, MHD_ctl%smctl_ctl%tctl)
!
      call add_fields_4_vizs_to_fld_ctl                                 &
     &   (viz_ctls, MHD_ctl%model_ctl%fld_ctl%field_ctl)
      call add_flds_4_tracers_to_fld_ctl                                &
     &   (tracer_ctls, MHD_ctl%model_ctl%fld_ctl%field_ctl)
!
      end subroutine read_control_4_sph_SGS_MHD
!
! ----------------------------------------------------------------------
!
      subroutine read_control_file_sph_SGS_MHD(file_name, MHD_ctl,      &
     &          sgs_ctl, tracer_ctls, viz_ctls, zm_ctls, c_buf)
!
      use t_ctl_data_SPH_MHD_control
      use viz_step_ctls_to_time_ctl
      use ctl_data_SGS_SPH_MHD_IO
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(SGS_model_control), intent(inout) ::       sgs_ctl
      type(tracers_control), intent(inout) ::         tracer_ctls
      type(visualization_controls), intent(inout) ::  viz_ctls
      type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      call init_sph_sgs_mhd_ctl_label(hd_mhd_ctl, MHD_ctl,              &
     &    sgs_ctl, tracer_ctls, viz_ctls, zm_ctls)
      open(id_control_file, file = file_name, status='old' )
!
      do
        call load_one_line_from_control(id_control_file,                &
     &                                  hd_mhd_ctl, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call read_sph_mhd_control_data(id_control_file, hd_mhd_ctl,     &
     &      MHD_ctl, sgs_ctl, tracer_ctls, viz_ctls, zm_ctls, c_buf)
        if(MHD_ctl%i_mhd_ctl .gt. 0) exit
      end do
      close(id_control_file)
      c_buf%level = c_buf%level - 1
!
      end subroutine read_control_file_sph_SGS_MHD
!
! ----------------------------------------------------------------------
!
      subroutine write_control_file_sph_SGS_MHD(file_name, MHD_ctl,     &
     &          sgs_ctl, tracer_ctls, viz_ctls, zm_ctls)
!
      use ctl_data_SGS_SPH_MHD_IO
      use delete_data_files
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(in) :: MHD_ctl
      type(SGS_model_control), intent(inout) ::       sgs_ctl
      type(tracers_control), intent(inout) ::         tracer_ctls
      type(visualization_controls), intent(inout) ::  viz_ctls
      type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
!
      integer(kind = kint) :: level1
!
!
      if(check_file_exist(file_name)) then
        write(*,*) 'File ', trim(file_name), ' exist. Continue?'
        read(*,*)
      end if
!
      write(*,*) 'Write MHD control file: ', trim(file_name)
      level1 = 0
      open(id_control_file, file = file_name)
      call write_sph_mhd_control_data(id_control_file,                  &
     &    MHD_ctl, sgs_ctl, tracer_ctls, viz_ctls, zm_ctls, level1)
      close(id_control_file)
!
      end subroutine write_control_file_sph_SGS_MHD
!
! ----------------------------------------------------------------------
!
      end module t_ctl_data_SGS_MHD
