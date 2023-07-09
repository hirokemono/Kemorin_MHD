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
!!     &         (file_name, FEM_MHD_ctl, sgs_ctl, viz_ctls, c_buf)
!!        character(len=kchara), intent(in) :: file_name
!!        type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!        type(buffer_for_control), intent(inout) :: c_buf
!!      subroutine write_control_4_fem_MHD                              &
!!     &         (file_name, FEM_MHD_ctl, sgs_ctl, viz_ctls)
!!        character(len=kchara), intent(in) :: file_name
!!        type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
!!        type(SGS_model_control), intent(in) :: sgs_ctl
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!
!!      subroutine dealloc_fem_mhd_ctl_data                             &
!!     &        (FEM_MHD_ctl, sgs_ctl, viz_ctls)
!!        type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!@endverbatim
!
      module t_ctl_data_FEM_MHD
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_MHD_model
      use t_ctl_data_FEM_MHD_control
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
      use t_control_data_vizs
!
      implicit none
!
!
      integer(kind=kint), parameter :: ctl_file_code = 11
!
      type fem_mhd_control
!>        Block name
        character(len=kchara) :: block_name = 'MHD_control'
!
!>        Structure for file settings
        type(platform_data_control) :: plt
!>        Control structure for orginal file informations
        type(platform_data_control) :: org_plt
!>        Control structure for new file informations
        type(platform_data_control) :: new_plt
!
!>        Control structure for MHD/model
        type(mhd_model_control) :: model_ctl
!>        Control structure for MHD/control
        type(fem_mhd_control_control) :: fmctl_ctl
!
!>        Structure for spectr monitoring control
        type(sph_monitor_control) :: smonitor_ctl
!>        Structure for monitoring slave list
        type(node_monitor_control) :: nmtr_ctl
!
        integer(kind = kint) :: i_mhd_ctl = 0
      end type fem_mhd_control
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_Fmhd_ctl = 'MHD_control'
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
     &      :: hd_sph_shell = 'spherical_shell_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_model =   'model'
      character(len=kchara), parameter, private                         &
     &                    :: hd_control = 'control'
      character(len=kchara), parameter, private                         &
     &                     :: hd_pick_sph = 'sph_monitor_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_monitor_data = 'monitor_data_ctl'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_control = 'visual_control'
!
      private :: read_fem_mhd_control_data
      private :: write_fem_mhd_control_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_fem_MHD                                 &
     &         (file_name, FEM_MHD_ctl, sgs_ctl, viz_ctls, c_buf)
!
      use t_ctl_data_SGS_model
      use viz_step_ctls_to_time_ctl
!
      character(len=kchara), intent(in) :: file_name
      type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
      type(SGS_model_control), intent(inout) :: sgs_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      open(ctl_file_code, file = file_name, status='old' )
!
      do
        call load_one_line_from_control(ctl_file_code,                  &
     &                                  hd_Fmhd_ctl, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call read_fem_mhd_control_data(ctl_file_code, hd_Fmhd_ctl,      &
     &      FEM_MHD_ctl, sgs_ctl, viz_ctls, c_buf)
        if(FEM_MHD_ctl%i_mhd_ctl .gt. 0) exit
      end do
      close(ctl_file_code)
!
      c_buf%level = c_buf%level - 1
      if(c_buf%iend .gt. 0) return
!
      call s_viz_step_ctls_to_time_ctl                                  &
     &   (viz_ctls, FEM_MHD_ctl%fmctl_ctl%tctl)
      call add_fields_4_vizs_to_fld_ctl(viz_ctls,                       &
     &    FEM_MHD_ctl%model_ctl%fld_ctl%field_ctl)
!
      end subroutine read_control_4_fem_MHD
!
! ----------------------------------------------------------------------
!
      subroutine write_control_4_fem_MHD                                &
     &         (file_name, FEM_MHD_ctl, sgs_ctl, viz_ctls)
!
      use t_ctl_data_SGS_model
      use delete_data_files
!
      character(len=kchara), intent(in) :: file_name
      type(fem_mhd_control), intent(in) :: FEM_MHD_ctl
      type(SGS_model_control), intent(in) :: sgs_ctl
      type(visualization_controls), intent(in) :: viz_ctls
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
      open(ctl_file_code, file = file_name)
      call write_fem_mhd_control_data                                   &
     &   (ctl_file_code, FEM_MHD_ctl%block_name,                        &
     &    FEM_MHD_ctl, sgs_ctl, viz_ctls, level1)
      close(ctl_file_code)
!
      end subroutine write_control_4_fem_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_fem_mhd_control_data(id_control, hd_block,        &
     &          FEM_MHD_ctl, sgs_ctl, viz_ctls, c_buf)
!
      use t_ctl_data_SGS_model
!
      use ctl_data_SGS_MHD_model_IO
      use ctl_data_platforms_IO
      use ctl_data_visualiser_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
      type(SGS_model_control), intent(inout) :: sgs_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(FEM_MHD_ctl%i_mhd_ctl .gt. 0) return
      FEM_MHD_ctl%block_name = trim(hd_block)
      call init_platforms_labels(hd_platform, FEM_MHD_ctl%plt)
      call init_platforms_labels(hd_org_data, FEM_MHD_ctl%org_plt)
      call init_sph_sgs_mhd_model(hd_model, FEM_MHD_ctl%model_ctl,      &
     &                            sgs_ctl)
      call init_viz_ctl_label(hd_viz_control, viz_ctls)
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, FEM_MHD_ctl%plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_org_data, FEM_MHD_ctl%org_plt, c_buf)
!
        call read_sph_sgs_mhd_model(id_control, hd_model,               &
     &      FEM_MHD_ctl%model_ctl, sgs_ctl, c_buf)
        call read_fem_mhd_control                                       &
     &     (id_control, hd_control, FEM_MHD_ctl%fmctl_ctl, c_buf)
!
        call read_monitor_data_ctl(id_control, hd_monitor_data,         &
     &                             FEM_MHD_ctl%nmtr_ctl, c_buf)
        call s_read_viz_controls(id_control, hd_viz_control,            &
     &                           viz_ctls, c_buf)
      end do
      FEM_MHD_ctl%i_mhd_ctl = 1
!
      end subroutine read_fem_mhd_control_data
!
!   --------------------------------------------------------------------
!
      subroutine write_fem_mhd_control_data(id_control, hd_block,       &
     &          FEM_MHD_ctl, sgs_ctl, viz_ctls, level)
!
      use t_ctl_data_SGS_model
!
      use ctl_data_SGS_MHD_model_IO
      use ctl_data_platforms_IO
      use ctl_data_visualiser_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(fem_mhd_control), intent(in) :: FEM_MHD_ctl
      type(SGS_model_control), intent(in) :: sgs_ctl
      type(visualization_controls), intent(in) :: viz_ctls
      integer(kind = kint), intent(inout) :: level
!
!
      if(FEM_MHD_ctl%i_mhd_ctl .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_platforms                                      &
     &   (id_control, hd_platform, FEM_MHD_ctl%plt, level)
      call write_control_platforms                                      &
     &   (id_control, hd_org_data, FEM_MHD_ctl%org_plt, level)
!
      call write_sph_sgs_mhd_model(id_control, hd_model,                &
     &    FEM_MHD_ctl%model_ctl, sgs_ctl, level)
      call write_fem_mhd_control                                        &
     &   (id_control, hd_control, FEM_MHD_ctl%fmctl_ctl, level)
!
      call write_monitor_data_ctl(id_control,                           &
     &                            FEM_MHD_ctl%nmtr_ctl, level)
      call write_viz_controls(id_control, viz_ctls, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_fem_mhd_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_fem_mhd_ctl_data                               &
     &        (FEM_MHD_ctl, sgs_ctl, viz_ctls)
!
      use t_ctl_data_SGS_model
!
      type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
      type(SGS_model_control), intent(inout) :: sgs_ctl
!
!
      call reset_control_platforms(FEM_MHD_ctl%plt)
      call reset_control_platforms(FEM_MHD_ctl%org_plt)
!
      call dealloc_sgs_ctl(sgs_ctl)
      call dealloc_sph_mhd_model(FEM_MHD_ctl%model_ctl)
      call dealloc_fem_mhd_control(FEM_MHD_ctl%fmctl_ctl)
!
      call dealloc_monitor_data_ctl(FEM_MHD_ctl%nmtr_ctl)
!
      call dealloc_viz_controls(viz_ctls)
!
      FEM_MHD_ctl%i_mhd_ctl = 0
!
      end subroutine dealloc_fem_mhd_ctl_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_FEM_MHD
