!>@file   t_ctl_data_rayleigh_vizs.f90
!!@brief  module t_ctl_data_rayleigh_vizs
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in July, 2019
!
!>@brief  Main loop of visualization of Rayleigh data
!!
!!@verbatim
!!      subroutine read_ctl_file_rayleigh_viz                           &
!!     &         (file_name, rayleigh_vizs_ctl, viz_ctls, c_buf)
!!        character(len = kchara), intent(in) :: file_name
!!        type(control_data_rayleigh_vizs), intent(inout)               &
!!     &                         :: rayleigh_vizs_ctl
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!        type(buffer_for_control), intent(inout) :: c_buf
!!      subroutine write_ctl_file_rayleigh_viz                          &
!!     &         (file_name, rayleigh_vizs_ctl, viz_ctls)
!!      subroutine dealloc_rayleigh_vizs_ctl_data(rayleigh_vizs_ctl)
!!        character(len = kchara), intent(in) :: file_name
!!        type(control_data_rayleigh_vizs), intent(inout)               &
!!     &                     :: vizs_ctlrayleigh_vizs_ctl
!!        type(visualization_controls), intent(in) :: viz_ctls
!!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin rayleigh_visualizer
!!    begin data_files_def
!!      ...
!!    end data_files_def
!!
!!    begin num_domain_ctl
!!      num_radial_domain_ctl         2
!!      num_horizontal_domain_ctl     2
!!    end num_domain_ctl
!!
!!    begin phys_values_ctl
!!      ...
!!    end phys_values_ctls
!!
!!    begin time_step_ctl
!!      ...
!!    end time_step_ctl
!!
!!    begin visual_control
!!      ...
!!    end  visual_control
!!  end  rayleigh_visualizer
!!
!!    -------------------------------------------------------------------
!!@endverbatim
!
!
      module t_ctl_data_rayleigh_vizs
!
      use m_precision
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
      use t_ctl_data_4_fields
      use t_ctl_data_4_divide_sphere
      use t_control_data_vizs
!
      implicit  none
!
!
      integer(kind = kint), parameter :: viz_ctl_file_code = 11
!
!>        Structures of Rayleigh convert control data
      type control_data_rayleigh_vizs
!>      Structure for file settings
        type(platform_data_control) :: viz_plt
!>        Structure for field information control
        type(field_control) :: fld_ctl
!>        Structure for time stepping control
        type(time_data_control) :: t_viz_ctl
!
!>        Structure of spherical shell domain decomposition
        type(sphere_domain_control) :: sdctl
!
        integer(kind=kint) :: i_viz_only_file = 0
      end type control_data_rayleigh_vizs
!
!     Top level
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_rayleigh_viz = 'rayleigh_visualizer'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform =    'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_phys_values = 'phys_values_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_time_step =   'time_step_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_domains_sph = 'num_domain_ctl'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_control = 'visual_control'
!
      private :: viz_ctl_file_code
      private :: read_rayleigh_vizs_ctl_data
      private :: write_rayleigh_vizs_ctl_data
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_file_rayleigh_viz                             &
     &         (file_name, rayleigh_vizs_ctl, viz_ctls, c_buf)
!
      use skip_comment_f
      use viz_step_ctls_to_time_ctl
!
      character(len = kchara), intent(in) :: file_name
      type(control_data_rayleigh_vizs), intent(inout)                   &
     &                         :: rayleigh_vizs_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      open (viz_ctl_file_code, file=file_name, status='old')
      do
        call load_one_line_from_control                                 &
     &     (viz_ctl_file_code, hd_rayleigh_viz, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call read_rayleigh_vizs_ctl_data                                &
     &     (viz_ctl_file_code, hd_rayleigh_viz,                         &
     &      rayleigh_vizs_ctl, viz_ctls, c_buf)
        if(rayleigh_vizs_ctl%i_viz_only_file .gt. 0) exit
      end do
      close(viz_ctl_file_code)
!
      c_buf%level = c_buf%level - 1
      if(c_buf%iend .gt. 0) return
!
      call s_viz_step_ctls_to_time_ctl                                  &
     &   (viz_ctls, rayleigh_vizs_ctl%t_viz_ctl)
      call add_fields_4_vizs_to_fld_ctl                                 &
     &   (viz_ctls, rayleigh_vizs_ctl%fld_ctl%field_ctl)
!
      end subroutine read_ctl_file_rayleigh_viz
!
!   --------------------------------------------------------------------
!
      subroutine write_ctl_file_rayleigh_viz                            &
     &         (file_name, rayleigh_vizs_ctl, viz_ctls)
!
      use delete_data_files
!
      character(len = kchara), intent(in) :: file_name
      type(control_data_rayleigh_vizs), intent(in)                      &
     &                         :: rayleigh_vizs_ctl
      type(visualization_controls), intent(in) :: viz_ctls
!
      integer(kind = kint) :: level1
!
      if(check_file_exist(file_name)) then
        write(*,*) 'File ', trim(file_name), ' exist. Continue?'
        read(*,*)
      end if
!
      open (viz_ctl_file_code, file=file_name, status='old')
      level1 = 0
      call write_rayleigh_vizs_ctl_data(viz_ctl_file_code,              &
     &    hd_rayleigh_viz, rayleigh_vizs_ctl, viz_ctls, level1)
      close(viz_ctl_file_code)
!
      end subroutine write_ctl_file_rayleigh_viz
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_rayleigh_vizs_ctl_data(id_control, hd_block,      &
     &          rayleigh_vizs_ctl, viz_ctls, c_buf)
!
      use skip_comment_f
      use ctl_data_platforms_IO
      use ctl_data_4_time_steps_IO
      use ctl_data_visualiser_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_rayleigh_vizs), intent(inout)                   &
     &                         :: rayleigh_vizs_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(rayleigh_vizs_ctl%i_viz_only_file .gt. 0) return
      call init_platforms_labels(hd_platform,                           &
     &                           rayleigh_vizs_ctl%viz_plt)
      call init_ctl_shell_domain_label(hd_domains_sph,                  &
     &                                 rayleigh_vizs_ctl%sdctl)
      call init_phys_data_ctl_label(hd_phys_values,                     &
     &                              rayleigh_vizs_ctl%fld_ctl)
      call init_ctl_time_step_label(hd_time_step,                       &
     &                              rayleigh_vizs_ctl%t_viz_ctl)
      call init_viz_ctl_label(hd_viz_control, viz_ctls)
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, rayleigh_vizs_ctl%viz_plt, c_buf)
        call read_control_time_step_data(id_control, hd_time_step,      &
     &      rayleigh_vizs_ctl%t_viz_ctl, c_buf)
!
        call read_phys_data_control(id_control, hd_phys_values,         &
     &      rayleigh_vizs_ctl%fld_ctl, c_buf)
!
        call read_control_shell_domain                                  &
     &     (id_control, hd_domains_sph, rayleigh_vizs_ctl%sdctl, c_buf)
!
        call s_read_viz_controls(id_control, hd_viz_control,            &
     &                           viz_ctls, c_buf)
      end do
      rayleigh_vizs_ctl%i_viz_only_file = 1
!
      end subroutine read_rayleigh_vizs_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine write_rayleigh_vizs_ctl_data(id_control, hd_block,     &
     &          rayleigh_vizs_ctl, viz_ctls, level)
!
      use skip_comment_f
      use ctl_data_platforms_IO
      use ctl_data_4_time_steps_IO
      use ctl_data_visualiser_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(control_data_rayleigh_vizs), intent(in) :: rayleigh_vizs_ctl
      type(visualization_controls), intent(in) :: viz_ctls
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(rayleigh_vizs_ctl%i_viz_only_file .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_platforms                                      &
     &   (id_control, hd_platform, rayleigh_vizs_ctl%viz_plt, level)
      call write_control_time_step_data(id_control,                     &
     &    rayleigh_vizs_ctl%t_viz_ctl, level)
!
      call write_phys_data_control(id_control,                          &
     &    rayleigh_vizs_ctl%fld_ctl, level)
!
      call write_control_shell_domain                                   &
     &   (id_control, rayleigh_vizs_ctl%sdctl, level)
!
      call write_viz_controls(id_control, viz_ctls, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_rayleigh_vizs_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_rayleigh_vizs_ctl_data(rayleigh_vizs_ctl)
!
      type(control_data_rayleigh_vizs), intent(inout)                   &
     &                                 :: rayleigh_vizs_ctl
!
      call reset_control_platforms(rayleigh_vizs_ctl%viz_plt)
!
      call dealloc_phys_control(rayleigh_vizs_ctl%fld_ctl)
      call dealloc_ndomain_rtp_ctl(rayleigh_vizs_ctl%sdctl)
!
      rayleigh_vizs_ctl%i_viz_only_file = 0
!
      end subroutine dealloc_rayleigh_vizs_ctl_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_rayleigh_vizs
