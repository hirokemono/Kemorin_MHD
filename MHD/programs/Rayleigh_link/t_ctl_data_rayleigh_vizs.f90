!>@file   t_ctl_data_rayleigh_vizs.f90
!!@brief  module t_ctl_data_rayleigh_vizs
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in July, 2019
!
!>@brief  Main loop of visualization of Rayleigh data
!!
!!@verbatim
!!      subroutine read_ctl_file_reayleigh_viz(rayleigh_vizs_ctl)
!!      subroutine dealloc_rayleigh_vizs_ctl_data(rayleigh_vizs_ctl)
!!        type(control_data_rayleigh_vizs), intent(inout)               &
!!     &                     :: vizs_ctlrayleigh_vizs_ctl
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
      character(len = kchara), parameter                                &
     &               :: fname_viz_ctl = "control_viz_rayleigh"
!
      type control_data_rayleigh_vizs
!>      Structure for file settings
        type(platform_data_control) :: viz_plt
!>        Structure for field information control
        type(field_control) :: field_ctl
!>        Structure for time stepping control
        type(time_data_control) :: t_viz_ctl
!>        Structures of visualization controls
        type(visualization_controls) :: viz_ctl_v
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
      character(len=kchara), parameter                                  &
     &                    :: hd_viz_control = 'visual_control'
!
      private :: hd_viz_control
!
      private :: viz_ctl_file_code, fname_viz_ctl
!
      private :: read_rayleigh_vizs_ctl_data
      private :: bcast_rayleigh_vizs_ctl_data
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_file_reayleigh_viz(rayleigh_vizs_ctl)
!
      use skip_comment_f
!
      type(control_data_rayleigh_vizs), intent(inout)                   &
     &                         :: rayleigh_vizs_ctl
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open (viz_ctl_file_code, file=fname_viz_ctl, status='old' )
        do
          call load_one_line_from_control(viz_ctl_file_code, c_buf1)
          call read_rayleigh_vizs_ctl_data                              &
     &       (viz_ctl_file_code, hd_rayleigh_viz,                       &
     &        rayleigh_vizs_ctl, c_buf1)
          if(rayleigh_vizs_ctl%i_viz_only_file .gt. 0) exit
        end do
        close(viz_ctl_file_code)
      end if
!
      call bcast_rayleigh_vizs_ctl_data(rayleigh_vizs_ctl)
!
      end subroutine read_ctl_file_reayleigh_viz
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_rayleigh_vizs_ctl_data                            &
     &         (id_control, hd_block, rayleigh_vizs_ctl, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_rayleigh_vizs), intent(inout)                   &
     &                         :: rayleigh_vizs_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(rayleigh_vizs_ctl%i_viz_only_file .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, rayleigh_vizs_ctl%viz_plt, c_buf)
        call read_control_time_step_data(id_control, hd_time_step,      &
     &      rayleigh_vizs_ctl%t_viz_ctl, c_buf)
!
      call read_phys_data_control(id_control, hd_phys_values,           &
     &    rayleigh_vizs_ctl%field_ctl, c_buf)
!
        call read_control_shell_domain                                  &
     &     (id_control, hd_domains_sph, rayleigh_vizs_ctl%sdctl, c_buf)
!
        call read_viz_controls(id_control, hd_viz_control,              &
     &      rayleigh_vizs_ctl%viz_ctl_v, c_buf)
      end do
      rayleigh_vizs_ctl%i_viz_only_file = 1
!
      end subroutine read_rayleigh_vizs_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_rayleigh_vizs_ctl_data(rayleigh_vizs_ctl)
!
      use bcast_4_platform_ctl
      use bcast_4_time_step_ctl
      use bcast_4_field_ctl
      use bcast_4_sphere_ctl
!
      type(control_data_rayleigh_vizs), intent(inout)                   &
     &                                 :: rayleigh_vizs_ctl
!
!
      call bcast_ctl_data_4_platform(rayleigh_vizs_ctl%viz_plt)
      call bcast_ctl_data_4_time_step(rayleigh_vizs_ctl%t_viz_ctl)
      call bcast_viz_controls(rayleigh_vizs_ctl%viz_ctl_v)
!
      call bcast_phys_data_ctl(rayleigh_vizs_ctl%field_ctl)
      call bcast_ctl_ndomain_4_shell(rayleigh_vizs_ctl%sdctl)
!
      call MPI_BCAST(rayleigh_vizs_ctl%i_viz_only_file, 1,              &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_rayleigh_vizs_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_rayleigh_vizs_ctl_data(rayleigh_vizs_ctl)
!
      type(control_data_rayleigh_vizs), intent(inout)                   &
     &                                 :: rayleigh_vizs_ctl
!
      call reset_control_platforms(rayleigh_vizs_ctl%viz_plt)
      call dealloc_viz_controls(rayleigh_vizs_ctl%viz_ctl_v)
!
      call dealloc_phys_control(rayleigh_vizs_ctl%field_ctl)
      call dealloc_ndomain_rtp_ctl(rayleigh_vizs_ctl%sdctl)
!
      rayleigh_vizs_ctl%i_viz_only_file = 0
!
      end subroutine dealloc_rayleigh_vizs_ctl_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_rayleigh_vizs
