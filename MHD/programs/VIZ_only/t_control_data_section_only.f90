!t_control_data_section_only.f90
!      module t_control_data_section_only
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine read_control_data_section_only(sec_viz_ctl)
!!      subroutine reset_section_control_data(sec_viz_ctl)
!!        type(control_data_section_only), intent(inout) :: sec_viz_ctl
!!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin visualizer
!!    begin data_files_def
!!      ...
!!    end data_files_def
!!
!!    begin time_step_ctl
!!      ...
!!    end time_step_ctl
!!
!!    begin visual_control
!!      ...
!!    end  visual_control
!!  end  visualizer
!!
!!    -------------------------------------------------------------------
!
      module t_control_data_section_only
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
      use t_control_data_sections
      use t_control_data_isosurfaces
!
      implicit  none
!
!
      integer(kind = kint), parameter :: viz_ctl_file_code = 11
      character(len = kchara), parameter :: fname_viz_ctl = "ctl_viz"
!
      type control_data_section_only
!>      Structure for file settings
        type(platform_data_control) :: sect_plt
!>      Structure for time stepping control
        type(time_data_control) :: t_sect_ctl
!
        type(section_controls) :: sect_psf_ctls
        type(isosurf_controls) :: sect_iso_ctls
!
        integer (kind=kint) :: i_viz_only_file = 0
      end type control_data_section_only
!
!     top level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_viz_only_file = 'visualizer'
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
!
      private :: hd_viz_only_file
      private :: hd_platform, hd_time_step
!
      private :: viz_ctl_file_code, fname_viz_ctl
!
      private :: read_section_control_data, bcast_section_control_data
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_data_section_only(sec_viz_ctl)
!
      use skip_comment_f
!
      type(control_data_section_only), intent(inout) :: sec_viz_ctl
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open (viz_ctl_file_code, file=fname_viz_ctl, status='old' )
        do
          call load_one_line_from_control(viz_ctl_file_code, c_buf1)
          call read_section_control_data                                &
     &       (viz_ctl_file_code, hd_viz_only_file, sec_viz_ctl, c_buf1)
          if(sec_viz_ctl%i_viz_only_file .gt. 0) exit
        end do
        close(viz_ctl_file_code)
      end if
!
      call bcast_section_control_data(sec_viz_ctl)
!
      end subroutine read_control_data_section_only
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_section_control_data                              &
     &         (id_control, hd_block, sec_viz_ctl, c_buf)
!
      use skip_comment_f
      use read_sections_control_data
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_section_only), intent(inout) :: sec_viz_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(sec_viz_ctl%i_viz_only_file .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, sec_viz_ctl%sect_plt, c_buf)
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, sec_viz_ctl%t_sect_ctl, c_buf)
!
        call s_read_sections_control_data(id_control,                   &
     &     sec_viz_ctl%sect_psf_ctls, sec_viz_ctl%sect_iso_ctls, c_buf)
      end do
      sec_viz_ctl%i_viz_only_file = 1
!
      end subroutine read_section_control_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_section_control_data(sec_viz_ctl)
!
      use bcast_4_platform_ctl
      use bcast_4_time_step_ctl
!
      type(control_data_section_only), intent(inout) :: sec_viz_ctl
!
!
      call bcast_ctl_data_4_platform(sec_viz_ctl%sect_plt)
      call bcast_ctl_data_4_time_step(sec_viz_ctl%t_sect_ctl)
      call bcast_files_4_psf_ctl(sec_viz_ctl%sect_psf_ctls)
      call bcast_files_4_iso_ctl(sec_viz_ctl%sect_iso_ctls)
!
      call MPI_BCAST(sec_viz_ctl%i_viz_only_file, 1,                    &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_section_control_data
!
!   --------------------------------------------------------------------
!
      subroutine reset_section_control_data(sec_viz_ctl)
!
      type(control_data_section_only), intent(inout) :: sec_viz_ctl
!
!
      call reset_control_platforms(sec_viz_ctl%sect_plt)
      call dealloc_psf_ctl_stract(sec_viz_ctl%sect_psf_ctls)
      call dealloc_iso_ctl_stract(sec_viz_ctl%sect_iso_ctls)
!
      sec_viz_ctl%i_viz_only_file = 0
!
      end subroutine reset_section_control_data
!
!   --------------------------------------------------------------------
!
      end module t_control_data_section_only
