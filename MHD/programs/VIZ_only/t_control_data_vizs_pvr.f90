!t_control_data_vizs_pvr.f90
!      module t_control_data_vizs_pvr
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine read_control_file_pvr_vizs(pvr_vizs_c)
!!      subroutine dealloc_pvr_vizs_control_data(pvr_vizs_c)
!!        type(control_data_pvr_vizs), intent(inout) :: pvr_vizs_c
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
!
      module t_control_data_vizs_pvr
!
      use m_precision
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
      use t_control_data_vizs
      use t_control_array_character3
!
      implicit  none
!
!
      integer(kind = kint), parameter :: viz_ctl_file_code = 11
      character(len = kchara), parameter :: fname_viz_ctl = "ctl_viz"
!
!>      Structure for visulization program
      type control_data_pvr_vizs
!>        Structure for file settings
        type(platform_data_control) :: viz_plt
!>        Structure for time stepping control
        type(time_data_control) :: t_viz_ctl
!
!>        Structures of visualization controls
        type(visualization_controls) :: viz_ctl_v
!
!>        Structures of field used in visualization
        type(ctl_array_c3) :: viz_field_ctl
!
        integer(kind=kint) :: i_viz_only_file = 0
      end type control_data_pvr_vizs
!
!     Top level
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_only_file = 'visualizer'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_time_step = 'time_step_ctl'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_partition = 'viz_repartition_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_control = 'visual_control'
!
      private :: viz_ctl_file_code, fname_viz_ctl
      private :: read_pvr_vizs_control_data
      private :: bcast_pvr_vizs_control_data
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_file_pvr_vizs(pvr_vizs_c)
!
      use skip_comment_f
      use viz_step_ctls_to_time_ctl
!
      type(control_data_pvr_vizs), intent(inout) :: pvr_vizs_c
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open (viz_ctl_file_code, file=fname_viz_ctl, status='old' )
        do
          call load_one_line_from_control(viz_ctl_file_code, c_buf1)
          call read_pvr_vizs_control_data                               &
     &       (viz_ctl_file_code, hd_viz_only_file, pvr_vizs_c, c_buf1)
          if(pvr_vizs_c%i_viz_only_file .gt. 0) exit
        end do
        close(viz_ctl_file_code)
!
        call s_viz_step_ctls_to_time_ctl                                &
     &     (pvr_vizs_c%viz_ctl_v, pvr_vizs_c%t_viz_ctl)
!
        pvr_vizs_c%viz_field_ctl%num =  0
        call alloc_control_array_c3(pvr_vizs_c%viz_field_ctl)
        call add_fields_4_vizs_to_fld_ctl(pvr_vizs_c%viz_ctl_v,         &
     &                                    pvr_vizs_c%viz_field_ctl)
      end if
!
      call bcast_pvr_vizs_control_data(pvr_vizs_c)
!
      end subroutine read_control_file_pvr_vizs
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_pvr_vizs_control_data                             &
     &         (id_control, hd_block, pvr_vizs_c, c_buf)
!
      use skip_comment_f
      use read_viz_controls
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_pvr_vizs), intent(inout) :: pvr_vizs_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pvr_vizs_c%i_viz_only_file .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, pvr_vizs_c%viz_plt, c_buf)
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, pvr_vizs_c%t_viz_ctl, c_buf)
!
        call s_read_viz_controls(id_control, hd_viz_control,            &
     &                           pvr_vizs_c%viz_ctl_v, c_buf)
      end do
      pvr_vizs_c%i_viz_only_file = 1
!
      end subroutine read_pvr_vizs_control_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_pvr_vizs_control_data(pvr_vizs_c)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_time_step_ctl
      use bcast_control_arrays
!
      type(control_data_pvr_vizs), intent(inout) :: pvr_vizs_c
!
!
      call bcast_ctl_array_c3(pvr_vizs_c%viz_field_ctl)
      call bcast_ctl_data_4_platform(pvr_vizs_c%viz_plt)
      call bcast_ctl_data_4_time_step(pvr_vizs_c%t_viz_ctl)
!
      call bcast_viz_controls(pvr_vizs_c%viz_ctl_v)
!
      call calypso_mpi_bcast_one_int(pvr_vizs_c%i_viz_only_file, 0)
!
      end subroutine bcast_pvr_vizs_control_data
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_pvr_vizs_control_data(pvr_vizs_c)
!
      use bcast_4_time_step_ctl
!
      type(control_data_pvr_vizs), intent(inout) :: pvr_vizs_c
!
      call dealloc_control_array_c3(pvr_vizs_c%viz_field_ctl)
      call reset_control_platforms(pvr_vizs_c%viz_plt)
      call dealloc_viz_controls(pvr_vizs_c%viz_ctl_v)
      call reset_ctl_data_4_time_step(pvr_vizs_c%t_viz_ctl)
!
      pvr_vizs_c%t_viz_ctl%i_tstep = 0
      pvr_vizs_c%i_viz_only_file =   0
!
      end subroutine dealloc_pvr_vizs_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      end module t_control_data_vizs_pvr
