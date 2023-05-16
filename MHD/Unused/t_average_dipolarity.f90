!t_average_dipolarity.f90
!      program t_average_dipolarity
!
!        programmed by H.Matsui on Apr., 2014
!
!
!! -----------------------------------------------------------------
!!    Input control file:  control_sph_time_average
!!
!!  begin time_averaging_sph_monitor
!!    start_time_ctl     1.0
!!    end_time_ctl       2.0
!!
!!    begin monitor_data_list_ctl
!!      dipolarity_prefix    'dipolarity'
!!    end monitor_data_list_ctl
!!  end time_averaging_sph_monitor
!! -----------------------------------------------------------------
!

      program t_average_dipolarity
!
      use m_precision
      use m_constants
!
      use t_CMB_dipolarity
      use t_ctl_data_tave_sph_monitor
      use time_average_dipolarity
!
      implicit  none
!
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
!
      real(kind = kreal) :: start_time, end_time
      character(len = kchara) :: file_name
!
!
      call read_control_file_sph_monitor(0, tave_sph_ctl1)
!
      call set_control_tave_dipolarity                                  &
     &   (tave_sph_ctl1, file_name, start_time, end_time)
      call s_time_average_dipolarity                                    &
     &   (file_name, start_time, end_time)
!
      write(*,*) '***** program finished *****'
      stop
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_control_tave_dipolarity                            &
     &         (tave_sph_ctl, file_name, start_time, end_time)
!
      use t_ctl_data_tave_sph_monitor
      use t_ctl_param_sph_series_util
      use set_parallel_file_name
!
      implicit  none
!
      type(tave_sph_monitor_ctl), intent(in) :: tave_sph_ctl
      character(len=kchara), intent(inout) :: file_name
      real(kind = kreal), intent(inout) :: start_time, end_time
!
      character(len = kchara) :: file_prefix
!
!
      if(tave_sph_ctl%monitor_list_ctl%dipolarity_file_prefix%iflag     &
     &                                                     .eq. 0) then
        write(*,*) 'Set File prefix for dipolarity'
        stop
      end if
      file_prefix                                                       &
     & =tave_sph_ctl%monitor_list_ctl%dipolarity_file_prefix%charavalue
      call set_sph_series_file_name(dummy_item, file_prefix, file_name)
!
      if(tave_sph_ctl%start_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set start time'
        stop
      end if
      start_time = tave_sph_ctl%start_time_ctl%realvalue
!
      if(tave_sph_ctl%end_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set end time'
        stop
      end if
      end_time = tave_sph_ctl%end_time_ctl%realvalue
!
      end subroutine set_control_tave_dipolarity
!
! -------------------------------------------------------------------
!
      end program t_average_dipolarity
