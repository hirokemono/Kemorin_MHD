!tave_picked_gauss_coefs.f90
!
!        programmed by H.Matsui on Dec., 2012
!
!! -----------------------------------------------------------------
!!    Input control file:  control_sph_time_average
!!
!!  begin time_averaging_sph_monitor
!!    start_time_ctl     1.0
!!    end_time_ctl       2.0
!!
!!    array vol_integrate_prefix
!!      gauss_coefs_prefix        'gauss_coefs_Re'
!!    end monitor_data_list_ctl
!!  end time_averaging_sph_monitor
!! -----------------------------------------------------------------
!
      program tave_picked_gauss_coefs
!
      use m_precision
      use m_constants
!
      use t_ctl_data_tave_sph_monitor
      use time_average_gauss_coefs
!
      implicit  none
!
!>      Structure for control data
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
!
      character(len=kchara) :: input_file_name
      real(kind = kreal) :: start_time, end_time
!
!
      call read_control_file_sph_monitor(0, tave_sph_ctl1)
      call set_control_ave_gauss(tave_sph_ctl1, input_file_name,        &
     &                           start_time, end_time)
!
      call s_time_average_gauss_coefs                                   &
     &   (.TRUE., input_file_name, start_time, end_time)
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
      subroutine set_control_ave_gauss(tave_sph_ctl,                    &
     &          input_file_name, start_time, end_time)
!
      use t_ctl_param_sph_series_util
!
      type(tave_sph_monitor_ctl), intent(in) :: tave_sph_ctl
      character(len=kchara), intent(inout) :: input_file_name
      real(kind = kreal), intent(inout) :: start_time, end_time
!
      character(len=kchara) :: file_prefix
!
      if(tave_sph_ctl%monitor_list_ctl%gauss_coefs_prefix%iflag         &
     &                                                 .eq. 0) then
        write(*,*) 'Set File prefix for Gauss coefficients'
        stop
      end if
      call set_sph_series_file_name                                     &
     &   (dummy_item, tave_sph_ctl%read_mnt_file_fmt_ctl,               &
     &    tave_sph_ctl%monitor_list_ctl%gauss_coefs_prefix%charavalue,  &
     &    input_file_name)
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
      end subroutine set_control_ave_gauss
!
! -------------------------------------------------------------------
!
      end program tave_picked_gauss_coefs
