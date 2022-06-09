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
!!    dipolarity_prefix    'dipolarity'
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
!
      implicit  none
!
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
      type(dipolarity_data), save :: dip_t
!
      real(kind = kreal) :: start_time, end_time
      character(len = kchara) :: file_prefix
!
!
      call read_control_file_sph_monitor(0, tave_sph_ctl1)
!
      call set_control_tave_dipolarity                                  &
     &   (dip_t%dipolarity_prefix, file_prefix, start_time, end_time)
      call time_average_dipolarity_f                              &
     &   (file_prefix, start_time, end_time)
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
     &         (tave_sph_ctl, file_prefix, start_time, end_time)
!
      use t_ctl_data_tave_sph_monitor
!
      implicit  none
!
      type(tave_sph_monitor_ctl), intent(in) :: tave_sph_ctl
      character(len=kchara), intent(inout) :: file_prefix
      real(kind = kreal), intent(inout) :: start_time, end_time
!
      if(tave_sph_ctl%dipolarity_file_prefix%iflag .eq. 0) then
        write(*,*) 'Set File prefix for dipolarity'
        stop
      end if
      file_prefix = tave_sph_ctl%dipolarity_file_prefix%charavalue
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
      end subroutine set_control_tave_Nu
!
! -------------------------------------------------------------------
!
      end program t_average_dipolarity
