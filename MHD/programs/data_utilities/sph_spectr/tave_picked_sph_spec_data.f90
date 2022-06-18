!tave_picked_sph_spec_data.f90
!      program tave_picked_sph_spec_data
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
!!    picked_sph_prefix        'picked_mode'
!!  end time_averaging_sph_monitor
!! -----------------------------------------------------------------
!
      program tave_picked_sph_spec_data
!
      use m_precision
      use m_constants
!
      use t_ctl_data_tave_sph_monitor
      use time_ave_picked_sph_spectr
!
      implicit  none
!
!>      Structure for control data
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
!
      character(len=kchara) :: file_name
      real(kind = kreal) :: start_time, end_time
!
!
      call read_control_file_sph_monitor(0, tave_sph_ctl1)
      call set_ctl_tave_picked_sph_spectr                               &
     &   (tave_sph_ctl1, file_name, start_time, end_time)
      call s_time_ave_picked_sph_spectr                                 &
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
      subroutine set_ctl_tave_picked_sph_spectr                         &
     &         (tave_sph_ctl, file_name, start_time, end_time)
!
      use set_parallel_file_name
!
      type(tave_sph_monitor_ctl), intent(in) :: tave_sph_ctl
      character(len=kchara), intent(inout) :: file_name
      real(kind = kreal), intent(inout) :: start_time, end_time
!
      character(len=kchara) :: evo_header
!
!
      if(tave_sph_ctl%picked_mode_head_ctl%iflag .eq. 0) then
        write(*,*) 'Set File prefix for Gauss coefficients'
        stop
      end if
      evo_header = tave_sph_ctl%picked_mode_head_ctl%charavalue
      file_name = add_dat_extension(evo_header)
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
      end subroutine set_ctl_tave_picked_sph_spectr
!
! -------------------------------------------------------------------
!
      end program tave_picked_sph_spec_data
