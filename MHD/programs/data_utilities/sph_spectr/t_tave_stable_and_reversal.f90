!>@file   t_tave_stable_and_reversal.f90
!!        module t_tave_stable_and_reversal
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Evaluate time average and standard deviation 
!!        from spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine set_control_ave_gauss(tave_svsr_ctl,                 &
!!     &                                 tave_st_rev_param)
!!        type(tave_stable_reverse_ctl), intent(in) :: tave_svsr_ctl
!!        type(tave_stable_and_reversal), intent(inout)                 &
!!     &                                 :: tave_st_rev_param
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin time_averaging_stable_rev
!!    start_time_ctl     1.0
!!    end_time_ctl       2.0
!!
!!    stable_limit_g10_ctl     7e-3
!!
!!    volume_average_file_name         'sph_ave_volume'
!!    volume_mean_square_file_name     'sph_pwr_volume_s'
!!    gauss_coefs_file_name           'sph_spectr/gauss_coefs'
!!  end time_averaging_stable_rev
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_tave_stable_and_reversal
!
      use m_precision
      use m_constants
!
      use t_ctl_data_tave_stable_rev
      use t_ctl_param_sph_series_util
      use set_parallel_file_name
!
      implicit none
!
      type tave_stable_and_reversal
        logical :: flag_old_gauss
        character(len=kchara) :: vave_file_name = 'NO_FILE'
        character(len=kchara) :: vpwr_file_name = 'NO_FILE'
        character(len=kchara) :: gauss_file_name
        real(kind = kreal) :: start_time
        real(kind = kreal) :: end_time
        real(kind = kreal) :: border_g10
      end type tave_stable_and_reversal
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_control_ave_gauss(tave_svsr_ctl,                   &
     &                                 tave_st_rev_param)
!
      use skip_comment_f
!
      type(tave_stable_reverse_ctl), intent(in) :: tave_svsr_ctl
      type(tave_stable_and_reversal), intent(inout)                     &
     &                                 :: tave_st_rev_param
!
!
      tave_st_rev_param%vave_file_name = 'NO_FILE'
      if(tave_svsr_ctl%volume_average_file_ctl%iflag .eq. 0) then
        write(*,*) 'No volume average data file'
      end if
      tave_st_rev_param%vave_file_name                                  &
     &      = tave_svsr_ctl%volume_average_file_ctl%charavalue
!
      tave_st_rev_param%vpwr_file_name = 'NO_FILE'
      if(tave_svsr_ctl%volume_power_file_ctl%iflag .eq. 0) then
        write(*,*) 'No volume mean square data file'
      end if
      tave_st_rev_param%vpwr_file_name                                  &
     &      = tave_svsr_ctl%volume_power_file_ctl%charavalue
!
      if(tave_svsr_ctl%gauss_coefs_file_ctl%iflag .eq. 0) then
        write(*,*) 'Set File prefix for Gauss coefficients'
        stop
      end if
      tave_st_rev_param%gauss_file_name                                 &
     &      = tave_svsr_ctl%gauss_coefs_file_ctl%charavalue
!
      if(tave_svsr_ctl%start_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set start time'
        stop
      end if
      tave_st_rev_param%start_time                                      &
     &     = tave_svsr_ctl%start_time_ctl%realvalue
!
      if(tave_svsr_ctl%end_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set end time'
        stop
      end if
      tave_st_rev_param%end_time                                        &
     &      = tave_svsr_ctl%end_time_ctl%realvalue
!
      if(tave_svsr_ctl%stable_limit_g10_ctl%iflag .eq. 0) then
        write(*,*) 'Set threthold for g10'
        stop
      end if
      tave_st_rev_param%border_g10                                      &
     &      = tave_svsr_ctl%stable_limit_g10_ctl%realvalue
!
      tave_st_rev_param%flag_old_gauss = .FALSE.
      if(tave_svsr_ctl%old_gauss_coefs_file_ctl%iflag .gt. 0) then
        if(yes_flag(tave_svsr_ctl%old_gauss_coefs_file_ctl%charavalue)) &
     &    tave_st_rev_param%flag_old_gauss = .TRUE.
      end if
!
      end subroutine set_control_ave_gauss
!
!   --------------------------------------------------------------------
!
      end module t_tave_stable_and_reversal
