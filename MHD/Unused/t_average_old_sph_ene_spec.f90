!>@file   t_average_old_sph_ene_spec.f90
!!        program t_average_old_sph_ene_spec
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Evaluate time average and standard deviation 
!!        from spherical harmonics spectrum data
!!
!!@verbatim
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin time_averaging_sph_monitor
!!    start_time_ctl     1.0
!!    end_time_ctl       2.0
!!
!!    array vol_integrate_prefix
!!      vol_integrate_prefix     'sph_ave_volume'
!!      vol_integrate_prefix     'sph_pwr_volume_s'
!!      vol_integrate_prefix     'sph_pwr_volume_m0'
!!    end array vol_integrate_prefix
!!
!!    array vol_spectr_prefix
!!      vol_spectr_prefix     'sph_pwr_volume_l'
!!      vol_spectr_prefix     'sph_pwr_volume_m'
!!      vol_spectr_prefix     'sph_pwr_volume_lm'
!!    end array vol_spectr_prefix
!!
!!    array sph_integrate_prefix
!!      sph_integrate_prefix     'sph_pwr_layer_s'
!!      sph_integrate_prefix     'sph_pwr_layer_m0'
!!    end array sph_integrate_prefix
!!
!!    array layer_sph_spectr_prefix
!!      layer_sph_spectr_prefix     'sph_pwr_layer_l'
!!      layer_sph_spectr_prefix     'sph_pwr_layer_m'
!!      layer_sph_spectr_prefix     'sph_pwr_layer_lm'
!!    end array layer_sph_spectr_prefix
!!  end time_averaging_sph_monitor
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      program t_average_old_sph_ene_spec
!
      use m_precision
      use m_constants
!
      use m_tave_sph_ene_spectr
      use t_ctl_data_tave_sph_monitor
      use t_ctl_param_sph_series_util
      use set_parallel_file_name
!
      implicit none
!
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
      type(sph_spectr_file_param), save :: spec_evo_p1
!
      integer :: i
!
      call read_control_file_sph_monitor(0, tave_sph_ctl1)
      call set_spec_series_file_and_time(tave_sph_ctl1, spec_evo_p1)
      call dealloc_ctl_tave_sph_monitor(tave_sph_ctl1)
!
      do i = 1, spec_evo_p1%vol_series%num_file
        call time_ave_sdev_sph_old_spectr                               &
     &     (spec_evo_p1%vol_series%evo_file_name(i),                    &
     &      spectr_off, volume_on,                                      &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time)
      end do
!
      do i = 1, spec_evo_p1%vol_spec_series%num_file
        call time_ave_sdev_sph_old_spectr                               &
     &     (spec_evo_p1%vol_spec_series%evo_file_name(i),               &
     &      spectr_on, volume_on,                                       &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time)
      end do
!
!
      do i = 1, spec_evo_p1%layer_series%num_file
        call time_ave_sdev_sph_old_spectr                               &
     &     (spec_evo_p1%layer_series%evo_file_name(i),                  &
     &      spectr_off, volume_off,                                     &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time)
      end do
!
      do i = 1, spec_evo_p1%layer_spec_series%num_file
        call time_ave_sdev_sph_old_spectr                               &
     &     (spec_evo_p1%layer_spec_series%evo_file_name(i),             &
     &      spectr_on, volume_off,                                      &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time)
      end do
!
      call dealloc_spec_series_file_param(spec_evo_p1)
      stop
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine time_ave_sdev_sph_old_spectr                           &
     &         (fname_org, flag_spectr, flag_vol_ave,                   &
     &          start_time, end_time)
!
      use t_read_sph_spectra
      use m_tave_sph_ene_spectr
!
      character(len = kchara), intent(in) :: fname_org
      logical, intent(in) :: flag_spectr, flag_vol_ave
      real(kind = kreal), intent(in) :: start_time, end_time
!
      type(read_sph_spectr_data), save :: sph_IN1
      type(spectr_ave_sigma_work), save :: WK_tave1
!
      real(kind = kreal) :: true_start, true_end
      logical, parameter :: flag_old_format =     .TRUE.
!
      call sph_spectr_average                                           &
     &   (flag_old_format, fname_org, flag_spectr, flag_vol_ave,        &
     &    start_time, end_time, true_start, true_end,                   &
     &    sph_IN1, WK_tave1)
      call sph_spectr_std_deviation                                     &
     &   (flag_old_format, fname_org, flag_spectr, flag_vol_ave,        &
     &    start_time, end_time, sph_IN1, WK_tave1)
!
      call dealloc_tave_sph_data(WK_tave1)
      call dealloc_sph_espec_data(sph_IN1)
      call dealloc_sph_espec_name(sph_IN1)
!
      end subroutine time_ave_sdev_sph_old_spectr
!
!   --------------------------------------------------------------------
!
      end program t_average_old_sph_ene_spec
