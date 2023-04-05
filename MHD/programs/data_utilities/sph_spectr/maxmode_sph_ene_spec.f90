!>@file   maxmode_sph_ene_spec.f90
!!        program maxmode_sph_ene_spec
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Find maximum degree and order of the spectrum
!!@verbatim
!! -----------------------------------------------------------------
!!
!!      control file: control_sph_maxmode
!!
!!  begin time_averaging_sph_monitor
!!    start_time_ctl     1.0
!!    end_time_ctl       2.0
!!
!!    old_format_flag     'Off'
!!    degree_range_ctl     1   12
!!
!!    begin monitor_data_list_ctl
!!      array vol_spectr_prefix
!!        vol_spectr_prefix     'sph_pwr_volume_l'
!!        vol_spectr_prefix     'sph_pwr_volume_m'
!!        vol_spectr_prefix     'sph_pwr_volume_lm'
!!      end array vol_spectr_prefix
!!
!!      array layer_sph_spectr_prefix
!!        layer_sph_spectr_prefix     'sph_pwr_layer_l'
!!        layer_sph_spectr_prefix     'sph_pwr_layer_m'
!!        layer_sph_spectr_prefix     'sph_pwr_layer_lm'
!!      end array layer_sph_spectr_prefix
!!    end monitor_data_list_ctl
!!  end time_averaging_sph_monitor
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      program maxmode_sph_ene_spec
!
      use m_precision
      use m_constants
!
      use m_maxmode_sph_ene_spectr
      use t_read_sph_spectra
      use t_ctl_data_tave_sph_monitor
      use t_ctl_param_sph_series_util
      use set_parallel_file_name
!
      implicit none
!
!>        Control file name
      character(len = kchara), parameter                                &
     &           :: fname_ctl_sph_maxmode = 'control_sph_maxmode'
!
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
      type(sph_spectr_file_param), save :: spec_evo_p1
      type(read_sph_spectr_data), save :: sph_IN_m
!
      integer :: i
!
      call read_control_file_sph_monitor(0, fname_ctl_sph_maxmode,      &
     &                                   tave_sph_ctl1)
      call set_spec_series_file_and_time(tave_sph_ctl1, spec_evo_p1)
      call dealloc_ctl_tave_sph_monitor(tave_sph_ctl1)
!
      do i = 1, spec_evo_p1%vol_spec_series%num_file
        call sph_maximum_volume_spectr                                  &
     &     (spec_evo_p1%vol_spec_series%evo_file_name(i),               &
     &      spec_evo_p1, sph_IN_m)
      end do
!
      do i = 1, spec_evo_p1%layer_spec_series%num_file
        call sph_maximum_layer_spectr                                   &
     &     (spec_evo_p1%layer_spec_series%evo_file_name(i),             &
     &      spec_evo_p1, sph_IN_m)
      end do
!
      call dealloc_spec_series_file_param(spec_evo_p1)
      stop
!
      end program maxmode_sph_ene_spec
