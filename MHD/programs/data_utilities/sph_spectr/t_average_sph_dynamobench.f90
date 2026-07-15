!>@file   t_average_sph_dynamobench.f90
!!        program t_average_sph_dynamobench
!!
!! @author H. Matsui
!! @date   Programmed in July, 2026
!!
!
!> @brief Evaluate time average and standard deviation 
!!        from dynamo benchmark results
!!
!!@verbatim
!! -----------------------------------------------------------------
!!
!!      control file: control_sph_time_average
!!
!!  begin time_averaging_sph_monitor
!!    start_time_ctl     1.0
!!    end_time_ctl       2.0
!!
!!    begin monitor_data_list_ctl
!!      dynamo_benchmark_file_prefix   'dynamobench'
!!    end monitor_data_list_ctl
!!  end time_averaging_sph_monitor
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      program t_average_sph_dynamobench
!
      use m_precision
      use m_constants
!
      use t_ctl_data_tave_sph_monitor
      use t_ctl_param_sph_series_util
      use t_time_ave_sph_volume_mean
      use time_ave_sdev_sph_dbench
!
      implicit none
!
!>        Control file name
      character(len = kchara), parameter                                &
     &           :: fname_ctl_tave_sph_mtr = 'control_sph_time_average'
!
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
      type(sph_spectr_file_param), save :: spec_evo_p1
      character(len = kchara) :: dynamobench_fname
!
      call read_control_file_sph_monitor(0, fname_ctl_tave_sph_mtr,     &
     &                                   tave_sph_ctl1)
      call set_spec_series_time_param(tave_sph_ctl1, spec_evo_p1)
      call set_dynamobench_file_name(tave_sph_ctl1%monitor_list_ctl,    &
     &                                     dynamobench_fname)
      call dealloc_ctl_tave_sph_monitor(tave_sph_ctl1)
!
      call time_ave_sdev_sph_dynamobench(dynamobench_fname,             &
     &    spec_evo_p1%start_time, spec_evo_p1%end_time)
!
      call dealloc_spec_series_file_param(spec_evo_p1)
      stop
!
      end program t_average_sph_dynamobench
