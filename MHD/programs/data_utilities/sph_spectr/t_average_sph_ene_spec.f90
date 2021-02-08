!>@file   t_average_sph_ene_spec.f90
!!        program t_average_sph_ene_spec
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
!!    nusselt_number_prefix        'Nusselt'
!!
!!    array volume_average_prefix
!!      volume_average_prefix        'sph_ave_volume'
!!    end array volume_spectrum_ctl
!!
!!    begin volume_pwr_spectr_prefix
!!      volume_pwr_spectr_prefix     'sph_pwr_volume'
!!    end   volume_pwr_spectr_prefix
!!
!!    layered_pwr_spectr_prefix    'sph_pwr_layer'
!!    gauss_coefs_prefix           'sph_spectr/gauss_coefs'
!!    picked_sph_prefix            'sph_spectr/picked_mode'
!!
!!    degree_spectr_switch         'On'
!!    order_spectr_switch          'On'
!!    diff_lm_spectr_switch        'On'
!!    axisymmetric_spectr_switch   'On'
!!  end time_averaging_sph_monitor
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      program t_average_sph_ene_spec
!
      use m_precision
!
      use m_tave_sph_ene_spectr
      use t_read_sph_spectra
      use t_ctl_data_tave_sph_monitor
      use set_parallel_file_name
!
      implicit none
!
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
      type(sph_spectr_file_param), save :: spec_evo_p1
      type(read_sph_spectr_data), save :: sph_IN_t
!
      character(len = kchara) :: fname_org_rms
!
      integer :: i
!
      call read_control_file_psf_compare(0, tave_sph_ctl1)
      call set_spec_series_file_param(tave_sph_ctl1, spec_evo_p1)
      call dealloc_ctl_tave_sph_monitor(tave_sph_ctl1)
!
      sph_IN_t%iflag_old_fmt = 0
      sph_IN_t%iflag_spectr = 0
      sph_IN_t%iflag_vol_ave = 1
      do i = 1, spec_evo_p1%nfile_vol_series_file
        write(*,*) i, trim(spec_evo_p1%vol_series_prefix(i))
        fname_org_rms                                                   &
     &      = add_dat_extension(spec_evo_p1%vol_series_prefix(i))
        call sph_spectr_average(fname_org_rms,                          &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time, sph_IN_t)
        call sph_spectr_std_deviation(fname_org_rms,                    &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time, sph_IN_t)
      end do
!
      sph_IN_t%iflag_spectr = 1
      sph_IN_t%iflag_vol_ave = 1
      do i = 1, spec_evo_p1%nfile_vol_spectr_file
        write(*,*) i, trim(spec_evo_p1%vol_spectr_prefix(i))
        fname_org_rms                                                   &
     &      = add_dat_extension(spec_evo_p1%vol_spectr_prefix(i))
        call sph_spectr_average(fname_org_rms,                          &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time, sph_IN_t)
        call sph_spectr_std_deviation(fname_org_rms,                    &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time, sph_IN_t)
      end do
!
!
      sph_IN_t%iflag_spectr = 0
      sph_IN_t%iflag_vol_ave = 0
      do i = 1, spec_evo_p1%nfile_layer_series_file
        write(*,*) i, trim(spec_evo_p1%layer_series_prefix(i))
        fname_org_rms                                                   &
     &      = add_dat_extension(spec_evo_p1%layer_series_prefix(i))
        call sph_spectr_average(fname_org_rms,                          &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time, sph_IN_t)
        call sph_spectr_std_deviation(fname_org_rms,                    &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time, sph_IN_t)
      end do
!
      sph_IN_t%iflag_spectr = 1
      sph_IN_t%iflag_vol_ave = 0
      do i = 1, spec_evo_p1%nfile_layer_sprctr_file
        write(*,*) i, trim(spec_evo_p1%layer_spectr_prefix(i))
        fname_org_rms                                                   &
     &      = add_dat_extension(spec_evo_p1%layer_spectr_prefix(i))
        call sph_spectr_average(fname_org_rms,                          &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time, sph_IN_t)
        call sph_spectr_std_deviation(fname_org_rms,                    &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time, sph_IN_t)
      end do
!
      call dealloc_spec_series_file_param(spec_evo_p1)
      stop
!
      end program t_average_sph_ene_spec
