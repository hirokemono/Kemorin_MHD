!>@file   maxmode_sph_ene_spec.f90
!!        program maxmode_sph_ene_spec
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Find maximum degree and order of the spectrum
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
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
      type(sph_spectr_file_param), save :: spec_evo_p1
      type(read_sph_spectr_data), save :: sph_IN_m
!
      integer :: i
!
      call read_control_file_psf_compare(0, tave_sph_ctl1)
      call set_spec_series_file_param(tave_sph_ctl1, spec_evo_p1)
      call dealloc_ctl_tave_sph_monitor(tave_sph_ctl1)
!
      do i = 1, spec_evo_p1%nfile_vol_spectr_file
        call sph_maximum_pwr_spectr(spec_evo_p1%vol_spectr_prefix(i),   &
     &      ione, spec_evo_p1, sph_IN_m)
      end do
!
      do i = 1, spec_evo_p1%nfile_layer_sprctr_file
        call sph_maximum_pwr_spectr(spec_evo_p1%layer_spectr_prefix(i), &
     &      izero, spec_evo_p1, sph_IN_m)
      end do
!
      call dealloc_spec_series_file_param(spec_evo_p1)
      stop
!
      end program maxmode_sph_ene_spec
