!>@file   sph_uli_lengh_scale.f90
!!        program sph_uli_lengh_scale
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Find maximum degree and order of the spectrum
!
      program sph_uli_lengh_scale
!
      use m_precision
      use m_constants
!
      use m_sph_uli_lengh_scale
      use t_read_sph_spectra
      use t_ctl_data_tave_sph_monitor
      use t_ctl_param_sph_series_util
!
      implicit none
!
!
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
      type(sph_spectr_file_param), save :: spec_evo_p1
      type(read_sph_spectr_data), save :: sph_IN_u
!
      integer :: i
!
      call read_control_file_sph_monitor(0, tave_sph_ctl1)
      call set_spec_series_file_and_time(tave_sph_ctl1, spec_evo_p1)
      call dealloc_ctl_tave_sph_monitor(tave_sph_ctl1)
!
      do i = 1, spec_evo_p1%vol_spec_series%num_file
        call sph_uli_lengh_scale_by_spectr                              &
     &     (spec_evo_p1%vol_spec_series%evo_file_name(i),               &
     &      .TRUE., spec_evo_p1, sph_IN_u)
      end do
!
      do i = 1, spec_evo_p1%layer_spec_series%num_file
        call sph_uli_lengh_scale_by_spectr                              &
     &     (spec_evo_p1%layer_spec_series%evo_file_name(i),             &
     &      .FALSE., spec_evo_p1, sph_IN_u)
      end do
!
      call dealloc_spec_series_file_param(spec_evo_p1)
      stop
!
      end program sph_uli_lengh_scale
