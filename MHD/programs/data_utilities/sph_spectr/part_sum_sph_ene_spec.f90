!>@file   part_sum_sph_ene_spec.f90
!!        program part_sum_sph_ene_spec
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Evaluate sum of mean square
!!        from spherical harmonics spectrum data
!
      program part_sum_sph_ene_spec
!
      use m_precision
      use m_constants
!
      use m_part_sum_sph_ene_spectr
      use t_read_sph_spectra
      use t_ctl_data_tave_sph_monitor
      use t_ctl_param_sph_series_util
!
      implicit none
!
!
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
      type(sph_spectr_file_param), save :: spec_evo_p1
      type(read_sph_spectr_data), save :: sph_IN_p
!
      integer :: i
!
      call read_control_file_sph_monitor(0, tave_sph_ctl1)
      if(tave_sph_ctl1%degree_range_ctl%iflag .eq. 0) then
        write(*,*) 'Set spharical harmonics range'
        stop
      end if
!
      call set_spec_series_file_param(tave_sph_ctl1, spec_evo_p1)
      call dealloc_ctl_tave_sph_monitor(tave_sph_ctl1)
!
      do i = 1, spec_evo_p1%nfile_vol_spectr_file
        call sph_part_pwr_spectr_sum                                    &
     &     (spec_evo_p1%vol_spectr_prefix(i),                           &
     &      ione, spec_evo_p1, sph_IN_p)
      end do
!
      do i = 1, spec_evo_p1%nfile_layer_sprctr_file
        call sph_part_pwr_spectr_sum                                    &
     &     (spec_evo_p1%layer_spectr_prefix(i),                         &
     &      izero, spec_evo_p1, sph_IN_p)
      end do
!
      call dealloc_spec_series_file_param(spec_evo_p1)
      stop
!
      end program part_sum_sph_ene_spec
