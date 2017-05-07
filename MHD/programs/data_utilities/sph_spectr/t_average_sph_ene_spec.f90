!>@file   t_average_sph_ene_spec.f90
!!        program t_average_sph_ene_spec
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Evaluate time average and standard deviation 
!!        from spherical harmonics spectrum data
!
      program t_average_sph_ene_spec
!
      use m_precision
!
      use m_tave_sph_ene_spectr
      use m_sph_ene_spectra
      use cal_tave_sph_ene_spectr
      use set_parallel_file_name
!
      implicit none
!
!
      integer(kind = kint) :: iflag_data_mode
      character(len = kchara) :: input_header
      real(kind = kreal) :: start_time, end_time
!
!
!
      write(*,*) ' Choose data to take average'
      write(*,*)  ' 0: Total and all spectrum  '
      write(*,*)  ' 1: Total average '
      write(*,*)  ' 2: One spectrum data '
      read(*,*) iflag_data_mode
!
      call select_sph_ene_spec_data_file                                &
     &   (sph_IN1%iflag_vol_ave, sph_IN1%iflag_old_fmt, input_header)
!
      if(iflag_data_mode .eq. 0) then
        call set_org_ene_spec_file_name(input_header)
      else if(iflag_data_mode .eq. 1) then
        call add_dat_extension(input_header, fname_org_rms)
      else if(iflag_data_mode .eq. 2) then
        call add_dat_extension(input_header, fname_org_rms_l)
      end if
!
      write(*,*) 'Input start and end time'
      read(*,*) start_time, end_time
!
!    Evaluate time average
!
      sph_IN1%iflag_spectr = 0
      if(iflag_data_mode .eq. 0 .or. iflag_data_mode .eq. 1) then
        call sph_spectr_average(fname_org_rms, start_time, end_time)
        call sph_spectr_std_deviation                                   &
     &     (fname_org_rms, start_time, end_time)
      end if
!
      sph_IN1%iflag_spectr = 1
      if(iflag_data_mode .eq. 0 .or. iflag_data_mode .eq. 2) then
        call sph_spectr_average(fname_org_rms_l, start_time, end_time)
        call sph_spectr_std_deviation                                   &
     &     (fname_org_rms_l, start_time, end_time)
      end if
!
      if(iflag_data_mode .eq. 0) then
        call sph_spectr_average(fname_org_rms_m, start_time, end_time)
        call sph_spectr_std_deviation                                   &
     &     (fname_org_rms_m, start_time, end_time)
!
        call sph_spectr_average(fname_org_rms_lm, start_time, end_time)
        call sph_spectr_std_deviation                                   &
     &     (fname_org_rms_lm, start_time, end_time)
      end if
!
      stop
!
      end program t_average_sph_ene_spec
