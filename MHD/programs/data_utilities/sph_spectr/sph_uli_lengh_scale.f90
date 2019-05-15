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
!
      use m_sph_uli_lengh_scale
      use t_read_sph_spectra
!
      implicit none
!
!
      character(len = kchara) :: input_header
      real(kind = kreal) :: start_time, end_time
      type(read_sph_spectr_data), save :: sph_IN_u
!
!
      call select_sph_ene_spec_data_file(sph_IN_u, input_header)
!
      write(*,*) 'Input start and end time'
      read(*,*) start_time, end_time
!
!    Evaluate length scale
!
      call sph_uli_lengh_scale_by_spectr                                &
     &   (input_header, start_time, end_time, sph_IN_u)
!
      stop
      end program sph_uli_lengh_scale
