!>@file   part_sum_sph_ene_spec.f90
!!        program part_sum_sph_ene_spec
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Evaluate time average and standard deviation 
!!        from spherical harmonics spectrum data
!
      program part_sum_sph_ene_spec
!
      use m_precision
!
      use m_part_sum_sph_ene_spectr
      use t_read_sph_spectra
!
      implicit none
!
!
      character(len = kchara) :: input_header
      real(kind = kreal) :: start_time, end_time
      integer(kind = kint) :: lst, led
      type(read_sph_spectr_data), save :: sph_IN_p
!
!
!
      call select_sph_ene_spec_data_file(sph_IN_p, input_header)
!
      write(*,*) 'Input start and end degree or order'
      read(*,*) lst, led
!
      write(*,*) 'Input start and end time'
      read(*,*) start_time, end_time
!
!    Evaluate time average
!
      call sph_part_pwr_spectr_sum                                      &
     &   (input_header, start_time, end_time, lst, led, sph_IN_p)
!
      stop
!
      end program part_sum_sph_ene_spec
