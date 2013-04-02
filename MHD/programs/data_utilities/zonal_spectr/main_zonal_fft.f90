!main_zonal_fft.f90
!     program  equator_zonal_fft

      program equator_zonal_fft
!
!      Main routine for equatorial spectrum
!      Programmed by H. Matsui on 2009
!
      use m_precision
!
      use m_parallel_var_dof
      use analyzer_zonal_fft

      implicit none
!
      call parallel_cal_init
!
      call init_analyzer

      call analyze

      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program equator_zonal_fft
