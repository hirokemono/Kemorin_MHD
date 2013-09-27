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
      use calypso_mpi
      use analyzer_zonal_fft

      implicit none
!
      call parallel_cal_init
!
      call init_analyzer

      call analyze

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program equator_zonal_fft
