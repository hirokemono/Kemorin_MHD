!>@file   main_sph_all_correlate.f90
!!@brief  program sph_all_correlate
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program of backward transform from spectr data output
!
      program sph_all_correlate
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sph_all_correlate
!
      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_sph_all_correlate
!
      call evolution_sph_all_correlate
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_all_correlate
