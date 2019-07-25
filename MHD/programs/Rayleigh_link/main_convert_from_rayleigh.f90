!>@file   main_convert_from_rayleigh.f90
!!@brief  program convert_from_rayleigh
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in July 2014 
!
!>@brief  Main program for assemble spectr data
!
      program convert_from_rayleigh
!
      use m_precision
!
      use calypso_mpi
      use analyzer_rayleigh_convert
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_cvt_rayleigh
      call analyze_cvt_rayleigh
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program convert_from_rayleigh
