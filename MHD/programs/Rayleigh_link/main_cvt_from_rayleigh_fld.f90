!>@file   main_cvt_from_rayleigh_fld.f90
!!@brief  program assemble_rayleigh_field
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in July 2014 
!
!>@brief  Main program for assemble field data
!
      program assemble_rayleigh_field
!
      use m_precision
!
      use calypso_mpi
      use analyzer_rayleigh_cvt_fld
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_rayleigh_cvt_fld
      call analyze_rayleigh_cvt_fld
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program assemble_rayleigh_field
