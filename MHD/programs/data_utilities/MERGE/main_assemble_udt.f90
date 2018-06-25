!>@file   main_assemble_udt.f90
!!@brief  program assemble_udt
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in July 2014 
!
!>@brief  Main program for assemble field data
!
      program assemble_udt
!
      use m_precision
!
      use calypso_mpi
      use analyzer_assemble_udt
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_assemble_udt
      call analyze_assemble_udt
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program assemble_udt
