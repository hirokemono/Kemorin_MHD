!>@file   main_assemble_rst.f90
!!@brief  program assemble_restart
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in July 2014 
!
!>@brief  Main program for assemble field data
!
      program assemble_restart
!
      use m_precision
!
      use calypso_mpi
      use analyzer_assemble_rst
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_assemble_rst
      call analyze_assemble_rst
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program assemble_restart
