!>@file   main_update_restart.f90
!!@brief  program update_restart
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in July 2014 
!
!>@brief  Main program for assemble field data
!
      program update_restart
!
      use m_precision
!
      use calypso_mpi
      use analyzer_update_rst
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_update_retstart
      call analyze_update_restart
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program update_restart
