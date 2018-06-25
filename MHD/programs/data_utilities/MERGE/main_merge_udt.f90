!>@file   main_merge_udt.f90
!!@brief  program merge_udt
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in July 2014 
!
!>@brief  Main program for assemble field data
!
      program merge_udt
!
      use m_precision
!
      use calypso_mpi
      use analyzer_merge_udt
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_merge_udt
      call analyze_merge_udt
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program merge_udt
