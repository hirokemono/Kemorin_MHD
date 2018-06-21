!>@file   main_update_sph.f90
!!@brief  program update_sph_rst
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in July 2014 
!
!>@brief  Main program for assemble spectr data
!
      program update_sph_rst
!
      use m_precision
!
      use calypso_mpi
      use analyzer_update_sph_rst
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_update_sph_rst
      call analyze_update_sph_rst
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program update_sph_rst
