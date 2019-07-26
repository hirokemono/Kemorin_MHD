!>@file   main_visualize_rayleigh.f90
!!@brief  program visualize_rayleigh
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in July 2019
!
!>@brief  Main program for assemble spectr data

      program visualize_rayleigh
!
      use m_precision
!
      use calypso_mpi
      use analyzer_viz_rayleigh

      implicit none
!
!
      call calypso_MPI_init
!
      call init_viz_rayleigh
      call analyze_viz_rayleigh
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program visualize_rayleigh
