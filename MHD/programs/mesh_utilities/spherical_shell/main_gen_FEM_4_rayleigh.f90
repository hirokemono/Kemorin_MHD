!>@file   main_gen_FEM_4_rayleigh.f90
!!@brief  program main_gen_FEM_4_rayleigh
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in Feb., 2015 
!
!>@brief  Main program generate spherical harmonics indices
!
      program main_gen_FEM_4_rayleigh
!
      use m_precision
!
      use calypso_mpi
      use analyzer_gen_FEM_4_rayleigh
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_gen_FEM_rayleigh
      call analyze_gen_sph_grids
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program main_gen_FEM_4_rayleigh
