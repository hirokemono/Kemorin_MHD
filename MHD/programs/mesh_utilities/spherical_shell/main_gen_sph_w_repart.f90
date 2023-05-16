!>@file   main_gen_sph_w_repart.f90
!!@brief  program gen_sph_grids_w_repart
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in Feb., 2015 
!
!>@brief  Main program generate spherical harmonics indices
!
      program gen_sph_grids_w_repart
!
      use m_precision
!
      use calypso_mpi
      use analyzer_gen_sph_w_repart
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_gen_sph_grids_w_repart
      call analyze_gen_sph_grids_w_repart
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program gen_sph_grids_w_repart
