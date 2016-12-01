!>@file   main_sph_back_transform.f90
!!@brief  program sph_back_transform
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program of backward transform from spectr data output
!
      program sph_back_transform
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sph_back_trans
!
      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_sph_back_trans
!
      call evolution_sph_back_trans
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_back_transform
