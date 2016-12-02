!>@file   main_diff_sph_spectr.f90
!!@brief  program diff_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Take diffreence between two spectr data
!
      program diff_sph_spectr
!
      use m_precision
!
      use calypso_mpi
      use analyzer_diff_sph_spectr
!
      implicit none
!
!
      call calypso_MPI_init
!
      call evolution_diff_sph_spectr
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program diff_sph_spectr
