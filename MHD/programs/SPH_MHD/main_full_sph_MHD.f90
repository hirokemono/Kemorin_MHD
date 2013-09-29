!>@file   main_full_sph_MHD.f90
!!@brief  program kemorin_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program for MHD dynamo simulation
!
      program kemorin_sph_MHD
!
      use m_precision
!
      use calypso_mpi
      use analyzer_full_sph_MHD
!
      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_full_sph_mhd
!
      call evolution_full_sph_mhd
!
      call calypso_MPI_finalize
!
      stop
      end program kemorin_sph_MHD
