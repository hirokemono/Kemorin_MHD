!>@file   main_sph_all_rms_ratio.f90
!!@brief  program sph_all_rms_ratio
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program of backward transform from spectr data output
!
      program sph_all_rms_ratio
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sph_all_rms_ratio
!
      implicit none
!
      character(len=kchara), parameter                                  &
     &                      :: ratio_ctl_name = 'control_sph_rms_ratio'
!
!
      call calypso_MPI_init
!
      call initialize_sph_all_rms_ratio(ratio_ctl_name)
      call evolution_sph_all_rms_ratio
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_all_rms_ratio
