!>@file   main_sph_snapshot.f90
!!@brief  program kemorin_sph_snapshot
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program to evaluate snapshots from spectr data
!
      program kemorin_sph_snapshot
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sph_snap
!
      implicit none
!
!>      File name for control file
      character(len=kchara), parameter                                  &
     &                      :: snap_ctl_name = 'control_snapshot'
!>      Structure of the all data of program
      type(sph_SGS_SNAP), save :: SSNAP_m
!
!
      call calypso_MPI_init
!
      call initialize_sph_snap(snap_ctl_name, SSNAP_m)
      call evolution_sph_snap(SSNAP_m)
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program kemorin_sph_snapshot
