!>@file   main_sph_snapshot_noviz.f90
!!@brief  program sph_snap_noviz
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program to evaluate snapshots from spectr data
!!        without visualization routines
!!         Input control file: control_snapshot
!
      program sph_snap_noviz
!
      use m_precision
!
      use calypso_mpi
      use analyzer_noviz_sph_snap
!
      implicit none
!
!>      File name for control file
      character(len=kchara), parameter                                  &
     &                      :: snap_ctl_name = 'control_snapshot'
!
!
      call calypso_MPI_init
!
      call initialize_noviz_sph_snap(snap_ctl_name)
      call evolution_noviz_sph_snap
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_snap_noviz
