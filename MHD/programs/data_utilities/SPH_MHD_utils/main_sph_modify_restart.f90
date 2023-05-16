!>@file   main_sph_modify_restart.f90
!!@brief  program sph_modify_restart
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program to modify initial field data
!
      program sph_modify_restart
!
      use m_precision
!
      use calypso_mpi
      use analyzer_noviz_sph_snap
      use analyzer_sph_modify_restart
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
      call initialize_sph_mod_restart(snap_ctl_name)
      call evolution_sph_mod_restart
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_modify_restart
