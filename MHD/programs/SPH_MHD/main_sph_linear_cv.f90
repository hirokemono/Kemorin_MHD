!>@file   main_sph_linear_cv.f90
!!@brief  program kemorin_sph_licv
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program for linear convection model in spherical shell
!
      program kemorin_sph_licv
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sph_licv
!
      implicit none
!
!>      File name for control file
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
!>      Structure of the all data of program
      type(sph_linear_convection), save :: LICV_m
!
!
      call calypso_MPI_init
!
      call initialize_sph_licv(MHD_ctl_name, LICV_m)
      call evolution_sph_licv(LICV_m)
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program kemorin_sph_licv
