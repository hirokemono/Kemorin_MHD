!>@file   main_sph_MHD_only.f90
!!@brief  program sph_MHD_only
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Disconnect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program for MHD dynamo simulation
!!        without visualization and snapshot routines
!
     program sph_MHD_only
!
      use m_precision
!
      use calypso_mpi
      use analyzer_small_sph_MHD
!
      implicit none
!
!>      File name for control file
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
!>      Structure of the all data of program
      type(sph_MHD_mini), save :: MHDM_m
!
!
      call calypso_MPI_init
!
      call initialize_sph_mhd_only(MHD_ctl_name, MHDM_m)
      call evolution_sph_mhd_only(MHDM_m)
!
      call calypso_MPI_finalize
!
      stop
      end program sph_MHD_only
