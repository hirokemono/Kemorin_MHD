!>@file   main_sph_MHD.f90
!!@brief  program sph_MHD
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program for MHD dynamo simulation
!!        without cross sectioning routines
!
     program sph_MHD
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sph_MHD
!
      implicit none
!
!>      File name for control file
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
!>      Structure of the all data of program
      type(sph_SGS_MHD), save :: SSMHD_m
!
!
      call calypso_MPI_init
!
      call initialize_sph_MHD(MHD_ctl_name, SSMHD_m)
      call evolution_sph_MHD(SSMHD_m)
!
      call calypso_MPI_finalize
!
      stop 'Program finished'
      end program sph_MHD
