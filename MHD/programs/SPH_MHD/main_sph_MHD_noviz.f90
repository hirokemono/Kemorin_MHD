!>@file   main_sph_MHD_noviz.f90
!!@brief  program sph_MHD_no_visualizer
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program for MHD dynamo simulation
!!        without cross sectioning routines
!!         input control file:  control_MHD
!
     program sph_MHD_no_visualizer
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sph_MHD_noviz
!
      implicit none
!
!>      File name for control file
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
!>      Structure of the all data of program
      type(sph_MHD_noviz), save :: MHDN_m
!
!
      call calypso_MPI_init
!
      call initialize_sph_MHD_noviz(MHD_ctl_name, MHDN_m)
      call evolution_sph_MHD_noviz(MHDN_m)
!
      call calypso_MPI_finalize
!
      stop
      end program sph_MHD_no_visualizer
