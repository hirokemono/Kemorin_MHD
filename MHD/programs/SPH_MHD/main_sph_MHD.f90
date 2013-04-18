!>@file   main_sph_MHD.f90
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
      use m_parallel_var_dof
      use analyzer_sph_MHD
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialize_sph_mhd
!
      call evolution_sph_mhd
!
      call parallel_cal_fin
!
      stop
      end program kemorin_sph_MHD
