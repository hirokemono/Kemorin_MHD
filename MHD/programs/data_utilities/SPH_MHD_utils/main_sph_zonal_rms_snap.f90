!>@file   main_sph_zonal_rms_snap.f90
!!@brief  program main_sph_zonal_rms_snap
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program to evaluate zonal root mean square field
!
      program main_sph_zonal_rms_snap
!
      use m_precision
!
      use m_parallel_var_dof
      use analyzer_sph_zonal_rms_snap
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialize_sph_zonal_rms_snap
      call evolution_sph_zonal_rms_snap
!
      call  parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program main_sph_zonal_rms_snap
