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
      use analyzer_sph_licv
      use m_parallel_var_dof
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialize_sph_licv
!
      call evolution_sph_licv
!
      call  parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program kemorin_sph_licv
