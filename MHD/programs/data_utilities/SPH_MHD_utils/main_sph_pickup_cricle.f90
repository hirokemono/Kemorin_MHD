!>@file   main_sph_pickup_cricle.f90
!!@brief  program sph_pickup_cricle
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in June, 2013 (ver 2.0)
!
!> @brief Main program to pickup field on circle at (s,z)
!
      program sph_pickup_cricle
!
      use m_precision
!
      use analyzer_sph_pickup_circle
      use m_parallel_var_dof
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialize_sph_pick_circle
!
      call evolution_sph_pick_circle
!
      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_pickup_cricle
