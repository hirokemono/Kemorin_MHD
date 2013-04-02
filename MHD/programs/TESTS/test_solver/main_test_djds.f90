!main_test_djds.f90
!
!     program test_djds_solver
!
!      Main routine for DJDS solver test
!      Programmed by H. Matsui on 2004
!
!-----------------------------------------------------------------------
!
      program test_djds_solver

      use m_precision
!
      use m_parallel_var_dof
      use analyzer_test_djds

      implicit none
!
!
      call parallel_cal_init
!
      call init_analyzer

      call analyze

      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program test_djds_solver
