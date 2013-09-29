!main_test_crs.f90
!
!     program  test_crs_solver
!
!      Main routine for CRS solver test
!      Programmed by H. Matsui on 2004
!
!-----------------------------------------------------------------------
!
      program test_crs_solver
!
      use m_precision
!
      use calypso_mpi
      use analyzer_test_crs

      implicit none
!
!
      call calypso_MPI_init
!
      call init_analyzer

      call analyze

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program test_crs_solver
