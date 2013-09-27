!main_udt_correlation.f90
!
!     program  correlation_udt
!
!      Main routine for taking correlation of field data
!      Programmed by H. Matsui on 2009
!
!-----------------------------------------------------------------------
!
      program correlation_udt
!
      use m_precision
!
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_udt_correlation

      implicit none
!
!
      call parallel_cal_init
!
      call initialize_udt_correlate

      call analyze_udt_correlate

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program correlation_udt
