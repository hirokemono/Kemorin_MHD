!main_udt_correlate_1comp.f90
!
!     program  correlation_udt_1comp
!
!      Main routine for taking correlation with one component
!      Programmed by H. Matsui on 2009
!
!-----------------------------------------------------------------------
!
      program correlation_udt_1comp
!
      use m_precision
!
      use calypso_mpi
      use analyzer_udt_corr_1comp

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_udt_corre_1comp

      call analyze_udt_corr_1comp

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program correlation_udt_1comp
