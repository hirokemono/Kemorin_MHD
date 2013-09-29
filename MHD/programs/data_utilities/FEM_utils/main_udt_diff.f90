!main_udt_diff.f90
!     program  diff_udt
!
!      Main routine for taking difference of field data
!      Programmed by H. Matsui on 2009
!
!-----------------------------------------------------------------------

      program diff_udt
!
      use m_precision
!
      use calypso_mpi
      use analyzer_udt_diff

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_udt_diff
      call analyze_udt_diff

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program diff_udt
