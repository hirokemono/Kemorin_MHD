!main_sleeve_extend.f90
!     program  main_sleeve_extend
!
!
      program main_sleeve_extend
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sleeve_extend

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_sleeve_extend

      call analyze_sleeve_extend

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program main_sleeve_extend
