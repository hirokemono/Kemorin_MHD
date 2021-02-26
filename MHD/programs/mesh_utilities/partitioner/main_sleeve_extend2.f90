!main_sleeve_extend2.f90
!     program  main_sleeve_extend2
!
!
      program main_sleeve_extend2
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sleeve_extend2

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_sleeve_extend2

      call analyze_sleeve_extend2

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program main_sleeve_extend2
