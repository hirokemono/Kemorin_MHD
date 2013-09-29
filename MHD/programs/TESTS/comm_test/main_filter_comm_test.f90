!main_filter_comm_test.f90
!     program  filter_commnication_test
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program filter_commnication_test
!
      use m_precision
!
      use calypso_mpi
      use analyzer_filter_comm_test

      implicit none


      call calypso_MPI_init
!
      call  init_analyzer

      call analyze

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program filter_commnication_test
