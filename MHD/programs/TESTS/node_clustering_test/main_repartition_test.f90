!main_repartition_test.f90
!     program  repartition_test
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program repartition_test
!
      use m_precision
!
      use calypso_mpi
      use analyzer_repartition_test

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_repartition_test

      call analyze_repartition_test

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program repartition_test
