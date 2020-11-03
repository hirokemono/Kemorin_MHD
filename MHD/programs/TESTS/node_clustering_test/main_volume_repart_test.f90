!main_volume_repart_test.f90
!     program  volume_repartition_test
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program volume_repartition_test
!
      use m_precision
!
      use calypso_mpi
      use analyzer_volume_repart_test

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_volume_repartition
      call analyze_volume_repartition
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program volume_repartition_test
