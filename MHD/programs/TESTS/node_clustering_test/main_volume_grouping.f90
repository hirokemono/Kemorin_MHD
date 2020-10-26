!main_volume_grouping.f90
!     program  volume_grouping_test
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program volume_grouping_test
!
      use m_precision
!
      use calypso_mpi
      use analyzer_volume_grouping

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_volume_grouping
      call analyze_volume_grouping
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program volume_grouping_test
