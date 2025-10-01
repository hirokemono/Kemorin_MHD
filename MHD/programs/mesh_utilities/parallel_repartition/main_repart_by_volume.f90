!main_repart_by_volume.f90
!     program  repartition_by_volume
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program repartition_by_volume
!
      use m_precision
!
      use calypso_mpi
      use analyzer_repart_by_volume

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_reapart_by_vol
      call analyze_reapart_by_vol
!
      call calypso_MPI_finalize
!
      stop '***** program finished *****'
!
      end program repartition_by_volume
