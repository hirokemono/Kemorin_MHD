!main_check_repartition.f90
!     program  check_repartition
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program check_repartition
!
      use m_precision
!
      use calypso_mpi
      use analyzer_check_repart

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_check_reapart_mesh
      call analyze_check_reapart_mesh
!
      call calypso_MPI_finalize
!
      stop '***** program finished *****'
!
      end program check_repartition
