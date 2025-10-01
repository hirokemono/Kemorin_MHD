!main_field_to_repartition.f90
!     program  field_to_repartition
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program field_to_repartition
!
      use m_precision
!
      use calypso_mpi
      use analyzer_field_to_repart
!
      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_field_to_repart
      call analyze_field_to_repart
!
      call calypso_MPI_finalize
!
      stop '***** program finished *****'
!
      end program field_to_repartition
