!main_fix_repart_by_mesh.f90
!     program  fix_repart_by_mesh
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program fix_repart_by_mesh
!
      use m_precision
!
      use calypso_mpi
      use analyzer_fix_repart_by_mesh

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_fix_repart_by_mesh
      call analyze_fix_repart_by_mesh
!
      call calypso_MPI_finalize
!
      stop '***** program finished *****'
!
      end program fix_repart_by_mesh
