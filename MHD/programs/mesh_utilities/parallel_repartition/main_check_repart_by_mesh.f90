!main_check_repart_by_mesh.f90
!     program  check_repart_by_mesh
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program check_repart_by_mesh
!
      use m_precision
!
      use calypso_mpi
      use analyzer_chk_repart_by_mesh

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_chk_repart_by_mesh
      call analyze_chk_repart_by_mesh
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program check_repart_by_mesh
