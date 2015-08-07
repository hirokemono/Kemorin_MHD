!main_mesh_test.f90
!     program  mesh_test
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program mesh_test
!
      use m_precision
!
      use calypso_mpi
      use analyzer_mesh_test

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_mesh_test

      call analyze_mesh_test

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program mesh_test
