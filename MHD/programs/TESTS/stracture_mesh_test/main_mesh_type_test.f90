!main_mesh_type_test.f90
!     program  stracture_mesh_test
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program stracture_mesh_test
!
      use m_precision
!
      use calypso_mpi
      use analyzer_mesh_type_test

      implicit none
!

      call calypso_MPI_init
!
      call  init_analyzer

      call    analyze

      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program stracture_mesh_test
