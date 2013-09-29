!main_test_dz.f90
!     program  test_delta_z_plane
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)

      program test_delta_z_plane
!
      use m_precision
!
      use calypso_mpi
      use analyzer_test_dz

      implicit none
!
!
      call calypso_MPI_init
!
      call init_analyzer

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program test_delta_z_plane
