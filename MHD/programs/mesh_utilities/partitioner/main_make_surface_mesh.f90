!main_make_surface_mesh.f90
!     program  make_surface_mesh
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program make_surface_mesh
!
      use m_precision
!
      use calypso_mpi
      use analyzer_make_surface_mesh

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_make_surface_mesh

      call analyze_make_surface_mesh

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program make_surface_mesh
