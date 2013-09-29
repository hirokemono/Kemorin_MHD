!main_sph_neutral_point.f90
!     program  sph_neutral_point
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program sph_neutral_point
!
      use m_precision
!
      use analyzer_sph_neutral_point
      use calypso_mpi

      implicit none
!
!
      call calypso_MPI_init
!
      call init_analyzer

      call analyze
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_neutral_point
