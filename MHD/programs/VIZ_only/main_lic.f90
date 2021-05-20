!
!     program  lic_only
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!    main routine for Kemo's MHD                 on May, 2003 (ver 2.0)
!    main routine for Kemo's MHD connect to vizs on July 2006 (ver 3.0)
!

      program lic_only
!
      use m_precision
!
      use calypso_mpi
      use analyzer_lic

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_lic
      call analyze_lic
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program lic_only
