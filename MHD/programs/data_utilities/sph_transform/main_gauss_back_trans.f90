!
!     program  gauss_back_transform

!-----------------------------------------------------------------------
      program gauss_back_transform
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!    main routine for Kemo's MHD                 on May, 2003 (ver 2.0)
!    main routine for Kemo's MHD connect to vizs on July 2006 (ver 3.0)
!

      use m_precision
!
      use calypso_mpi
      use analyzer_gauss_back_trans

      implicit none
!

      call calypso_MPI_init
!
      call init_analyzer_gauss_back_trans
      call analyze_gauss_back_trans
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program gauss_back_transform
