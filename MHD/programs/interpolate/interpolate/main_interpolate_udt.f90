!main_interpolate_udt.f90
!     program  interpolate_udt
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program interpolate_udt
!
      use m_precision
!
      use calypso_mpi
      use analyzer_interpolate_udt

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_itp_udt

      call analyze_itp_udt

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program interpolate_udt
