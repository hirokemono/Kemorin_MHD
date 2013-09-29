!main_gen_z_filter.f90
!     program  generate_z_filter
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program generate_z_filter
!
      use m_precision
!
      use calypso_mpi
      use analyzer_gen_z_filter

      implicit none
!
!
      call calypso_MPI_init
!
      call init_analyzer

      call analyze

      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program generate_z_filter
