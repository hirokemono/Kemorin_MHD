!main_gen_filter.f90
!     program  gen_filter

!-----------------------------------------------------------------------
      program gen_filter
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      use m_precision
!
      use calypso_mpi
      use analyzer_gen_filter
!
      implicit none
!
!
      call calypso_MPI_init
!
      call generate_filter_init
!
      call generate_filter_analyze
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program gen_filter
