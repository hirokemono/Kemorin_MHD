!main_time_ave_sph.f90
!     program  time_ave_sph_spectr
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program time_ave_sph_spectr
!
      use m_precision
!
      use analyzer_time_ave_sph
      use calypso_mpi
!
      implicit none
!
!
      call calypso_MPI_init
!
!
      call initialize_ave_sph

      call evolution_ave_sph

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program time_ave_sph_spectr
