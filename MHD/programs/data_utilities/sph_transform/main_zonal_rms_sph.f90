!main_zonal_mean_sph.f90
!     program  zonal_rms_sph
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program zonal_rms_sph
!
      use m_precision
!
      use calypso_mpi
      use analyzer_zonal_rms_sph

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
      end program zonal_rms_sph
