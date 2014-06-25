!main_zonal_mean_sph.f90
!     program  zonal_mean_sph
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program zonal_mean_sph
!
      use m_precision
!
      use calypso_mpi
      use analyzer_zonal_mean_sph
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_zonal_mean_sph
      call analyze_zonal_mean_sph
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program zonal_mean_sph
