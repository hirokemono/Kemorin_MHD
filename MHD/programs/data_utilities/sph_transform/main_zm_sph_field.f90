!main_zm_sph_field.f90
!     program  zonal_mean_sph_field
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program zonal_mean_sph_field
!
      use m_precision
!
      use calypso_mpi
      use analyzer_zm_sph_field

      implicit none
!
      call calypso_MPI_init
!
      call init_zm_sph_field
      call analyze_zm_sph_field

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program zonal_mean_sph_field
