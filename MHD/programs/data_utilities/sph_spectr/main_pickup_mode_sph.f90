!main_pickup_mode_sph.f90
!     program  pickup_mode_sph
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program pickup_mode_sph
!
      use m_precision
!
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_pickup_mode_sph
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialization

      call evolution

      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program pickup_mode_sph
