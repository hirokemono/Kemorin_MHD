!main_zm_transform.f90
!     program  zonal_mean_transform
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program zonal_mean_transform
!
      use m_precision
!
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_zm_transform
!
      implicit none
!
      call parallel_cal_init
!
      call init_zm_trans
      call analyze_zm_trans

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program zonal_mean_transform
