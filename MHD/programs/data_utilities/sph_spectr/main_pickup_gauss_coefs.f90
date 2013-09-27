!main_pickup_gauss_coefs.f90
!     program  pickup_gauss_coefs
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program pickup_gauss_coefs
!
      use m_precision
!
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_pickup_gauss_coefs
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialize_pick_gauss_coef

      call analyze_pick_gauss_coef

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program pickup_gauss_coefs
