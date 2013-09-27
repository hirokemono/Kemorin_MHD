!main_pick_rms_sph.f90
!
!     program  pick_rms_sph
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program pick_rms_sph
!
      use m_precision
!
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_pick_rms_sph

      implicit none
!
!
      call parallel_cal_init
!
      call initialize_pick_rms_sph

      call analyze_pick_rms_sph

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program pick_rms_sph
