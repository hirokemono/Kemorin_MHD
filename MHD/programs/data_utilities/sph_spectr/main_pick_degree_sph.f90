!main_pick_degree_sph.f90
!     program  pickup_degree_sph
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program pickup_degree_sph
!
      use m_precision
!
      use m_parallel_var_dof
      use analyzer_pick_degree_sph

      implicit none
!
!
      call parallel_cal_init
!
      call init_analyzer

      call analyze

      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program pickup_degree_sph
