!main_sph_neutral_point.f90
!     program  sph_neutral_point
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program sph_neutral_point
!
      use m_precision
!
      use analyzer_sph_neutral_point
      use m_parallel_var_dof

      implicit none
!
!
      call parallel_cal_init
!
      call init_analyzer

      call analyze
!
      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_neutral_point
