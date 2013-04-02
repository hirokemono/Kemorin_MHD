!
!     program  sph_dynamobench

!-----------------------------------------------------------------------
      program sph_dynamobench
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!    main routine for Benchmark check            on May, 2012 (ver 2.0)
!
      use m_precision
!
      use analyzer_sph_dynamobench
      use m_parallel_var_dof
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialization
!
      call evolution
!
      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_dynamobench
