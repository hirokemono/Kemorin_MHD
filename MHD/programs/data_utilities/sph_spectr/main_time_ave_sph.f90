!main_time_ave_sph.f90
!     program  time_ave_sph_spectr
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program time_ave_sph_spectr
!
      use m_precision
!
      use analyzer_time_ave_sph
      use m_parallel_var_dof
!
      implicit none
!
!
      call parallel_cal_init
!
!
      call initialization

      call evolution

      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program time_ave_sph_spectr
