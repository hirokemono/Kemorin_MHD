!main_pick_rms_vol.f90
!
!     program  pick_rms_vol
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program pick_rms_vol
!
      use m_precision
!
      use m_parallel_var_dof
      use analyzer_pick_rms_vol
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialyze_pick_rms_vol
      call analyze_pick_rms_vol

      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program pick_rms_vol
