!main_interpolate_udt.f90
!     program  interpolate_udt
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program interpolate_udt
!
      use m_precision
!
      use analyzer_interpolate_udt
      use m_parallel_var_dof

      implicit none
!
!
      call parallel_cal_init
!
      call initialize_itp_udt

      call analyze_itp_udt

      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program interpolate_udt
