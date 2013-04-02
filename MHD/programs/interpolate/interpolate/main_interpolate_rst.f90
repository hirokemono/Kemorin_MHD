!main_interpolate_rst.f90
!     program  interpolate_rst
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program interpolate_rst
!
      use m_precision
!
      use analyzer_interpolate_rst
      use m_parallel_var_dof
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialize_itp_rst
!
      call analyze_itp_rst
!
      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program interpolate_rst
