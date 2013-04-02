!
!     program  gauss_back_transform

!-----------------------------------------------------------------------
      program gauss_back_transform
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!    main routine for Kemo's MHD                 on May, 2003 (ver 2.0)
!    main routine for Kemo's MHD connect to vizs on July 2006 (ver 3.0)
!

      use m_precision
!
      use m_parallel_var_dof
      use analyzer_gauss_back_trans

      implicit none
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
      end program gauss_back_transform
