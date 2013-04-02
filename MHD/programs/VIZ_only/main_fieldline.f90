!
!     program  kemorin_fieldline
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!    main routine for Kemo's MHD                 on May, 2003 (ver 2.0)
!    main routine for Kemo's MHD connect to vizs on July 2006 (ver 3.0)
!
!-----------------------------------------------------------------------
!
      program kemorin_fieldline
!
      use m_precision
!
      use m_parallel_var_dof
      use analyzer_fline
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialization
      call analyze
!
      call  parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program kemorin_fieldline
