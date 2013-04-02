!
!     program  kemorin_sph_snapshot

!-----------------------------------------------------------------------
      program kemorin_sph_snapshot
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!    main routine for Kemo's MHD                 on May, 2003 (ver 2.0)
!    main routine for Kemo's MHD connect to vizs on July 2006 (ver 3.0)
!

      use m_precision
!
      use m_parallel_var_dof
      use analyzer_sph_snap
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
      end program kemorin_sph_snapshot
