!
!     program  sph_MHD_only
!
     program sph_MHD_only
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!    main routine for spherical  MHD             on May, 2003 (ver 2.0)
!
!
      use m_precision
!
      use m_parallel_var_dof
      use analyzer_small_sph_MHD
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
      stop
      end program sph_MHD_only
