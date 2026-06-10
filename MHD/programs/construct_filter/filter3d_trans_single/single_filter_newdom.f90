!single_filter_newdom.f90
!     program  filter_newdomain_single

!-----------------------------------------------------------------------
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)

      program filter_newdomain_single
!
      use m_precision
!
      use analyzer_filter_newdom_sgl
!
      implicit none
!
!
      call newdomain_filter_init
!
      call newdomain_filter_analyze

      write(*,*) '***** single node program finished *****'
      stop
!
      end program filter_newdomain_single
