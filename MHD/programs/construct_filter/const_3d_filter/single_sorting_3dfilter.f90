!single_sorting_3dfilter.f90
!     program  sorting_3dfilter

!-----------------------------------------------------------------------
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)

      program sorting_3dfilter
!
      use m_precision
!
      use analyzer_sorting_3dfilter

      implicit none
!
!
      call init_analyzer
      call analyze

      write(*,*) '***** single node program finished *****'
      stop
!
      end program sorting_3dfilter
