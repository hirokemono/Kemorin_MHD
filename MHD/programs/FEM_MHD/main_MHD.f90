!
!     program  kemorin_FEM_MHD

!-----------------------------------------------------------------------
      program kemorin_FEM_MHD
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!    main routine for Kemo's MHD                 on May, 2003 (ver 2.0)
!    main routine for Kemo's MHD connect to vizs on July 2006 (ver 3.0)
!
      use m_precision
!
      use calypso_mpi
      use m_parallel_var_dof
      use analyzer_MHD
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialization_MHD
      call evolution_MHD
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program kemorin_FEM_MHD
