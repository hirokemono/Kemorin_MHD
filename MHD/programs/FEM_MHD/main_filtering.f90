!
!     program  kemorin_FEM_snapshot

!-----------------------------------------------------------------------
      program kemorin_FEM_snapshot
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!    main routine for Kemo's MHD                 on May, 2003 (ver 2.0)
!    main routine for Kemo's MHD connect to vizs on July 2006 (ver 3.0)
!

      use m_precision
!
      use analyzer_filtering
      use calypso_mpi
      use m_parallel_var_dof
!
      implicit none
!
!
      call parallel_cal_init
!
      call init_analyzer
      call analyze
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program kemorin_FEM_snapshot
