!main_FEM_volume_ave.f90
!     program  kemorin_FEM_volume_ave
!
      program kemorin_FEM_volume_ave
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      use m_precision
!
      use calypso_mpi
      use analyzer_volume_ave

      implicit none
!
!
!
      call calypso_MPI_init
!
      call  init_analyzer
      call  analyze
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program kemorin_FEM_volume_ave
