!main_FEM_volume_ave.f90
!     program  kemorin_FEM_volume_ave
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program kemorin_FEM_volume_ave
!
      use m_precision
!
      use calypso_mpi
      use analyzer_FEM_volume_average

      implicit none
!
!
!
      call calypso_MPI_init
!
      call  init_FEM_volume_ave
      call  analyze_FEM_volume_ave
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program kemorin_FEM_volume_ave
