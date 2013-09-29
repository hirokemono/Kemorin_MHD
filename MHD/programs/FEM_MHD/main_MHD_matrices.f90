!main_MHD_matrices.f90
!     program  kemorin_FEM_MHD_matrices
!
      program kemorin_FEM_MHD_matrices
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      use m_precision
!
      use calypso_mpi
      use analyzer_check_mat_MHD
!
      implicit none
!
!
!
      call calypso_MPI_init
!
      call  init_analyzer
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program kemorin_FEM_MHD_matrices
