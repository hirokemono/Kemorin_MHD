!main_ene_sph_shell.f90
!
!     program  ene_sph_shell
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program ene_sph_shell
!
      use m_precision
!
      use calypso_mpi
      use analyzer_ene_sph_shell

      implicit none
!

      call calypso_MPI_init
!
      call initialize_ene_sph_shell
      call analyze_ene_sph_shell

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program ene_sph_shell
