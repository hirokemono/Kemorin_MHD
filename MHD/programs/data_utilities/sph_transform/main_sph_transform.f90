!main_sph_transform.f90
!     program  geofem_tiger

!-----------------------------------------------------------------------
      program geofem_tiger
! \beginSUBROUTINE
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)

      use m_precision
!
      use calypso_mpi
      use analyzer_sph_transform

      implicit none
!

      call calypso_MPI_init
!
      call initialize_sph_transform
      call analyze_sph_transform

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program geofem_tiger
