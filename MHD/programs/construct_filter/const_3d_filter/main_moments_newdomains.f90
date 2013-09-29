!main_moments_newdomains.f90
!     program  moments_on_newdomains

!-----------------------------------------------------------------------
! \beginSUBROUTINE
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)

      program moments_on_newdomains
!
      use m_precision
!
      use calypso_mpi
      use analyzer_moments_newdomains
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_analyzer

      call analyze

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program moments_on_newdomains
