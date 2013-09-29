!main_ene_zonal_spec.f90
!     program  ene_equator_zonal_spectr

!-----------------------------------------------------------------------
      program ene_equator_zonal_spectr
!
!      Main routine for equatorial spectrum
!      Programmed by H. Matsui on 2009
!

      use m_precision
!
      use calypso_mpi
      use analyzer_ene_zonal_spec

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
      end program ene_equator_zonal_spectr
