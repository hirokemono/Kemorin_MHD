!
!     program  sph_back_transform

!-----------------------------------------------------------------------
      program sph_back_transform
!
      use m_precision
!
      use calypso_mpi
      use analyzer_sph_back_trans
!
      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_sph_back_trans
!
      call analyze_sph_back_trans
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_back_transform
