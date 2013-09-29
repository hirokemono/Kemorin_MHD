!main_med_grp_patch.f90
!
!     program  meridional_grp_patch
!
!      Main routine for generagte zonal grouping on meridional section
!      Programmed by H. Matsui on 2008
!
!-----------------------------------------------------------------------
!
      program meridional_grp_patch
!
      use m_precision
!
      use calypso_mpi
      use analyzer_med_grp_patch

      implicit none
!

      call calypso_MPI_init
!
      call initialize_med_grp_patch

      call analyze_med_grp_patch

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program meridional_grp_patch
