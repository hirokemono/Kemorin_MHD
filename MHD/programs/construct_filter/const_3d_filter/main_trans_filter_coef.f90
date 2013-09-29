!main_trans_filter_coef.f90
!
!     program  transfer_filter_coef

!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)

      program transfer_filter_coef
!
      use m_precision
!
      use calypso_mpi
      use analyzer_trans_filter_coef

      implicit none
!
!
      call calypso_MPI_init
!
      call init_analyzer

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program transfer_filter_coef
