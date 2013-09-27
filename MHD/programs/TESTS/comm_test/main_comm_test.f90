!main_comm_test.f90
!
!     program  commnication_test
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program commnication_test
!
      use m_precision
!
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_comm_test

      implicit none
!
!
      call parallel_cal_init
!
      call init_analyzer

      call analyze

      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program commnication_test
