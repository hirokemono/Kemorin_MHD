!main_gen_itp_tbl.f90
!     program  test_interpolate_table

!-----------------------------------------------------------------------
      program test_interpolate_table
! \beginSUBROUTINE
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)

      use m_precision
!
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_test_table

      implicit none
!
!
      call parallel_cal_init
!
      call init_analyzer

      call analyze

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program test_interpolate_table
