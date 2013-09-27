!main_test_itp_ele.f90
!     program  test_itp_element_table

!-----------------------------------------------------------------------
      program test_itp_element_table
! \beginSUBROUTINE
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)

      use m_precision
!
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_test_ele_tbl

      implicit none
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
      end program test_itp_element_table
