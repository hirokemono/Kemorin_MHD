!main_interpolate_rst.f90
!     program  interpolate_rst
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program interpolate_rst
!
      use m_precision
!
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_interpolate_rst
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialize_itp_rst
!
      call analyze_itp_rst
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program interpolate_rst
