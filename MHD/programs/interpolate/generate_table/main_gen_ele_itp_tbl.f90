!main_gen_ele_itp_tbl.f90
!     program  const_ele_interpolate_tbl

!-----------------------------------------------------------------------
! \beginSUBROUTINE
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)

      program const_ele_interpolate_tbl
!
      use m_precision
!
      use calypso_mpi
      use analyzer_gen_ele_table

      implicit none
!
!
      call calypso_MPI_init
!
      call init_analyzer

      call  analyze

      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program const_ele_interpolate_tbl
