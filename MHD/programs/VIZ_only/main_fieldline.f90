!>@file   main_fieldline.f90
!!@brief  program kemorin_fieldline
!!
!!@author H. Matsui
!!@date Programmed in Mar., 2000 (ver 1.0)
!!      Modified in May, 2003
!!      Modified in July 2006
!!
!> @brief Main program for field line generation
!!
!!@verbatim
!!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!!    main routine for Kemo's MHD                 on May, 2003 (ver 2.0)
!!    main routine for Kemo's MHD connect to vizs on July 2006 (ver 3.0)
!!@endverbatim
      program kemorin_fieldline
!
      use m_precision
!
      use calypso_mpi
      use analyzer_fline
!
      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_fline
      call analyze_fline
!
      call  calypso_MPI_finalize
!
      stop '***** program finished *****'
!
      end program kemorin_fieldline
