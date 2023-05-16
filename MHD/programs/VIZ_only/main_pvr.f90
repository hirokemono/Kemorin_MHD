!>@file   main_pvr.f90
!!@brief  program kemo_volume_rendering
!!
!!@author H. Matsui
!!@date Programmed in Mar., 2000 (ver 1.0)
!!      Modified in May, 2003
!!      Modified in July 2006
!!
!> @brief Main program for parallel volume rendering
!!
!!@verbatim
!!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!!    main routine for Kemo's MHD                 on May, 2003 (ver 2.0)
!!    main routine for Kemo's MHD connect to vizs on July 2006 (ver 3.0)
!!@endverbatim
      program kemo_volume_rendering
!
      use m_precision
!
      use calypso_mpi
      use analyzer_pvr
!
      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_pvr
      call analyze_pvr
!
      call  calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program kemo_volume_rendering
