!>@file   main_rename_sph_field.f90
!!@brief  program rename_sph_field
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Change spectr data field name
!
      program rename_sph_field
!
      use m_precision
!
      use calypso_mpi
      use analyzer_rename_sph_field
!
      implicit none
!
!
      call calypso_MPI_init
!
      call evolution_rename_sph_field
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program rename_sph_field
