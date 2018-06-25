!>@file   main_merge_mesh.f90
!!@brief  program merge_mesh
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in July 2014 
!
!>@brief  Main program for assemble field data
!
      program merge_mesh
!
      use m_precision
!
      use calypso_mpi
      use analyzer_merge_mesh
!
      implicit none
!
!
      call calypso_MPI_init
!
      call init_merge_mesh
      call analyze_merge_mesh
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program merge_mesh
