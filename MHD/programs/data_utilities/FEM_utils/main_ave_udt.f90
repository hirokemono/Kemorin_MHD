!main_ave_udt.f90
!
!     program  time_average_udt
!
!      Main routine for taking (time) average of field data
!      Programmed by H. Matsui on 2008
!
!-----------------------------------------------------------------------
!
      program time_average_udt
!
      use m_precision
!
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_ave_udt

      implicit none
!
!
      call parallel_cal_init
!
      call initialize_ave_udt
      call analyze_ave_udt

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program time_average_udt
