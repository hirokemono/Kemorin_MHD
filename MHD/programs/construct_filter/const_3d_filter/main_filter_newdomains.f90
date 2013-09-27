!main_filter_newdomains.f90
!
!     program  filter_newdomains

!-----------------------------------------------------------------------
      program filter_newdomains
!
      use m_precision
!
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_filter_newdomains
!
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
      end program filter_newdomains
