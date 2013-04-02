!main_filter_newdomains.f90
!
!     program  filter_newdomains

!-----------------------------------------------------------------------
      program filter_newdomains
!
      use m_precision
!
      use analyzer_filter_newdomains
      use m_parallel_var_dof
!
      implicit none
!

      call parallel_cal_init
!
      call init_analyzer

      call analyze

      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program filter_newdomains
