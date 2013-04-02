!
!     program  sph_back_transform

!-----------------------------------------------------------------------
      program sph_back_transform
!
      use m_precision
!
      use m_parallel_var_dof
      use analyzer_sph_back_trans
!
      implicit none
!
!
      call parallel_cal_init
!
      call init_analyzer
!
      call analyze
!
      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program sph_back_transform
