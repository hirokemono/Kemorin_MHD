!main_MHD_lenghscale.f90
!     program  length_scale_MHD
!
!      Main routine for evaluating length scale from diffusion
!      Programmed by H. Matsui on 2012
!
!-----------------------------------------------------------------------
!
      program length_scale_MHD
!
      use m_precision
!
      use analyzer_MHD_lengthscale
      use m_parallel_var_dof

      implicit none
!

      call parallel_cal_init
!
      call initialize_MHD_lscale

      call analyze_MHD_lscale

      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program length_scale_MHD
