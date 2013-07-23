!main_med_grp_patch.f90
!
!     program  meridional_grp_patch
!
!      Main routine for generagte zonal grouping on meridional section
!      Programmed by H. Matsui on 2008
!
!-----------------------------------------------------------------------
!
      program meridional_grp_patch
!
      use m_precision
!
      use analyzer_med_grp_patch
      use m_parallel_var_dof

      implicit none
!

      call parallel_cal_init
!
      call initialize_med_grp_patch

      call analyze_med_grp_patch

      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program meridional_grp_patch
