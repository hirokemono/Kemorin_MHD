!main_udt_diff.f90
!     program  diff_udt
!
!      Main routine for taking difference of field data
!      Programmed by H. Matsui on 2009
!
!-----------------------------------------------------------------------

      program diff_udt
!
      use m_precision
!
      use analyzer_udt_diff
      use m_parallel_var_dof

      implicit none
!
!
      call parallel_cal_init
!
      call initialize_udt_diff
      call analyze_udt_diff

      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program diff_udt
