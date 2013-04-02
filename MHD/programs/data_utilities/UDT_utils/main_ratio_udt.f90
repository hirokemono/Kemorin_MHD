!
!     program  ratio_udt
!
!      Main routine for taking ratio of field data
!      Programmed by H. Matsui on 2009
!
!-----------------------------------------------------------------------
      program ratio_udt
!
      use m_precision
!
      use analyzer_udt_ratio
      use m_parallel_var_dof

      implicit none
!
!
      call parallel_cal_init
!
      call initialize_udt_ratio
      call analyze_udt_ratio
!
      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program ratio_udt
