!
!     program  product_udt
!
!      Main routine for taking products of field data
!      Programmed by H. Matsui on 2009
!
!-----------------------------------------------------------------------
!
      program product_udt
!
      use m_precision
!
      use analyzer_udt_product
      use m_parallel_var_dof

      implicit none
!

      call parallel_cal_init
!
      call initialize_udt_product
      call analyze_udt_product
!
      call  parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program product_udt
