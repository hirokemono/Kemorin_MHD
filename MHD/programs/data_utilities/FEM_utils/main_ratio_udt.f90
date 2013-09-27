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
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_udt_ratio

      implicit none
!
!
      call parallel_cal_init
!
      call initialize_udt_ratio
      call analyze_udt_ratio
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program ratio_udt
