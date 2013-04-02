!collect_diff_filter_ctest.f90
!      module collect_diff_filter_ctest
!
      module collect_diff_filter_ctest
!
!     Written by H. Matsui on Sep., 2007
!
      use m_precision
!
      use m_parallel_var_dof
      use m_geometry_filter_comm_test
!
!
      implicit  none
!
!      subroutine count_diff_filter_nod_comm_test
!      subroutine collect_diff_filter_nod_ctest
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_diff_filter_nod_comm_test
!
      use collect_diff_4_comm_test
!
      call count_diff_ctest(nnod_filter_diff_local,                     &
     &    ntot_nod_filter_diff_pe, num_filter_nod_diff_pe,              &
     &    istack_filter_nod_diff_pe)
!
      end subroutine count_diff_filter_nod_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine collect_diff_filter_nod_ctest
!
      use collect_diff_4_comm_test
!
      call collect_diff_ctest(nprocs, nnod_filter_diff_local,           &
     &    inod_filter_diff, inod_gl_filter_diff, xx_filter_diff,        &
     &    ntot_nod_filter_diff_pe, num_filter_nod_diff_pe,              &
     &    istack_filter_nod_diff_pe, inod_filter_diff_IO,               &
     &    inod_gl_filter_diff_IO, xx_filter_diff_IO)
!
!
      end subroutine collect_diff_filter_nod_ctest
!
! ----------------------------------------------------------------------
!
      end module collect_diff_filter_ctest
