!m_geometry_filter_comm_test.f90
!      module m_geometry_filter_comm_test
!
      module m_geometry_filter_comm_test
!
!     Written by H. Matsui on May, 2008
!
      use m_precision
!
      implicit  none
!
      real(kind = kreal), allocatable :: xx_filter_comm(:)
!
      integer(kind = kint) :: nnod_filter_diff_local
      integer(kind = kint), allocatable :: inod_filter_diff(:)
!
      real(kind = kreal), allocatable :: xx_filter_diff(:)
!
      integer(kind = kint), allocatable :: istack_filter_nod_diff_pe(:)
      integer(kind = kint), allocatable :: inod_filter_diff_IO(:)
!
      real(kind = kreal), allocatable :: xx_filter_diff_IO(:)
!
!      subroutine allocate_filter_nod_comm_test
!      subroutine deallocate_filter_nod_comm_test
!
!      subroutine allocate_diff_filter_ctest
!      subroutine deallocate_diff_filter_ctest
!
!      subroutine allocate_filter_stk_ctest_IO
!      subroutine allocate_filter_comm_test_IO
!
!      subroutine deallocate_filter_stk_ctest_IO
!      subroutine deallocate_filter_comm_test_IO
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_filter_nod_comm_test
!
      use m_nod_filter_comm_table
!
      allocate(xx_filter_comm(3*nnod_filtering))
      xx_filter_comm = 0.0d0
!
      end subroutine allocate_filter_nod_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_filter_nod_comm_test
!
      deallocate(xx_filter_comm)
!
      end subroutine deallocate_filter_nod_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_diff_filter_ctest
!
      allocate( inod_filter_diff(nnod_filter_diff_local) )
      allocate( xx_filter_diff(6*nnod_filter_diff_local) )
!
      inod_filter_diff =     0
      xx_filter_diff =       0.0d0
!
      end subroutine allocate_diff_filter_ctest
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_diff_filter_ctest
!
      deallocate( inod_filter_diff    )
      deallocate( xx_filter_diff      )
!
      end subroutine deallocate_diff_filter_ctest
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_filter_stk_ctest_IO
!
      use calypso_mpi
!
      allocate( istack_filter_nod_diff_pe(0:nprocs)  )
      istack_filter_nod_diff_pe =  0
!
      end subroutine allocate_filter_stk_ctest_IO
!
! ----------------------------------------------------------------------
!
      subroutine allocate_filter_comm_test_IO
!
      use calypso_mpi
!
      allocate(inod_filter_diff_IO(istack_filter_nod_diff_pe(nprocs)))
      allocate(xx_filter_diff_IO(6*istack_filter_nod_diff_pe(nprocs)))
!
      inod_filter_diff_IO =     0
      xx_filter_diff_IO =       0.0d0
!
      end subroutine allocate_filter_comm_test_IO
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_filter_stk_ctest_IO
!
      deallocate( istack_filter_nod_diff_pe )
!
      end subroutine deallocate_filter_stk_ctest_IO
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_filter_comm_test_IO
!
      deallocate( inod_filter_diff_IO    )
      deallocate( xx_filter_diff_IO      )
!
      end subroutine deallocate_filter_comm_test_IO
!
! ----------------------------------------------------------------------
!
      end module m_geometry_filter_comm_test
