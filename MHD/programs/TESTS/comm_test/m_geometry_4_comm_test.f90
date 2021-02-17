!
!      module m_geometry_4_comm_test
!
!!      subroutine alloc_geom_4_comm_test(numele, wk_check)
!!      subroutine dealloc_ele_4_comm_test(wk_check)
!!        type(work_for_comm_check), intent(inout) :: wk_check
!
!      subroutine allocate_diff_nod_comm_test
!      subroutine allocate_diff_geom_comm_test
!      subroutine deallocate_diff_nod_comm_test
!      subroutine deallocate_diff_geom_comm_test
!
!      subroutine allocate_nod_stack_ctest_IO
!      subroutine allocate_geom_stack_ctest_IO
!      subroutine allocate_nod_comm_test_IO
!      subroutine allocate_geom_comm_test_IO
!
!      subroutine deallocate_nod_stack_ctest_IO
!      subroutine deallocate_geom_stack_ctest_IO
!      subroutine deallocate_nod_comm_test_IO
!      subroutine deallocate_geom_comm_test_IO
!
!     Written by H. Matsui on Sep., 2007
!
      module m_geometry_4_comm_test
!
      use m_precision
      use m_constants
!
      implicit  none
!
      type work_for_comm_check
        real(kind = kreal), allocatable :: xx_test(:,:)
      end type work_for_comm_check
!
      type(work_for_comm_check), save :: ele_check
      type(work_for_comm_check), save :: surf_check
      type(work_for_comm_check), save :: edge_check
!
      integer(kind = kint) :: nnod_diff_local
      integer(kind = kint) :: nele_diff_local
      integer(kind = kint) :: nsurf_diff_local
      integer(kind = kint) :: nedge_diff_local
!
      integer(kind = kint), allocatable :: inod_diff(:)
      integer(kind = kint), allocatable :: iele_diff(:)
      integer(kind = kint), allocatable :: isurf_diff(:)
      integer(kind = kint), allocatable :: iedge_diff(:)
!
      real(kind = kreal), allocatable :: xx_diff(:)
      real(kind = kreal), allocatable :: xele_diff(:)
      real(kind = kreal), allocatable :: xsurf_diff(:)
      real(kind = kreal), allocatable :: xedge_diff(:)
!
      integer(kind = kint), allocatable :: istack_nod_diff_pe(:)
      integer(kind = kint), allocatable :: istack_ele_diff_pe(:)
      integer(kind = kint), allocatable :: istack_surf_diff_pe(:)
      integer(kind = kint), allocatable :: istack_edge_diff_pe(:)
!
      integer(kind = kint), allocatable :: inod_diff_IO(:)
      integer(kind = kint), allocatable :: iele_diff_IO(:)
      integer(kind = kint), allocatable :: isurf_diff_IO(:)
      integer(kind = kint), allocatable :: iedge_diff_IO(:)
!
      real(kind = kreal), allocatable :: xx_diff_IO(:)
      real(kind = kreal), allocatable :: xele_diff_IO(:)
      real(kind = kreal), allocatable :: xsurf_diff_IO(:)
      real(kind = kreal), allocatable :: xedge_diff_IO(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_geom_4_comm_test(numele, wk_check)
!
      integer(kind = kint), intent(in) :: numele
      type(work_for_comm_check), intent(inout) :: wk_check
!
!
      allocate(wk_check%xx_test(numele,3))
      if(numele .gt. 0) wk_check%xx_test = 0.0d0
!
      end subroutine alloc_geom_4_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_ele_4_comm_test(wk_check)
!
      type(work_for_comm_check), intent(inout) :: wk_check
!
      deallocate(wk_check%xx_test)
!
      end subroutine dealloc_ele_4_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_diff_nod_comm_test
!
      allocate( inod_diff(nnod_diff_local) )
      allocate( xx_diff(6*nnod_diff_local) )
!
      inod_diff =     0
      xx_diff =       0.0d0
!
      end subroutine allocate_diff_nod_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine allocate_diff_geom_comm_test
!
!
      allocate( iele_diff(nele_diff_local) )
      allocate( xele_diff(6*nele_diff_local) )
!
      allocate( isurf_diff(nsurf_diff_local) )
      allocate( xsurf_diff(6*nsurf_diff_local) )
!
      allocate( iedge_diff(nedge_diff_local) )
      allocate( xedge_diff(6*nedge_diff_local) )
!
      iele_diff =     0
      xele_diff =     0.0d0
      isurf_diff =    0
      xsurf_diff =    0.0d0
      iedge_diff =    0
      xedge_diff =    0.0d0
!
      end subroutine allocate_diff_geom_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_diff_nod_comm_test
!
      deallocate( inod_diff    )
      deallocate( xx_diff      )
!
      end subroutine deallocate_diff_nod_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_diff_geom_comm_test
!
!
      deallocate( iele_diff    )
      deallocate( xele_diff    )
!
      deallocate( isurf_diff    )
      deallocate( xsurf_diff    )
!
      deallocate( iedge_diff    )
      deallocate( xedge_diff    )
!
      end subroutine deallocate_diff_geom_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_nod_stack_ctest_IO
!
      use calypso_mpi
!
      allocate( istack_nod_diff_pe(0:nprocs)  )
      istack_nod_diff_pe =  0
!
      end subroutine allocate_nod_stack_ctest_IO
!
! ----------------------------------------------------------------------
!
      subroutine allocate_geom_stack_ctest_IO
!
      use calypso_mpi
!
!
      allocate( istack_ele_diff_pe(0:nprocs)  )
      allocate( istack_surf_diff_pe(0:nprocs) )
      allocate( istack_edge_diff_pe(0:nprocs) )
      istack_ele_diff_pe =  0
      istack_surf_diff_pe = 0
      istack_edge_diff_pe = 0
!
      end subroutine allocate_geom_stack_ctest_IO
!
! ----------------------------------------------------------------------
!
      subroutine allocate_nod_comm_test_IO
!
      use calypso_mpi
!
      allocate( inod_diff_IO(istack_nod_diff_pe(nprocs)) )
      allocate( xx_diff_IO(6*istack_nod_diff_pe(nprocs)) )
!
      inod_diff_IO =     0
      xx_diff_IO =       0.0d0
!
      end subroutine allocate_nod_comm_test_IO
!
! ----------------------------------------------------------------------
!
      subroutine allocate_geom_comm_test_IO
!
      use calypso_mpi
!
      allocate( iele_diff_IO(istack_ele_diff_pe(nprocs) ) )
      allocate( xele_diff_IO(6*istack_ele_diff_pe(nprocs) ) )
!
      allocate( isurf_diff_IO(istack_surf_diff_pe(nprocs)) )
      allocate( xsurf_diff_IO(6*istack_surf_diff_pe(nprocs)) )
!
      allocate( iedge_diff_IO(istack_edge_diff_pe(nprocs)) )
      allocate( xedge_diff_IO(6*istack_edge_diff_pe(nprocs)) )
!
      iele_diff_IO =     0
      xele_diff_IO =     0.0d0
      isurf_diff_IO =    0
      xsurf_diff_IO =    0.0d0
      iedge_diff_IO =    0
      xedge_diff_IO =    0.0d0
!
      end subroutine allocate_geom_comm_test_IO
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_nod_stack_ctest_IO
!
      deallocate( istack_nod_diff_pe )
!
      end subroutine deallocate_nod_stack_ctest_IO
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_geom_stack_ctest_IO
!
!
      call deallocate_nod_stack_ctest_IO
!
      deallocate( istack_ele_diff_pe  )
      deallocate( istack_surf_diff_pe )
      deallocate( istack_edge_diff_pe )
!
      end subroutine deallocate_geom_stack_ctest_IO
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_nod_comm_test_IO
!
      deallocate( inod_diff_IO    )
      deallocate( xx_diff_IO      )
!
      end subroutine deallocate_nod_comm_test_IO
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_geom_comm_test_IO
!
!
      deallocate( iele_diff_IO     )
      deallocate( xele_diff_IO     )
!
      deallocate( isurf_diff_IO    )
      deallocate( xsurf_diff_IO    )
!
      deallocate( iedge_diff_IO    )
      deallocate( xedge_diff_IO    )
!
      end subroutine deallocate_geom_comm_test_IO
!
! ----------------------------------------------------------------------
!
      end module m_geometry_4_comm_test
