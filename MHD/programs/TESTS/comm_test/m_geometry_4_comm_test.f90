!
!      module m_geometry_4_comm_test
!
!      subroutine allocate_geom_4_comm_test(numele, numsurf, numedge)
!      subroutine deallocate_geom_4_comm_test
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
      real(kind = kreal), allocatable :: x_ele_comm(:)
      real(kind = kreal), allocatable :: x_surf_comm(:)
      real(kind = kreal), allocatable :: x_edge_comm(:)
!
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
!
      integer(kind = kint) :: ntot_nod_diff_pe
      integer(kind = kint) :: ntot_ele_diff_pe
      integer(kind = kint) :: ntot_surf_diff_pe
      integer(kind = kint) :: ntot_edge_diff_pe
!
      integer(kind = kint), allocatable :: num_nod_diff_pe(:)
      integer(kind = kint), allocatable :: num_ele_diff_pe(:)
      integer(kind = kint), allocatable :: num_surf_diff_pe(:)
      integer(kind = kint), allocatable :: num_edge_diff_pe(:)
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
      subroutine allocate_geom_4_comm_test(numele, numsurf, numedge)
!
      integer(kind = kint), intent(in) :: numele, numsurf, numedge
!
!
      allocate(x_ele_comm(3*numele))
      x_ele_comm = 0.0d0
!
      allocate(x_surf_comm(3*numsurf))
      x_surf_comm = 0.0d0
!
      allocate(x_edge_comm(3*numedge))
      x_edge_comm = 0.0d0
!
      end subroutine allocate_geom_4_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_geom_4_comm_test
!
!
      deallocate(x_ele_comm   )
      deallocate(x_surf_comm  )
      deallocate(x_edge_comm  )
!
      end subroutine deallocate_geom_4_comm_test
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
      allocate( num_nod_diff_pe(nprocs)  )
      allocate( istack_nod_diff_pe(0:nprocs)  )
!
      num_nod_diff_pe =     0
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
      allocate( num_ele_diff_pe(nprocs)  )
      allocate( num_surf_diff_pe(nprocs) )
      allocate( num_edge_diff_pe(nprocs) )
      allocate( istack_ele_diff_pe(0:nprocs)  )
      allocate( istack_surf_diff_pe(0:nprocs) )
      allocate( istack_edge_diff_pe(0:nprocs) )
      num_ele_diff_pe =     0
      num_surf_diff_pe =    0
      num_edge_diff_pe =    0
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
      allocate( inod_diff_IO(ntot_nod_diff_pe) )
      allocate( xx_diff_IO(6*ntot_nod_diff_pe) )
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
!
      allocate( iele_diff_IO(ntot_ele_diff_pe) )
      allocate( xele_diff_IO(6*ntot_ele_diff_pe) )
!
      allocate( isurf_diff_IO(ntot_surf_diff_pe) )
      allocate( xsurf_diff_IO(6*ntot_surf_diff_pe) )
!
      allocate( iedge_diff_IO(ntot_edge_diff_pe) )
      allocate( xedge_diff_IO(6*ntot_edge_diff_pe) )
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
      deallocate( num_nod_diff_pe    )
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
      deallocate( num_ele_diff_pe     )
      deallocate( num_surf_diff_pe    )
      deallocate( num_edge_diff_pe    )
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
