!m_dxi_dxes_3d_node.f90
!      module m_dxi_dxes_3d_node
!
!        programmed by H.Matsui on Nov., 2008
!
!      subroutine allocate_dxi_dx_ele
!      subroutine allocate_dxi_dx_nod
!      subroutine deallocate_dxi_dx_ele
!      subroutine deallocate_dxi_dx_nod
!
      module m_dxi_dxes_3d_node
!
      use m_precision
!
      implicit none
!
      real(kind=kreal),   allocatable :: dxidx_ele(:)
      real(kind=kreal),   allocatable :: deidx_ele(:)
      real(kind=kreal),   allocatable :: dzidx_ele(:)
      real(kind=kreal),   allocatable :: dxidy_ele(:)
      real(kind=kreal),   allocatable :: deidy_ele(:)
      real(kind=kreal),   allocatable :: dzidy_ele(:)
      real(kind=kreal),   allocatable :: dxidz_ele(:)
      real(kind=kreal),   allocatable :: deidz_ele(:)
      real(kind=kreal),   allocatable :: dzidz_ele(:)
!
      real(kind=kreal),   allocatable :: dxidx_nod(:)
      real(kind=kreal),   allocatable :: deidx_nod(:)
      real(kind=kreal),   allocatable :: dzidx_nod(:)
      real(kind=kreal),   allocatable :: dxidy_nod(:)
      real(kind=kreal),   allocatable :: deidy_nod(:)
      real(kind=kreal),   allocatable :: dzidy_nod(:)
      real(kind=kreal),   allocatable :: dxidz_nod(:)
      real(kind=kreal),   allocatable :: deidz_nod(:)
      real(kind=kreal),   allocatable :: dzidz_nod(:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_dxi_dx_ele
!
      use m_geometry_parameter
!
!
      allocate( dxidx_ele(numele) )
      allocate( deidx_ele(numele) )
      allocate( dzidx_ele(numele) )
      allocate( dxidy_ele(numele) )
      allocate( deidy_ele(numele) )
      allocate( dzidy_ele(numele) )
      allocate( dxidz_ele(numele) )
      allocate( deidz_ele(numele) )
      allocate( dzidz_ele(numele) )
!
      dxidx_ele = 0.0d0
      deidx_ele = 0.0d0
      dzidx_ele = 0.0d0
      dxidy_ele = 0.0d0
      deidy_ele = 0.0d0
      dzidy_ele = 0.0d0
      dxidz_ele = 0.0d0
      deidz_ele = 0.0d0
      dzidz_ele = 0.0d0
!
      end subroutine allocate_dxi_dx_ele
!
!-----------------------------------------------------------------------
!
      subroutine allocate_dxi_dx_nod
!
      use m_geometry_parameter
!
!
      allocate( dxidx_nod(numnod) )
      allocate( deidx_nod(numnod) )
      allocate( dzidx_nod(numnod) )
      allocate( dxidy_nod(numnod) )
      allocate( deidy_nod(numnod) )
      allocate( dzidy_nod(numnod) )
      allocate( dxidz_nod(numnod) )
      allocate( deidz_nod(numnod) )
      allocate( dzidz_nod(numnod) )
!
      dxidx_nod = 0.0d0
      deidx_nod = 0.0d0
      dzidx_nod = 0.0d0
      dxidy_nod = 0.0d0
      deidy_nod = 0.0d0
      dzidy_nod = 0.0d0
      dxidz_nod = 0.0d0
      deidz_nod = 0.0d0
      dzidz_nod = 0.0d0
!
      end subroutine allocate_dxi_dx_nod
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_dxi_dx_ele
!
      deallocate( dxidx_ele )
      deallocate( deidx_ele )
      deallocate( dzidx_ele )
      deallocate( dxidy_ele )
      deallocate( deidy_ele )
      deallocate( dzidy_ele )
      deallocate( dxidz_ele )
      deallocate( deidz_ele )
      deallocate( dzidz_ele )
!
      end subroutine deallocate_dxi_dx_ele
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_dxi_dx_nod
!
      deallocate( dxidx_nod )
      deallocate( deidx_nod )
      deallocate( dzidx_nod )
      deallocate( dxidy_nod )
      deallocate( deidy_nod )
      deallocate( dzidy_nod )
      deallocate( dxidz_nod )
      deallocate( deidz_nod )
      deallocate( dzidz_nod )
!
      end subroutine deallocate_dxi_dx_nod
!
!-----------------------------------------------------------------------
!
      end module m_dxi_dxes_3d_node
