!m_filter_dxdxi.f90
!      module m_filter_dxdxi
!
!     Written by H. Matsui
!
!       subroutine allocate_jacobians_on_node
!       subroutine allocate_jacobians_for_ele
!       subroutine deallocate_jacobians_on_node
!       subroutine deallocate_jacobians_for_ele
!
      module m_filter_dxdxi
!
      use m_precision
!
      implicit none
!
      real(kind=kreal),   allocatable :: dxdxi_nod(:)
      real(kind=kreal),   allocatable :: dxdei_nod(:)
      real(kind=kreal),   allocatable :: dxdzi_nod(:)
      real(kind=kreal),   allocatable :: dydxi_nod(:)
      real(kind=kreal),   allocatable :: dydei_nod(:)
      real(kind=kreal),   allocatable :: dydzi_nod(:)
      real(kind=kreal),   allocatable :: dzdxi_nod(:)
      real(kind=kreal),   allocatable :: dzdei_nod(:)
      real(kind=kreal),   allocatable :: dzdzi_nod(:)
!          1st difference of elen_nod
!              (node ID, direction of diffrence)
!
      real(kind=kreal),   allocatable :: dxdxi_ele(:)
      real(kind=kreal),   allocatable :: dxdei_ele(:)
      real(kind=kreal),   allocatable :: dxdzi_ele(:)
      real(kind=kreal),   allocatable :: dydxi_ele(:)
      real(kind=kreal),   allocatable :: dydei_ele(:)
      real(kind=kreal),   allocatable :: dydzi_ele(:)
      real(kind=kreal),   allocatable :: dzdxi_ele(:)
      real(kind=kreal),   allocatable :: dzdei_ele(:)
      real(kind=kreal),   allocatable :: dzdzi_ele(:)
!          1st difference of elen_ele
!              (element ID, direction of diffrence)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_jacobians_on_node
!
      use m_filter_elength
!
!
      allocate(dxdxi_nod(nnod_filter_mom))
      allocate(dxdei_nod(nnod_filter_mom))
      allocate(dxdzi_nod(nnod_filter_mom))
      allocate(dydxi_nod(nnod_filter_mom))
      allocate(dydei_nod(nnod_filter_mom))
      allocate(dydzi_nod(nnod_filter_mom))
      allocate(dzdxi_nod(nnod_filter_mom))
      allocate(dzdei_nod(nnod_filter_mom))
      allocate(dzdzi_nod(nnod_filter_mom))
!
      dxdxi_nod = 0.0d0
      dxdei_nod = 0.0d0
      dxdzi_nod = 0.0d0
      dydxi_nod = 0.0d0
      dydei_nod = 0.0d0
      dydzi_nod = 0.0d0
      dzdxi_nod = 0.0d0
      dzdei_nod = 0.0d0
      dzdzi_nod = 0.0d0
!
      end subroutine allocate_jacobians_on_node
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_jacobians_for_ele
!
      use m_filter_elength
!
!
      allocate(dxdxi_ele(nele_filter_mom))
      allocate(dxdei_ele(nele_filter_mom))
      allocate(dxdzi_ele(nele_filter_mom))
      allocate(dydxi_ele(nele_filter_mom))
      allocate(dydei_ele(nele_filter_mom))
      allocate(dydzi_ele(nele_filter_mom))
      allocate(dzdxi_ele(nele_filter_mom))
      allocate(dzdei_ele(nele_filter_mom))
      allocate(dzdzi_ele(nele_filter_mom))
!
      dxdxi_ele = 0.0d0
      dxdei_ele = 0.0d0
      dxdzi_ele = 0.0d0
      dydxi_ele = 0.0d0
      dydei_ele = 0.0d0
      dydzi_ele = 0.0d0
      dzdxi_ele = 0.0d0
      dzdei_ele = 0.0d0
      dzdzi_ele = 0.0d0
!
      end subroutine allocate_jacobians_for_ele
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_jacobians_on_node
!
      deallocate(dxdxi_nod)
      deallocate(dxdei_nod)
      deallocate(dxdzi_nod)
      deallocate(dydxi_nod)
      deallocate(dydei_nod)
      deallocate(dydzi_nod)
      deallocate(dzdxi_nod)
      deallocate(dzdei_nod)
      deallocate(dzdzi_nod)
!
      end subroutine deallocate_jacobians_on_node
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_jacobians_for_ele
!
      deallocate(dxdxi_ele)
      deallocate(dxdei_ele)
      deallocate(dxdzi_ele)
      deallocate(dydxi_ele)
      deallocate(dydei_ele)
      deallocate(dydzi_ele)
      deallocate(dzdxi_ele)
      deallocate(dzdei_ele)
      deallocate(dzdzi_ele)
!
      end subroutine deallocate_jacobians_for_ele
!
!  ---------------------------------------------------------------------
!
      end module m_filter_dxdxi
