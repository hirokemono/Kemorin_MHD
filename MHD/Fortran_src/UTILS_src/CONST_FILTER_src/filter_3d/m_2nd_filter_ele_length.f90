!m_2nd_filter_ele_length.f90
!      module m_2nd_filter_ele_length
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine allocate_2nd_ele_length(nele_2nd)
!      subroutine deallocate_2nd_ele_length
!
      module m_2nd_filter_ele_length
!
      use m_precision
      use m_constants
      use t_filter_elength
!
      implicit none
!
!
      real(kind = kreal), allocatable :: elen_dx2_ele_2nd(:)
      real(kind = kreal), allocatable :: elen_dy2_ele_2nd(:)
      real(kind = kreal), allocatable :: elen_dz2_ele_2nd(:)
      real(kind = kreal), allocatable :: elen_dxdy_ele_2nd(:)
      real(kind = kreal), allocatable :: elen_dydz_ele_2nd(:)
      real(kind = kreal), allocatable :: elen_dzdx_ele_2nd(:)
!
      real(kind = kreal), allocatable :: elen_dx2_ele_dx_2nd(:,:)
      real(kind = kreal), allocatable :: elen_dy2_ele_dx_2nd(:,:)
      real(kind = kreal), allocatable :: elen_dz2_ele_dx_2nd(:,:)
      real(kind = kreal), allocatable :: elen_dxdy_ele_dx_2nd(:,:)
      real(kind = kreal), allocatable :: elen_dydz_ele_dx_2nd(:,:)
      real(kind = kreal), allocatable :: elen_dzdx_ele_dx_2nd(:,:)
!
      real(kind = kreal), allocatable :: elen_dx2_ele_dx2_2nd(:,:)
      real(kind = kreal), allocatable :: elen_dy2_ele_dx2_2nd(:,:)
      real(kind = kreal), allocatable :: elen_dz2_ele_dx2_2nd(:,:)
      real(kind = kreal), allocatable :: elen_dxdy_ele_dx2_2nd(:,:)
      real(kind = kreal), allocatable :: elen_dydz_ele_dx2_2nd(:,:)
      real(kind = kreal), allocatable :: elen_dzdx_ele_dx2_2nd(:,:)
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_2nd_ele_length(nele_2nd)
!
      integer(kind = kint), intent(in) :: nele_2nd
!
!
      allocate(elen_dx2_ele_2nd(nele_2nd))
      allocate(elen_dy2_ele_2nd(nele_2nd))
      allocate(elen_dz2_ele_2nd(nele_2nd))
      allocate(elen_dxdy_ele_2nd(nele_2nd))
      allocate(elen_dydz_ele_2nd(nele_2nd))
      allocate(elen_dzdx_ele_2nd(nele_2nd))
!
      allocate(elen_dx2_ele_dx_2nd(nele_2nd,ithree))
      allocate(elen_dy2_ele_dx_2nd(nele_2nd,ithree))
      allocate(elen_dz2_ele_dx_2nd(nele_2nd,ithree))
      allocate(elen_dxdy_ele_dx_2nd(nele_2nd,ithree))
      allocate(elen_dydz_ele_dx_2nd(nele_2nd,ithree))
      allocate(elen_dzdx_ele_dx_2nd(nele_2nd,ithree))
!
      allocate(elen_dx2_ele_dx2_2nd(nele_2nd,ithree))
      allocate(elen_dy2_ele_dx2_2nd(nele_2nd,ithree))
      allocate(elen_dz2_ele_dx2_2nd(nele_2nd,ithree))
      allocate(elen_dxdy_ele_dx2_2nd(nele_2nd,ithree))
      allocate(elen_dydz_ele_dx2_2nd(nele_2nd,ithree))
      allocate(elen_dzdx_ele_dx2_2nd(nele_2nd,ithree))
!
!
      end subroutine allocate_2nd_ele_length
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_2nd_ele_length
!
!
      deallocate(elen_dx2_ele_2nd)
      deallocate(elen_dy2_ele_2nd)
      deallocate(elen_dz2_ele_2nd)
      deallocate(elen_dxdy_ele_2nd)
      deallocate(elen_dydz_ele_2nd)
      deallocate(elen_dzdx_ele_2nd)
!
      deallocate(elen_dx2_ele_dx_2nd)
      deallocate(elen_dy2_ele_dx_2nd)
      deallocate(elen_dz2_ele_dx_2nd)
      deallocate(elen_dxdy_ele_dx_2nd)
      deallocate(elen_dydz_ele_dx_2nd)
      deallocate(elen_dzdx_ele_dx_2nd)
!
      deallocate(elen_dx2_ele_dx2_2nd)
      deallocate(elen_dy2_ele_dx2_2nd)
      deallocate(elen_dz2_ele_dx2_2nd)
      deallocate(elen_dxdy_ele_dx2_2nd)
      deallocate(elen_dydz_ele_dx2_2nd)
      deallocate(elen_dzdx_ele_dx2_2nd)
!
      end subroutine deallocate_2nd_ele_length
!
!   --------------------------------------------------------------------
!
      end module m_2nd_filter_ele_length
