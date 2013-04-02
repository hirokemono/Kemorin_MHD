!
!      module m_2nd_filter_moments
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine allocate_2nd_filter_moms_ele(nele_2nd, num_filter)
!      subroutine deallocate_2nd_filter_moms_ele
!
      module m_2nd_filter_moments
!
      use m_precision
!
      implicit none
!
!
      integer (kind = kint) :: nele_fmom_2nd
      integer (kind = kint) :: num_filter_moms_2nd
!
      real(kind = kreal), allocatable :: filter_x2_ele_2nd(:,:)
      real(kind = kreal), allocatable :: filter_y2_ele_2nd(:,:)
      real(kind = kreal), allocatable :: filter_z2_ele_2nd(:,:)
      real(kind = kreal), allocatable :: filter_xy_ele_2nd(:,:)
      real(kind = kreal), allocatable :: filter_yz_ele_2nd(:,:)
      real(kind = kreal), allocatable :: filter_zx_ele_2nd(:,:)
      real(kind = kreal), allocatable :: filter_x_ele_2nd(:,:)
      real(kind = kreal), allocatable :: filter_y_ele_2nd(:,:)
      real(kind = kreal), allocatable :: filter_z_ele_2nd(:,:)
!
      real(kind = kreal), allocatable :: filter_x2_ele_dx_2nd(:,:,:)
      real(kind = kreal), allocatable :: filter_y2_ele_dx_2nd(:,:,:)
      real(kind = kreal), allocatable :: filter_z2_ele_dx_2nd(:,:,:)
      real(kind = kreal), allocatable :: filter_xy_ele_dx_2nd(:,:,:)
      real(kind = kreal), allocatable :: filter_yz_ele_dx_2nd(:,:,:)
      real(kind = kreal), allocatable :: filter_zx_ele_dx_2nd(:,:,:)
      real(kind = kreal), allocatable :: filter_x_ele_dx_2nd(:,:,:)
      real(kind = kreal), allocatable :: filter_y_ele_dx_2nd(:,:,:)
      real(kind = kreal), allocatable :: filter_z_ele_dx_2nd(:,:,:)
!
      real(kind = kreal), allocatable :: filter_x2_ele_dx2_2nd(:,:,:)
      real(kind = kreal), allocatable :: filter_y2_ele_dx2_2nd(:,:,:)
      real(kind = kreal), allocatable :: filter_z2_ele_dx2_2nd(:,:,:)
      real(kind = kreal), allocatable :: filter_xy_ele_dx2_2nd(:,:,:)
      real(kind = kreal), allocatable :: filter_yz_ele_dx2_2nd(:,:,:)
      real(kind = kreal), allocatable :: filter_zx_ele_dx2_2nd(:,:,:)
      real(kind = kreal), allocatable :: filter_x_ele_dx2_2nd(:,:,:)
      real(kind = kreal), allocatable :: filter_y_ele_dx2_2nd(:,:,:)
      real(kind = kreal), allocatable :: filter_z_ele_dx2_2nd(:,:,:)
!
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_2nd_filter_moms_ele(nele_2nd, num_filter)
!
      use m_constants
!
      integer(kind = kint), intent(in) :: nele_2nd, num_filter
!
!
      nele_fmom_2nd = nele_2nd
!
      allocate(filter_x2_ele_2nd(nele_2nd,num_filter))
      allocate(filter_y2_ele_2nd(nele_2nd,num_filter))
      allocate(filter_z2_ele_2nd(nele_2nd,num_filter))
      allocate(filter_xy_ele_2nd(nele_2nd,num_filter))
      allocate(filter_yz_ele_2nd(nele_2nd,num_filter))
      allocate(filter_zx_ele_2nd(nele_2nd,num_filter))
      allocate(filter_x_ele_2nd(nele_2nd,num_filter))
      allocate(filter_y_ele_2nd(nele_2nd,num_filter))
      allocate(filter_z_ele_2nd(nele_2nd,num_filter))
!
      allocate(filter_x2_ele_dx_2nd(nele_2nd,ithree,num_filter))
      allocate(filter_y2_ele_dx_2nd(nele_2nd,ithree,num_filter))
      allocate(filter_z2_ele_dx_2nd(nele_2nd,ithree,num_filter))
      allocate(filter_xy_ele_dx_2nd(nele_2nd,ithree,num_filter))
      allocate(filter_yz_ele_dx_2nd(nele_2nd,ithree,num_filter))
      allocate(filter_zx_ele_dx_2nd(nele_2nd,ithree,num_filter))
      allocate(filter_x_ele_dx_2nd(nele_2nd,ithree,num_filter))
      allocate(filter_y_ele_dx_2nd(nele_2nd,ithree,num_filter))
      allocate(filter_z_ele_dx_2nd(nele_2nd,ithree,num_filter))
!
      allocate(filter_x2_ele_dx2_2nd(nele_2nd,ithree,num_filter))
      allocate(filter_y2_ele_dx2_2nd(nele_2nd,ithree,num_filter))
      allocate(filter_z2_ele_dx2_2nd(nele_2nd,ithree,num_filter))
      allocate(filter_xy_ele_dx2_2nd(nele_2nd,ithree,num_filter))
      allocate(filter_yz_ele_dx2_2nd(nele_2nd,ithree,num_filter))
      allocate(filter_zx_ele_dx2_2nd(nele_2nd,ithree,num_filter))
      allocate(filter_x_ele_dx2_2nd(nele_2nd,ithree,num_filter))
      allocate(filter_y_ele_dx2_2nd(nele_2nd,ithree,num_filter))
      allocate(filter_z_ele_dx2_2nd(nele_2nd,ithree,num_filter))
!
      end subroutine allocate_2nd_filter_moms_ele
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_2nd_filter_moms_ele
!
!
      deallocate(filter_x2_ele_2nd)
      deallocate(filter_y2_ele_2nd)
      deallocate(filter_z2_ele_2nd)
      deallocate(filter_xy_ele_2nd)
      deallocate(filter_yz_ele_2nd)
      deallocate(filter_zx_ele_2nd)
      deallocate(filter_x_ele_2nd)
      deallocate(filter_y_ele_2nd)
      deallocate(filter_z_ele_2nd)
!
      deallocate(filter_x2_ele_dx_2nd)
      deallocate(filter_y2_ele_dx_2nd)
      deallocate(filter_z2_ele_dx_2nd)
      deallocate(filter_xy_ele_dx_2nd)
      deallocate(filter_yz_ele_dx_2nd)
      deallocate(filter_zx_ele_dx_2nd)
      deallocate(filter_x_ele_dx_2nd)
      deallocate(filter_y_ele_dx_2nd)
      deallocate(filter_z_ele_dx_2nd)
!
      deallocate(filter_x2_ele_dx2_2nd)
      deallocate(filter_y2_ele_dx2_2nd)
      deallocate(filter_z2_ele_dx2_2nd)
      deallocate(filter_xy_ele_dx2_2nd)
      deallocate(filter_yz_ele_dx2_2nd)
      deallocate(filter_zx_ele_dx2_2nd)
      deallocate(filter_x_ele_dx2_2nd)
      deallocate(filter_y_ele_dx2_2nd)
      deallocate(filter_z_ele_dx2_2nd)
!
      end subroutine deallocate_2nd_filter_moms_ele
!
!   --------------------------------------------------------------------
!
      end module m_2nd_filter_moments
