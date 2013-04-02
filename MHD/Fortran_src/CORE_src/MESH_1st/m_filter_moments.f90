!
!      module m_filter_moments
!
!     Written by H. Matsui
!
!      subroutine allocate_filter_moms_nod
!      subroutine allocate_filter_moms_ele
!
!      subroutine deallocate_filter_moms_nod
!      subroutine deallocate_filter_moms_ele
!
      module m_filter_moments
!
      use m_precision
!
      implicit none
!
      integer (kind = kint) :: nnod_fmom, nele_fmom
      integer (kind = kint) :: num_filter_moms
!
      real(kind=kreal),   allocatable :: filter_x_nod(:,:)
      real(kind=kreal),   allocatable :: filter_y_nod(:,:)
      real(kind=kreal),   allocatable :: filter_z_nod(:,:)
      real(kind=kreal),   allocatable :: filter_x2_nod(:,:)
      real(kind=kreal),   allocatable :: filter_y2_nod(:,:)
      real(kind=kreal),   allocatable :: filter_z2_nod(:,:)
      real(kind=kreal),   allocatable :: filter_xy_nod(:,:)
      real(kind=kreal),   allocatable :: filter_yz_nod(:,:)
      real(kind=kreal),   allocatable :: filter_zx_nod(:,:)
!
      real(kind=kreal),   allocatable :: filter_x_nod_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_y_nod_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_z_nod_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_x2_nod_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_y2_nod_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_z2_nod_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_xy_nod_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_yz_nod_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_zx_nod_dx(:,:,:)
!
!
      real(kind=kreal),   allocatable :: filter_x_ele(:,:)
      real(kind=kreal),   allocatable :: filter_y_ele(:,:)
      real(kind=kreal),   allocatable :: filter_z_ele(:,:)
      real(kind=kreal),   allocatable :: filter_x2_ele(:,:)
      real(kind=kreal),   allocatable :: filter_y2_ele(:,:)
      real(kind=kreal),   allocatable :: filter_z2_ele(:,:)
      real(kind=kreal),   allocatable :: filter_xy_ele(:,:)
      real(kind=kreal),   allocatable :: filter_yz_ele(:,:)
      real(kind=kreal),   allocatable :: filter_zx_ele(:,:)
!
      real(kind=kreal),   allocatable :: filter_x_ele_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_y_ele_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_z_ele_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_x2_ele_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_y2_ele_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_z2_ele_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_xy_ele_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_yz_ele_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_zx_ele_dx(:,:,:)
!
      real(kind=kreal),   allocatable :: filter_x_ele_dx2(:,:,:)
      real(kind=kreal),   allocatable :: filter_y_ele_dx2(:,:,:)
      real(kind=kreal),   allocatable :: filter_z_ele_dx2(:,:,:)
      real(kind=kreal),   allocatable :: filter_x2_ele_dx2(:,:,:)
      real(kind=kreal),   allocatable :: filter_y2_ele_dx2(:,:,:)
      real(kind=kreal),   allocatable :: filter_z2_ele_dx2(:,:,:)
      real(kind=kreal),   allocatable :: filter_xy_ele_dx2(:,:,:)
      real(kind=kreal),   allocatable :: filter_yz_ele_dx2(:,:,:)
      real(kind=kreal),   allocatable :: filter_zx_ele_dx2(:,:,:)
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_filter_moms_nod(nnod)
!
      integer(kind = kint), intent(in) :: nnod
!
!
      nnod_fmom = nnod
      allocate( filter_x_nod(nnod_fmom,num_filter_moms) )
      allocate( filter_y_nod(nnod_fmom,num_filter_moms) )
      allocate( filter_z_nod(nnod_fmom,num_filter_moms) )
      allocate( filter_x2_nod(nnod_fmom,num_filter_moms) )
      allocate( filter_y2_nod(nnod_fmom,num_filter_moms) )
      allocate( filter_z2_nod(nnod_fmom,num_filter_moms) )
      allocate( filter_xy_nod(nnod_fmom,num_filter_moms) )
      allocate( filter_yz_nod(nnod_fmom,num_filter_moms) )
      allocate( filter_zx_nod(nnod_fmom,num_filter_moms) )
!
      allocate( filter_x_nod_dx(nnod_fmom,3,num_filter_moms) )
      allocate( filter_y_nod_dx(nnod_fmom,3,num_filter_moms) )
      allocate( filter_z_nod_dx(nnod_fmom,3,num_filter_moms) )
      allocate( filter_x2_nod_dx(nnod_fmom,3,num_filter_moms) )
      allocate( filter_y2_nod_dx(nnod_fmom,3,num_filter_moms) )
      allocate( filter_z2_nod_dx(nnod_fmom,3,num_filter_moms) )
      allocate( filter_xy_nod_dx(nnod_fmom,3,num_filter_moms) )
      allocate( filter_yz_nod_dx(nnod_fmom,3,num_filter_moms) )
      allocate( filter_zx_nod_dx(nnod_fmom,3,num_filter_moms) )
!
      filter_x_nod = 0.0d0
      filter_y_nod = 0.0d0
      filter_z_nod = 0.0d0
      filter_x2_nod = 0.0d0
      filter_y2_nod = 0.0d0
      filter_z2_nod = 0.0d0
      filter_xy_nod = 0.0d0
      filter_yz_nod = 0.0d0
      filter_zx_nod = 0.0d0
!
      filter_x_nod_dx = 0.0d0
      filter_y_nod_dx = 0.0d0
      filter_z_nod_dx = 0.0d0
      filter_x2_nod_dx = 0.0d0
      filter_y2_nod_dx = 0.0d0
      filter_z2_nod_dx = 0.0d0
      filter_xy_nod_dx = 0.0d0
      filter_yz_nod_dx = 0.0d0
      filter_zx_nod_dx = 0.0d0
!
      end subroutine allocate_filter_moms_nod
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_filter_moms_ele(nele)
!
      integer(kind = kint), intent(in) :: nele
!
!
      nele_fmom = nele
      allocate( filter_x_ele(nele_fmom,num_filter_moms) )
      allocate( filter_y_ele(nele_fmom,num_filter_moms) )
      allocate( filter_z_ele(nele_fmom,num_filter_moms) )
      allocate( filter_x2_ele(nele_fmom,num_filter_moms) )
      allocate( filter_y2_ele(nele_fmom,num_filter_moms) )
      allocate( filter_z2_ele(nele_fmom,num_filter_moms) )
      allocate( filter_xy_ele(nele_fmom,num_filter_moms) )
      allocate( filter_yz_ele(nele_fmom,num_filter_moms) )
      allocate( filter_zx_ele(nele_fmom,num_filter_moms) )
!
      allocate( filter_x_ele_dx(nele_fmom,3,num_filter_moms) )
      allocate( filter_y_ele_dx(nele_fmom,3,num_filter_moms) )
      allocate( filter_z_ele_dx(nele_fmom,3,num_filter_moms) )
      allocate( filter_x2_ele_dx(nele_fmom,3,num_filter_moms) )
      allocate( filter_y2_ele_dx(nele_fmom,3,num_filter_moms) )
      allocate( filter_z2_ele_dx(nele_fmom,3,num_filter_moms) )
      allocate( filter_xy_ele_dx(nele_fmom,3,num_filter_moms) )
      allocate( filter_yz_ele_dx(nele_fmom,3,num_filter_moms) )
      allocate( filter_zx_ele_dx(nele_fmom,3,num_filter_moms) )
!
      allocate( filter_x_ele_dx2(nele_fmom,3,num_filter_moms) )
      allocate( filter_y_ele_dx2(nele_fmom,3,num_filter_moms) )
      allocate( filter_z_ele_dx2(nele_fmom,3,num_filter_moms) )
      allocate( filter_x2_ele_dx2(nele_fmom,3,num_filter_moms) )
      allocate( filter_y2_ele_dx2(nele_fmom,3,num_filter_moms) )
      allocate( filter_z2_ele_dx2(nele_fmom,3,num_filter_moms) )
      allocate( filter_xy_ele_dx2(nele_fmom,3,num_filter_moms) )
      allocate( filter_yz_ele_dx2(nele_fmom,3,num_filter_moms) )
      allocate( filter_zx_ele_dx2(nele_fmom,3,num_filter_moms) )
!
      filter_x_ele =  0.0d0
      filter_y_ele =  0.0d0
      filter_z_ele =  0.0d0
      filter_x2_ele = 0.0d0
      filter_y2_ele = 0.0d0
      filter_z2_ele = 0.0d0
      filter_xy_ele = 0.0d0
      filter_yz_ele = 0.0d0
      filter_zx_ele = 0.0d0
!
      filter_x_ele_dx =  0.0d0
      filter_y_ele_dx =  0.0d0
      filter_z_ele_dx =  0.0d0
      filter_x2_ele_dx = 0.0d0
      filter_y2_ele_dx = 0.0d0
      filter_z2_ele_dx = 0.0d0
      filter_xy_ele_dx = 0.0d0
      filter_yz_ele_dx = 0.0d0
      filter_zx_ele_dx = 0.0d0
!
      filter_x_ele_dx2 =  0.0d0
      filter_y_ele_dx2 =  0.0d0
      filter_z_ele_dx2 =  0.0d0
      filter_x2_ele_dx2 = 0.0d0
      filter_y2_ele_dx2 = 0.0d0
      filter_z2_ele_dx2 = 0.0d0
      filter_xy_ele_dx2 = 0.0d0
      filter_yz_ele_dx2 = 0.0d0
      filter_zx_ele_dx2 = 0.0d0
!
      end subroutine allocate_filter_moms_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_filter_moms_nod
!
      deallocate( filter_x_nod )
      deallocate( filter_y_nod )
      deallocate( filter_z_nod )
      deallocate( filter_x2_nod )
      deallocate( filter_y2_nod )
      deallocate( filter_z2_nod )
      deallocate( filter_xy_nod )
      deallocate( filter_yz_nod )
      deallocate( filter_zx_nod )
!
      deallocate( filter_x_nod_dx )
      deallocate( filter_y_nod_dx )
      deallocate( filter_z_nod_dx )
      deallocate( filter_x2_nod_dx )
      deallocate( filter_y2_nod_dx )
      deallocate( filter_z2_nod_dx )
      deallocate( filter_xy_nod_dx )
      deallocate( filter_yz_nod_dx )
      deallocate( filter_zx_nod_dx )
!
      end subroutine deallocate_filter_moms_nod
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_filter_moms_ele
!
      deallocate( filter_x_ele )
      deallocate( filter_y_ele )
      deallocate( filter_z_ele )
      deallocate( filter_x2_ele )
      deallocate( filter_y2_ele )
      deallocate( filter_z2_ele )
      deallocate( filter_xy_ele )
      deallocate( filter_yz_ele )
      deallocate( filter_zx_ele )
!
      deallocate( filter_x_ele_dx )
      deallocate( filter_y_ele_dx )
      deallocate( filter_z_ele_dx )
      deallocate( filter_x2_ele_dx )
      deallocate( filter_y2_ele_dx )
      deallocate( filter_z2_ele_dx )
      deallocate( filter_xy_ele_dx )
      deallocate( filter_yz_ele_dx )
      deallocate( filter_zx_ele_dx )
!
      deallocate( filter_x_ele_dx2 )
      deallocate( filter_y_ele_dx2 )
      deallocate( filter_z_ele_dx2 )
      deallocate( filter_x2_ele_dx2 )
      deallocate( filter_y2_ele_dx2 )
      deallocate( filter_z2_ele_dx2 )
      deallocate( filter_xy_ele_dx2 )
      deallocate( filter_yz_ele_dx2 )
      deallocate( filter_zx_ele_dx2 )
!
      end subroutine deallocate_filter_moms_ele
!
!  ---------------------------------------------------------------------
!
      end module m_filter_moments
