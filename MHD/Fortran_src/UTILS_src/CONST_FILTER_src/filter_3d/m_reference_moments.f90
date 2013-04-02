!
!     module m_reference_moments
!
      module m_reference_moments
!
!     Written by H. Matsui on Aug., 2006
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: num_order_3d
      integer(kind = kint) :: max_num_order_3d
!
      integer(kind = kint), allocatable :: id_moments(:,:)
      integer(kind = kint), allocatable :: iorder_mom_3d(:,:)
!
      real(kind = kreal), allocatable :: ref_moments_1d(:)
      real(kind = kreal), allocatable :: seed_moments(:)
!
      integer(kind = kint) :: num_order_1d
      integer(kind = kint) :: max_num_order_1d
      integer(kind = kint) :: ntot_power
      integer(kind = kint), allocatable :: istack_power(:)
      integer(kind = kint), allocatable :: itbl_power(:,:)
      real(kind = kreal), allocatable :: coef_x(:)
      real(kind = kreal), allocatable :: coef_y(:)
      real(kind = kreal), allocatable :: coef_z(:)
!
      integer(kind = kint), allocatable :: ipower_x_xi(:)
      integer(kind = kint), allocatable :: ipower_x_ei(:)
      integer(kind = kint), allocatable :: ipower_x_zi(:)
      integer(kind = kint), allocatable :: ipower_y_xi(:)
      integer(kind = kint), allocatable :: ipower_y_ei(:)
      integer(kind = kint), allocatable :: ipower_y_zi(:)
      integer(kind = kint), allocatable :: ipower_z_xi(:)
      integer(kind = kint), allocatable :: ipower_z_ei(:)
      integer(kind = kint), allocatable :: ipower_z_zi(:)
!
      real(kind = kreal), allocatable :: seed_moments_ele(:,:)
      real(kind = kreal), allocatable :: seed_moments_nod(:,:)
!
!
!      subroutine allocate_reference_moments
!      subroutine deallocate_reference_moments
!
!      subroutine allocate_seed_moms_ele
!      subroutine deallocate_seed_moms_ele
!      subroutine allocate_seed_moms_nod
!      subroutine deallocate_seed_moms_nod
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_reference_moments
!
      allocate( id_moments(num_order_3d,3) )
      allocate( iorder_mom_3d(num_order_3d,3) )
      allocate( seed_moments(num_order_3d) )
!
      id_moments = 0
      iorder_mom_3d = 0
      seed_moments = 0.0d0
!
      end subroutine allocate_reference_moments
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_reference_moments
!
      deallocate( id_moments )
      deallocate( iorder_mom_3d )
      deallocate( seed_moments )
!
      end subroutine deallocate_reference_moments
!
! ----------------------------------------------------------------------
!
      subroutine allocate_istack_power
!
      allocate( istack_power(-1:max_num_order_1d) )
      istack_power = -1
!
      end subroutine allocate_istack_power
!
! ----------------------------------------------------------------------
!
      subroutine allocate_itbl_power
!
      allocate( itbl_power(3,0:ntot_power) )
      itbl_power = 0
!
      end subroutine allocate_itbl_power
!
! ----------------------------------------------------------------------
!
      subroutine allocate_coef_4_filter_moms
!
      allocate( ref_moments_1d(0:3*max_num_order_1d) )
      allocate( coef_x(0:ntot_power) )
      allocate( coef_y(0:ntot_power) )
      allocate( coef_z(0:ntot_power) )
!
      allocate( ipower_x_xi(0:ntot_power) )
      allocate( ipower_x_ei(0:ntot_power) )
      allocate( ipower_x_zi(0:ntot_power) )
      allocate( ipower_y_xi(0:ntot_power) )
      allocate( ipower_y_ei(0:ntot_power) )
      allocate( ipower_y_zi(0:ntot_power) )
      allocate( ipower_z_xi(0:ntot_power) )
      allocate( ipower_z_ei(0:ntot_power) )
      allocate( ipower_z_zi(0:ntot_power) )
!
      ref_moments_1d = 0.0d0
      coef_x = 0.0d0
      coef_y = 0.0d0
      coef_z = 0.0d0
!
      ipower_x_xi = 0
      ipower_x_ei = 0
      ipower_x_zi = 0
      ipower_y_xi = 0
      ipower_y_ei = 0
      ipower_y_zi = 0
      ipower_z_xi = 0
      ipower_z_ei = 0
      ipower_z_zi = 0
!
      end subroutine allocate_coef_4_filter_moms
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_filter_moms
!
      deallocate( coef_x )
      deallocate( coef_y )
      deallocate( coef_z )
      deallocate( ref_moments_1d )
      deallocate( itbl_power )
      deallocate( istack_power )
!
      deallocate( ipower_x_xi )
      deallocate( ipower_x_ei )
      deallocate( ipower_x_zi )
      deallocate( ipower_y_xi )
      deallocate( ipower_y_ei )
      deallocate( ipower_y_zi )
      deallocate( ipower_z_xi )
      deallocate( ipower_z_ei )
      deallocate( ipower_z_zi )
!
      end subroutine deallocate_coef_4_filter_moms
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_seed_moms_ele
!
      use m_filter_elength
!
      allocate( seed_moments_ele(nele_filter_mom,num_order_3d) )
      seed_moments_ele = 0.0d0
!
      end subroutine allocate_seed_moms_ele
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_seed_moms_ele
!
      deallocate( seed_moments_ele )
!
      end subroutine deallocate_seed_moms_ele
!
!-----------------------------------------------------------------------
!
      subroutine allocate_seed_moms_nod
!
      use m_filter_elength
!
      allocate( seed_moments_nod(nnod_filter_mom,num_order_3d) )
      seed_moments_nod = 0.0d0
!
      end subroutine allocate_seed_moms_nod
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_seed_moms_nod
!
      deallocate( seed_moments_nod )
!
      end subroutine deallocate_seed_moms_nod
!
!-----------------------------------------------------------------------
!
      end module m_reference_moments
