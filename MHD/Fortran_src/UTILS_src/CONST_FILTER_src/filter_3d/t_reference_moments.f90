!
!     module t_reference_moments
!
!     Written by H. Matsui on Aug., 2006
!
!!      subroutine alloc_reference_moments(ref_m)
!!      subroutine dealloc_reference_moments(ref_m)
!!
!!      subroutine alloc_seed_moms_ele(nele_filter_mom, ref_m)
!!      subroutine dealloc_seed_moms_ele(ref_m)
!!      subroutine alloc_seed_moms_nod(nnod_filter_mom, ref_m)
!!      subroutine dealloc_seed_moms_nod(ref_m)
!
      module t_reference_moments
!
      use m_precision
!
      implicit none
!
      type reference_moments
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
!
        real(kind = kreal), allocatable :: seed_moments_ele(:,:)
        real(kind = kreal), allocatable :: seed_moments_nod(:,:)
      end type reference_moments
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_reference_moments(ref_m)
!
      type(reference_moments), intent(inout) :: ref_m
!
!
      allocate( ref_m%id_moments(ref_m%num_order_3d,3) )
      allocate( ref_m%iorder_mom_3d(ref_m%num_order_3d,3) )
      allocate( ref_m%seed_moments(ref_m%num_order_3d) )
!
      ref_m%id_moments = 0
      ref_m%iorder_mom_3d = 0
      ref_m%seed_moments = 0.0d0
!
      end subroutine alloc_reference_moments
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_reference_moments(ref_m)
!
      type(reference_moments), intent(inout) :: ref_m
!
!
      deallocate( ref_m%id_moments )
      deallocate( ref_m%iorder_mom_3d )
      deallocate( ref_m%seed_moments )
!
      end subroutine dealloc_reference_moments
!
! ----------------------------------------------------------------------
!
      subroutine alloc_coef_4_filter_moms(ref_m)
!
      type(reference_moments), intent(inout) :: ref_m
!
!
      allocate( ref_m%ref_moments_1d(0:3*ref_m%max_num_order_1d) )
      ref_m%ref_moments_1d = 0.0d0
!
      end subroutine alloc_coef_4_filter_moms
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_coef_4_filter_moms(ref_m)
!
      type(reference_moments), intent(inout) :: ref_m
!
!
      deallocate( ref_m%ref_moments_1d )
!
      end subroutine dealloc_coef_4_filter_moms
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_seed_moms_ele(nele_filter_mom, ref_m)
!
      integer(kind = kint), intent(in) :: nele_filter_mom
      type(reference_moments), intent(inout) :: ref_m
!
      integer(kind = kint) :: num
!
!
      num = ref_m%num_order_3d
      allocate( ref_m%seed_moments_ele(nele_filter_mom,num) )
      ref_m%seed_moments_ele = 0.0d0
!
      end subroutine alloc_seed_moms_ele
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_seed_moms_ele(ref_m)
!
      type(reference_moments), intent(inout) :: ref_m
!
!
      deallocate( ref_m%seed_moments_ele )
!
      end subroutine dealloc_seed_moms_ele
!
!-----------------------------------------------------------------------
!
      subroutine alloc_seed_moms_nod(nnod_filter_mom, ref_m)
!
      integer(kind = kint), intent(in) :: nnod_filter_mom
      type(reference_moments), intent(inout) :: ref_m
!
      integer(kind = kint) :: num
!
!
      num = ref_m%num_order_3d
      allocate( ref_m%seed_moments_nod(nnod_filter_mom,num) )
      ref_m%seed_moments_nod = 0.0d0
!
      end subroutine alloc_seed_moms_nod
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_seed_moms_nod(ref_m)
!
      type(reference_moments), intent(inout) :: ref_m
!
!
      deallocate( ref_m%seed_moments_nod )
!
      end subroutine dealloc_seed_moms_nod
!
!-----------------------------------------------------------------------
!
      end module t_reference_moments
