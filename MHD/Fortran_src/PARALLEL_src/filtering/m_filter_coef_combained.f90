!
!      module m_filter_coef_combained
!
!     Written by H. Matsui on Nov., 2006
!
!
!      subroutine allocate_num_filtering_comb
!      subroutine allocate_inod_filter_comb
!      subroutine allocate_3d_filter_comb
!
!      subroutine allocate_stack_vec_filter
!      subroutine allocate_istart_vec_filter
!
!      subroutine deallocate_num_filtering_comb
!      subroutine deallocate_inod_filter_comb
!      subroutine deallocate_3d_filter_func_comb
!      subroutine deallocate_3d_filter_comb
!
!      subroutine deallocate_stack_vec_filter
!      subroutine deallocate_istart_vec_filter
!
      module m_filter_coef_combained
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: ngrp_nod_3d_filter
      character(len=kchara), allocatable :: grp_name_3d_filter(:)
      integer(kind = kint), allocatable :: num_nod_3d_filter(:)
      integer(kind = kint), allocatable :: istack_nod_3d_filter(:)
!  number and stacks of node for filtering
!
      integer(kind = kint) :: ntot_nod_3d_filter
      integer(kind = kint), allocatable :: inod_3d_filter(:)
!
      integer(kind = kint) :: ntot_near_nod_3d_filter
      integer(kind = kint), allocatable :: num_near_nod_3d_filter(:)
      integer(kind = kint), allocatable :: istack_near_nod_3d_filter(:)
      integer(kind = kint), allocatable :: inod_near_nod_3d(:)
!
      real(kind = kreal), allocatable :: filter_func_3d(:)
      real(kind = kreal), allocatable :: filter_weight_3d(:)
!
!
      integer(kind = kint), allocatable :: min_nsum_3d_filter(:)
      integer(kind = kint), allocatable :: max_nsum_3d_filter(:)
      integer(kind = kint), allocatable :: istack_nsum_3d_filter(:)
!
      integer(kind = kint) :: ntot_nsum_3d_filter
      integer(kind = kint), allocatable :: ist_nsum_3d_filter(:)
      integer(kind = kint), allocatable :: ied_nsum_3d_filter(:)
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_num_filtering_comb
!
      allocate(grp_name_3d_filter(ngrp_nod_3d_filter))
      allocate(num_nod_3d_filter(ngrp_nod_3d_filter))
      allocate(istack_nod_3d_filter(0:ngrp_nod_3d_filter))
!
      num_nod_3d_filter = 0
      istack_nod_3d_filter = 0
!
      end subroutine allocate_num_filtering_comb
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_inod_filter_comb
!
      allocate(inod_3d_filter(ntot_nod_3d_filter))
      allocate(num_near_nod_3d_filter(ntot_nod_3d_filter))
      allocate(istack_near_nod_3d_filter(0:ntot_nod_3d_filter))
!
      inod_3d_filter = 0
      num_near_nod_3d_filter = 0
      istack_near_nod_3d_filter = 0
!
      end subroutine allocate_inod_filter_comb
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_3d_filter_comb
!
      allocate(inod_near_nod_3d(ntot_near_nod_3d_filter))
      allocate(filter_func_3d(ntot_near_nod_3d_filter))
      allocate(filter_weight_3d(ntot_near_nod_3d_filter))
!
      inod_near_nod_3d = 0
      filter_func_3d = 0.0d0
      filter_weight_3d = 0.0d0
!
      end subroutine allocate_3d_filter_comb
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_stack_vec_filter
!
      allocate(min_nsum_3d_filter(ngrp_nod_3d_filter))
      allocate(max_nsum_3d_filter(ngrp_nod_3d_filter))
      allocate(istack_nsum_3d_filter(0:ngrp_nod_3d_filter))
!
      min_nsum_3d_filter = 0
      max_nsum_3d_filter = 0
      istack_nsum_3d_filter = -1
!
      end subroutine allocate_stack_vec_filter
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_istart_vec_filter
!
      allocate(ist_nsum_3d_filter(ntot_nsum_3d_filter))
      allocate(ied_nsum_3d_filter(ntot_nsum_3d_filter))
      ist_nsum_3d_filter = 0
      ied_nsum_3d_filter = 0
!
      end subroutine allocate_istart_vec_filter
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_num_filtering_comb
!
      deallocate(grp_name_3d_filter)
      deallocate(num_nod_3d_filter)
      deallocate(istack_nod_3d_filter)
!
      end subroutine deallocate_num_filtering_comb
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_inod_filter_comb
!
      deallocate(inod_3d_filter)
      deallocate(num_near_nod_3d_filter)
      deallocate(istack_near_nod_3d_filter)
!
      end subroutine deallocate_inod_filter_comb
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_3d_filter_func_comb
!
      deallocate(filter_func_3d)
!
      end subroutine deallocate_3d_filter_func_comb
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_3d_filter_comb
!
      deallocate(inod_near_nod_3d)
      deallocate(filter_weight_3d)
!
      end subroutine deallocate_3d_filter_comb
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_stack_vec_filter
!
      deallocate(min_nsum_3d_filter)
      deallocate(max_nsum_3d_filter)
      deallocate(istack_nsum_3d_filter)
!
      end subroutine deallocate_stack_vec_filter
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_istart_vec_filter
!
      deallocate(ist_nsum_3d_filter)
      deallocate(ied_nsum_3d_filter)
!
      end subroutine deallocate_istart_vec_filter
!
!  ---------------------------------------------------------------------
!
      end module m_filter_coef_combained
