!m_3d_w_filter_coef.f90
!      module m_3d_w_filter_coef
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine allocate_num_w_filtering_comb
!      subroutine allocate_inod_w_fil_comb
!      subroutine allocate_3d_w_fil_comb
!
!      subroutine allocate_stack_vec_w_fil
!      subroutine allocate_istart_vec_w_fil
!
!      subroutine deallocate_num_w_filtering_comb
!      subroutine deallocate_inod_w_fil_comb
!      subroutine deallocate_3d_w_fil_comb
!
!      subroutine deallocate_stack_vec_w_fil
!      subroutine deallocate_istart_vec_w_fil
!
      module m_3d_w_filter_coef
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: ngrp_nod_3d_w_fil
      character(len=kchara), allocatable :: grp_name_3d_w_fil(:)
      integer(kind = kint), allocatable :: num_nod_3d_w_fil(:)
      integer(kind = kint), allocatable :: istack_nod_3d_w_fil(:)
!  number and stacks of node for filtering
!
      integer(kind = kint) :: ntot_nod_3d_w_fil
      integer(kind = kint), allocatable :: inod_3d_w_filter(:)
!
      integer(kind = kint) :: ntot_near_nod_3d_w_fil
      integer(kind = kint), allocatable :: num_near_nod_3d_w_fil(:)
      integer(kind = kint), allocatable :: istack_near_nod_3d_w_fil(:)
      integer(kind = kint), allocatable :: inod_near_nod_3d_w(:)
!
      real(kind = kreal), allocatable :: filter_weight_3d_w(:)
!
!
      integer(kind = kint), allocatable :: min_nsum_3d_w_fil(:)
      integer(kind = kint), allocatable :: max_nsum_3d_w_fil(:)
      integer(kind = kint), allocatable :: istack_nsum_3d_w_fil(:)
!
      integer(kind = kint) :: ntot_nsum_3d_w_fil
      integer(kind = kint), allocatable :: ist_nsum_3d_w_fil(:)
      integer(kind = kint), allocatable :: ied_nsum_3d_w_fil(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_num_w_filtering_comb
!
      allocate(grp_name_3d_w_fil(ngrp_nod_3d_w_fil))
      allocate(num_nod_3d_w_fil(ngrp_nod_3d_w_fil))
      allocate(istack_nod_3d_w_fil(0:ngrp_nod_3d_w_fil))
!
      num_nod_3d_w_fil = 0
      istack_nod_3d_w_fil = 0
!
      end subroutine allocate_num_w_filtering_comb
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_inod_w_fil_comb
!
      allocate(inod_3d_w_filter(ntot_nod_3d_w_fil))
      allocate(num_near_nod_3d_w_fil(ntot_nod_3d_w_fil))
      allocate(istack_near_nod_3d_w_fil(0:ntot_nod_3d_w_fil))
!
      inod_3d_w_filter = 0
      num_near_nod_3d_w_fil = 0
      istack_near_nod_3d_w_fil = 0
!
      end subroutine allocate_inod_w_fil_comb
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_3d_w_fil_comb
!
      allocate(inod_near_nod_3d_w(ntot_near_nod_3d_w_fil))
      allocate(filter_weight_3d_w(ntot_near_nod_3d_w_fil))
!
      inod_near_nod_3d_w = 0
      filter_weight_3d_w = 0.0d0
!
      end subroutine allocate_3d_w_fil_comb
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_stack_vec_w_fil
!
      allocate(min_nsum_3d_w_fil(ngrp_nod_3d_w_fil))
      allocate(max_nsum_3d_w_fil(ngrp_nod_3d_w_fil))
      allocate(istack_nsum_3d_w_fil(0:ngrp_nod_3d_w_fil))
!
      min_nsum_3d_w_fil = 0
      max_nsum_3d_w_fil = 0
      istack_nsum_3d_w_fil = -1
!
      end subroutine allocate_stack_vec_w_fil
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_istart_vec_w_fil
!
      allocate(ist_nsum_3d_w_fil(ntot_nsum_3d_w_fil))
      allocate(ied_nsum_3d_w_fil(ntot_nsum_3d_w_fil))
      ist_nsum_3d_w_fil = 0
      ied_nsum_3d_w_fil = 0
!
      end subroutine allocate_istart_vec_w_fil
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_num_w_filtering_comb
!
      deallocate(grp_name_3d_w_fil)
      deallocate(num_nod_3d_w_fil)
      deallocate(istack_nod_3d_w_fil)
!
      end subroutine deallocate_num_w_filtering_comb
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_inod_w_fil_comb
!
      deallocate(inod_3d_w_filter)
      deallocate(num_near_nod_3d_w_fil)
      deallocate(istack_near_nod_3d_w_fil)
!
      end subroutine deallocate_inod_w_fil_comb
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_3d_w_fil_comb
!
      deallocate(inod_near_nod_3d_w)
      deallocate(filter_weight_3d_w)
!
      end subroutine deallocate_3d_w_fil_comb
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_stack_vec_w_fil
!
      deallocate(min_nsum_3d_w_fil)
      deallocate(max_nsum_3d_w_fil)
      deallocate(istack_nsum_3d_w_fil)
!
      end subroutine deallocate_stack_vec_w_fil
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_istart_vec_w_fil
!
      deallocate(ist_nsum_3d_w_fil)
      deallocate(ied_nsum_3d_w_fil)
!
      end subroutine deallocate_istart_vec_w_fil
!
!  ---------------------------------------------------------------------
!
      end module m_3d_w_filter_coef
