!
!      module m_3d_filter_coef_smp
!
!     Written by H. Matsui on Nov., 2006
!
!      subroutine allocate_nnod_3d_filter_smp(np_smp)
!      subroutine allocate_inod_3d_filter_smp
!      subroutine allocate_3d_filter_coef_smp
!
!      subroutine allocate_stack_vec_fil_smp(np_smp)
!      subroutine allocate_istart_vec_fil_smp
!
!      subroutine deallocate_nnod_3d_filter_smp
!      subroutine deallocate_inod_3d_filter_smp
!      subroutine deallocate_3d_filter_coef_smp
!
!      subroutine deallocate_stack_vec_fil_smp
!      subroutine deallocate_istart_vec_fil_smp
!
      module m_3d_filter_coef_smp
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: ngrp_nod_3d_filter_smp
      character(len=kchara), allocatable :: grp_name_3d_filter_smp(:)
      integer(kind = kint), allocatable :: num_nod_3d_filter_smp(:)
      integer(kind = kint), allocatable :: istack_nod_3d_fil_smp(:)
!  number and stacks of node for filtering
!
      integer(kind = kint) :: ntot_nod_3d_filter_smp
      integer(kind = kint), allocatable :: inod_3d_filter_smp(:)
      integer(kind = kint), allocatable :: num_near_nod_3d_fil_smp(:)
      integer(kind = kint), allocatable :: istack_near_nod_3d_f_smp(:)
!
      integer(kind = kint) :: ntot_near_nod_3d_filter_smp
      integer(kind = kint), allocatable :: inod_near_nod_3d_smp(:)
      real(kind = kreal), allocatable :: filter_weight_3d_smp(:)
!
!
      integer(kind = kint), allocatable :: min_nsum_3d_fil_smp(:)
      integer(kind = kint), allocatable :: max_nsum_3d_fil_smp(:)
      integer(kind = kint), allocatable :: istack_nsum_3d_fil_smp(:)
!
      integer(kind = kint) :: ntot_nsum_3d_fil_smp
      integer(kind = kint), allocatable :: ist_nsum_3d_fil_smp(:)
      integer(kind = kint), allocatable :: ied_nsum_3d_fil_smp(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_nnod_3d_filter_smp(np_smp)
!
      integer(kind = kint), intent(in) :: np_smp
!
      allocate(grp_name_3d_filter_smp(ngrp_nod_3d_filter_smp))
      allocate(num_nod_3d_filter_smp(ngrp_nod_3d_filter_smp*np_smp))
      allocate(istack_nod_3d_fil_smp(0:ngrp_nod_3d_filter_smp*np_smp))
!
      num_nod_3d_filter_smp =  0
      istack_nod_3d_fil_smp = -1
!
      end subroutine allocate_nnod_3d_filter_smp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_inod_3d_filter_smp
!
      allocate(inod_3d_filter_smp(ntot_nod_3d_filter_smp))
      allocate(num_near_nod_3d_fil_smp(ntot_nod_3d_filter_smp))
      allocate(istack_near_nod_3d_f_smp(0:ntot_nod_3d_filter_smp))
!
      inod_3d_filter_smp =        0
      num_near_nod_3d_fil_smp =   0
      istack_near_nod_3d_f_smp = -1
!
      end subroutine allocate_inod_3d_filter_smp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_3d_filter_coef_smp
!
      allocate(inod_near_nod_3d_smp(ntot_near_nod_3d_filter_smp))
      allocate(filter_weight_3d_smp(ntot_near_nod_3d_filter_smp))
!
      inod_near_nod_3d_smp = 0
      filter_weight_3d_smp = 0.0d0
!
      end subroutine allocate_3d_filter_coef_smp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_stack_vec_fil_smp(np_smp)
!
      integer(kind = kint), intent(in) :: np_smp
!
      allocate(min_nsum_3d_fil_smp(ngrp_nod_3d_filter_smp*np_smp))
      allocate(max_nsum_3d_fil_smp(ngrp_nod_3d_filter_smp*np_smp))
      allocate(istack_nsum_3d_fil_smp(0:ngrp_nod_3d_filter_smp*np_smp))
!
      min_nsum_3d_fil_smp = 0
      max_nsum_3d_fil_smp = 0
      istack_nsum_3d_fil_smp = -1
!
      end subroutine allocate_stack_vec_fil_smp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_istart_vec_fil_smp
!
      allocate(ist_nsum_3d_fil_smp(ntot_nsum_3d_fil_smp))
      allocate(ied_nsum_3d_fil_smp(ntot_nsum_3d_fil_smp))
      ist_nsum_3d_fil_smp = 0
      ied_nsum_3d_fil_smp = 0
!
      end subroutine allocate_istart_vec_fil_smp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_nnod_3d_filter_smp
!
      deallocate(grp_name_3d_filter_smp)
      deallocate(num_nod_3d_filter_smp)
      deallocate(istack_nod_3d_fil_smp)
!
      end subroutine deallocate_nnod_3d_filter_smp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_inod_3d_filter_smp
!
      deallocate(inod_3d_filter_smp)
      deallocate(num_near_nod_3d_fil_smp)
      deallocate(istack_near_nod_3d_f_smp)
!
      end subroutine deallocate_inod_3d_filter_smp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_3d_filter_coef_smp
!
      deallocate(inod_near_nod_3d_smp)
      deallocate(filter_weight_3d_smp)
!
      end subroutine deallocate_3d_filter_coef_smp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_stack_vec_fil_smp
!
      deallocate(min_nsum_3d_fil_smp)
      deallocate(max_nsum_3d_fil_smp)
      deallocate(istack_nsum_3d_fil_smp)
!
      end subroutine deallocate_stack_vec_fil_smp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_istart_vec_fil_smp
!
      deallocate(ist_nsum_3d_fil_smp)
      deallocate(ied_nsum_3d_fil_smp)
!
      end subroutine deallocate_istart_vec_fil_smp
!
!  ---------------------------------------------------------------------
!
      end module m_3d_filter_coef_smp
