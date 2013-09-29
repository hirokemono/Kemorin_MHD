!
!      module sorting_by_filtering_area
!
      module sorting_by_filtering_area
!
!     Written by H. Matsui on Nov., 2006
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
!      subroutine s_sorting_by_filtering_area
!
!      subroutine copy_3d_filter_stack_no_sort
!      subroutine copy_3d_filtering_no_sorting
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_sorting_by_filtering_area
!
      use m_filter_coef_combained
      use m_filter_coefs
      use cal_minmax_and_stacks
!
      integer(kind = kint), allocatable :: id_org(:)
      integer(kind = kint) :: icou, inum, i, j
      integer(kind = kint) :: ist, ied, i_org, j_org, j_new
!
!
      allocate(id_org(ntot_nod_3d_filter))
      id_org = 0
!
      call allocate_inod_filter_comb
!
!    ordering by number of filtering nodes
!
      icou = 0
      do i = 1, ngrp_nod_3d_filter
        icou = istack_nod_3d_filter(i-1)
        ist = istack_nod_3d_filter(i-1) + 1
        ied = istack_nod_3d_filter(i)
        do j = nmin_nod_near_all_w, nmax_nod_near_all_w
          do inum = ist, ied
            if ( nnod_near_nod_all_w(inum) .eq. j) then
              icou = icou+1
              num_near_nod_3d_filter(icou) = nnod_near_nod_all_w(inum)
              id_org(icou) = inum
            end if
          end do
        end do
      end do
!
!     count stack for filtering positions
!
      do inum = 1, ntot_nod_3d_filter
        i_org = id_org(inum)
        inod_3d_filter(inum) = inod_all_w(i_org)
      end do
!
      call s_cal_total_and_stacks(ntot_nod_3d_filter,                   &
     &    num_near_nod_3d_filter, izero, istack_near_nod_3d_filter,     &
     &    ntot_near_nod_3d_filter)
!
!      copy filter coefficients and weight
!
      call allocate_3d_filter_comb
!
      do inum = 1, ntot_nod_3d_filter
        i_org = id_org(inum)
!
        do i = 1, num_near_nod_3d_filter(inum)
          j_new = istack_near_nod_3d_filter(inum-1) + i
          j_org = inod_stack_nod_all_w(i_org-1) + i
          inod_near_nod_3d(j_new) = inod_near_nod_all_w(j_org)
          filter_func_3d(j_new) =   filter_func(j_org)
          filter_weight_3d(j_new) = filter_weight(j_org)
        end do
!
      end do
!
      deallocate(id_org)
!
      call deallocate_filter_coefs
      call deallocate_nod_ele_near_all_w
!
      end subroutine s_sorting_by_filtering_area
!
! ----------------------------------------------------------------------
!
      subroutine copy_3d_filter_stack_no_sort
!
      use m_filter_coef_combained
      use m_filter_coefs
      use cal_minmax_and_stacks
!
!
      call allocate_inod_filter_comb
!
      inod_3d_filter(1:ntot_nod_3d_filter)                              &
     &      = inod_all_w(1:ntot_nod_3d_filter)
      num_near_nod_3d_filter(1:ntot_nod_3d_filter)                      &
     &      = nnod_near_nod_all_w(1:ntot_nod_3d_filter)
!
      call s_cal_total_and_stacks(ntot_nod_3d_filter,                   &
     &    num_near_nod_3d_filter, izero, istack_near_nod_3d_filter,     &
     &    ntot_near_nod_3d_filter)
!
      end subroutine copy_3d_filter_stack_no_sort
!
! ----------------------------------------------------------------------
!
      subroutine copy_3d_filtering_no_sorting
!
      use m_filter_coef_combained
      use m_filter_coefs
!
!
      call allocate_3d_filter_comb
!
      inod_near_nod_3d(1:ntot_near_nod_3d_filter)                       &
     &      = inod_near_nod_all_w(1:ntot_near_nod_3d_filter)
      filter_weight_3d(1:ntot_near_nod_3d_filter)                       &
     &      = filter_weight(1:ntot_near_nod_3d_filter)
      filter_func_3d(1:ntot_near_nod_3d_filter)                         &
     &      = filter_func(1:ntot_near_nod_3d_filter)
!
      call deallocate_filter_coefs
      call deallocate_nod_ele_near_all_w
!
      end subroutine copy_3d_filtering_no_sorting
!
! ----------------------------------------------------------------------
!
      end module sorting_by_filtering_area
