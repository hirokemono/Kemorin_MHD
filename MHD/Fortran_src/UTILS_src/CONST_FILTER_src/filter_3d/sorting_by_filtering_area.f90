!
!      module sorting_by_filtering_area
!
!     Written by H. Matsui on Nov., 2006
!
!      subroutine s_sorting_by_filtering_area(filter)
!
!      subroutine copy_3d_filter_stack_no_sort(filter)
!      subroutine copy_3d_filtering_no_sorting(filter)
!
      module sorting_by_filtering_area
!
      use m_precision
      use m_constants
      use calypso_mpi
      use t_filter_coefficients
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_sorting_by_filtering_area(filter)
!
      use m_filter_coefs
      use cal_minmax_and_stacks
!
      type(filter_coefficients_type), intent(inout) :: filter
!
      integer(kind = kint), allocatable :: id_org(:)
      integer(kind = kint) :: icou, inum, i, j
      integer(kind = kint) :: ist, ied, i_org, j_org, j_new
!
!
      allocate(id_org(filter%ntot_nod))
      id_org = 0
!
      call alloc_inod_filter_comb(filter)
!
!    ordering by number of filtering nodes
!
      icou = 0
      do i = 1, filter%ngrp_node
        icou = filter%istack_node(i-1)
        ist = filter%istack_node(i-1) + 1
        ied = filter%istack_node(i)
        do j = nmin_nod_near_all_w, nmax_nod_near_all_w
          do inum = ist, ied
            if ( nnod_near_nod_all_w(inum) .eq. j) then
              icou = icou+1
              filter%nnod_near(icou) = nnod_near_nod_all_w(inum)
              id_org(icou) = inum
            end if
          end do
        end do
      end do
!
!     count stack for filtering positions
!
      do inum = 1, filter%ntot_nod
        i_org = id_org(inum)
        filter%inod_filter(inum) = inod_all_w(i_org)
      end do
!
      call s_cal_total_and_stacks(filter%ntot_nod,                      &
     &    filter%nnod_near, izero, filter%istack_near_nod,              &
     &    filter%ntot_near_nod)
!
!      copy filter coefficients and weight
!
      call alloc_3d_filter_comb(filter)
      call alloc_3d_filter_func(filter)
!
      do inum = 1, filter%ntot_nod
        i_org = id_org(inum)
!
        do i = 1, filter%nnod_near(inum)
          j_new = filter%istack_near_nod(inum-1) + i
          j_org = inod_stack_nod_all_w(i_org-1) + i
          filter%inod_near(j_new) = inod_near_nod_all_w(j_org)
          filter%func(j_new) =   filter_func(j_org)
          filter%weight(j_new) = filter_weight(j_org)
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
      subroutine copy_3d_filter_stack_no_sort(filter)
!
      use m_filter_coefs
      use cal_minmax_and_stacks
!
      type(filter_coefficients_type), intent(inout) :: filter
!
!
      call alloc_inod_filter_comb(filter)
!
      filter%inod_filter(1:filter%ntot_nod)                             &
     &      = inod_all_w(1:filter%ntot_nod)
      filter%nnod_near(1:filter%ntot_nod)                               &
     &      = nnod_near_nod_all_w(1:filter%ntot_nod)
!
      call s_cal_total_and_stacks(filter%ntot_nod,                      &
     &    filter%nnod_near, izero, filter%istack_near_nod,              &
     &    filter%ntot_near_nod)
!
      end subroutine copy_3d_filter_stack_no_sort
!
! ----------------------------------------------------------------------
!
      subroutine copy_3d_filtering_no_sorting(filter)
!
      use m_filter_coefs
!
      type(filter_coefficients_type), intent(inout) :: filter
!
!
      call alloc_3d_filter_comb(filter)
      call alloc_3d_filter_func(filter)
!
      filter%inod_near(1:filter%ntot_near_nod)                          &
     &      = inod_near_nod_all_w(1:filter%ntot_near_nod)
      filter%weight(1:filter%ntot_near_nod)                             &
     &      = filter_weight(1:filter%ntot_near_nod)
      filter%func(1:filter%ntot_near_nod)                               &
     &      = filter_func(1:filter%ntot_near_nod)
!
      call deallocate_filter_coefs
      call deallocate_nod_ele_near_all_w
!
      end subroutine copy_3d_filtering_no_sorting
!
! ----------------------------------------------------------------------
!
      end module sorting_by_filtering_area
