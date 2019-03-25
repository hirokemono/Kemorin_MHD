!
!      module sorting_by_filtering_area
!
!     Written by H. Matsui on Nov., 2006
!
!!      subroutine s_sorting_by_filtering_area(filter)
!!     &         (nmin_nod_near_all, nmax_nod_near_all, fil_org, filter)
!!        type(filter_coefficients_type), intent(in) :: fil_org
!!        type(filter_coefficients_type), intent(inout) :: filter
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
      subroutine s_sorting_by_filtering_area                            &
     &         (nmin_nod_near_all, nmax_nod_near_all, fil_org, filter)
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: nmin_nod_near_all
      integer(kind = kint), intent(in) :: nmax_nod_near_all
      type(filter_coefficients_type), intent(in) :: fil_org
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
        do j = nmin_nod_near_all, nmax_nod_near_all
          do inum = ist, ied
            if(fil_org%nnod_near(inum) .eq. j) then
              icou = icou+1
              filter%nnod_near(icou) = fil_org%nnod_near(inum)
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
        filter%inod_filter(inum) = fil_org%inod_filter(i_org)
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
          j_org = fil_org%istack_near_nod(i_org-1) + i
          filter%inod_near(j_new) = fil_org%inod_near(j_org)
          filter%func(j_new) =   fil_org%func(j_org)
          filter%weight(j_new) = fil_org%weight(j_org)
        end do
!
      end do
!
      deallocate(id_org)
!
      end subroutine s_sorting_by_filtering_area
!
! ----------------------------------------------------------------------
!
      end module sorting_by_filtering_area
