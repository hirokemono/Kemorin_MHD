!
!      module delete_small_weighting
!
      module delete_small_weighting
!
!     Written by H. Matsui on Nov., 2006
!
      use m_precision
!
      use calypso_mpi
!
      implicit none
!
!
      integer(kind = kint), allocatable :: id_org(:), id_tmp(:)
      real(kind= kreal), allocatable :: wgt_tmp(:), coef_tmp(:)
!
      private :: id_org, id_tmp
      private :: wgt_tmp, coef_tmp
      private :: sorting_by_filter_weights, truncate_tiny_weighting
!
!      subroutine allocate_tmp_4_filter_sort
!      subroutine deallocate_tmp_4_filter_sort
!      subroutine s_delete_small_weighting
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_delete_small_weighting
!
      call sorting_by_filter_weights
!
      call truncate_tiny_weighting
!
      end subroutine s_delete_small_weighting
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_tmp_4_filter_sort
!
      use m_geometry_parameter
      use m_filter_coefs
!
      allocate(id_org(numnod))
      allocate(wgt_tmp(numnod))
!
      allocate(id_tmp(numnod))
      allocate(coef_tmp(numnod))
!
      id_org = 0
      id_tmp = 0
      wgt_tmp = 0.0d0
      coef_tmp = 0.0d0
!
      end subroutine allocate_tmp_4_filter_sort
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_tmp_4_filter_sort
!
      deallocate(id_org)
      deallocate(wgt_tmp)
      deallocate(id_tmp)
      deallocate(coef_tmp)
!
      end subroutine deallocate_tmp_4_filter_sort
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sorting_by_filter_weights
!
      use m_geometry_parameter
      use m_filter_coefs
      use quicksort
!
      integer(kind = kint) :: id_start
      integer(kind = kint) :: i, j, j_new, j_org
!
!
!
      do i = 1, nnod_near_1nod_weight
        wgt_tmp(i) = - abs( weight_1nod(i) )
        id_org(i) = i
      end do
!
      do i = nnod_near_1nod_weight, 1, -1
        if (filter_1nod(i) .ne. 0.0d0) then
          id_start = i+1
          exit
        end if
      end do
!
      if (id_start .ge. nnod_near_1nod_weight) return
!
      call quicksort_real_w_index(numnod, wgt_tmp,                      &
     &      id_start, nnod_near_1nod_weight, id_org)
!
      id_tmp(1:numnod) =   0
      coef_tmp(1:numnod) = 0
      wgt_tmp(1:numnod) =  0
!
      do i = 1, nnod_near_1nod_weight
        id_tmp(i) =   inod_near_1nod_weight(i)
        coef_tmp(i) = filter_1nod(i)
        wgt_tmp(i) =  weight_1nod(i)
      end do
!
      do i = id_start, nnod_near_1nod_weight
        j_org = id_org(i)
        inod_near_1nod_weight(i) = id_tmp(j_org)
        filter_1nod(i) =           coef_tmp(j_org)
        weight_1nod(i) =           wgt_tmp(j_org)
      end do
!
      end subroutine sorting_by_filter_weights
!
! ----------------------------------------------------------------------
!
      subroutine truncate_tiny_weighting
!
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
!
      integer(kind = kint) :: i, new_number
      real(kind = kreal) :: ratio, zero_moment
!
!
      do i = 1, nnod_near_1nod_weight
        if (filter_1nod(i) .eq. 0.0d0) then
          ratio = abs( weight_1nod(i) / weight_1nod(1) )
          if (ratio .lt. omitted_ratio) exit
        end if
        new_number = i
      end do
      nnod_near_1nod_weight = new_number
!
!
      zero_moment = 0.0d0
      do i = 1, nnod_near_1nod_weight
        zero_moment = zero_moment + weight_1nod(i)
      end do
!
      zero_moment = 1.0d0 / zero_moment
      do i = 1, nnod_near_1nod_weight
        weight_1nod(i) = weight_1nod(i) * zero_moment
      end do
!
      end subroutine truncate_tiny_weighting
!
! ----------------------------------------------------------------------
!
      end module delete_small_weighting
