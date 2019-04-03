!
!      module delete_small_weighting
!
!     Written by H. Matsui on Nov., 2006
!
!!      subroutine s_delete_small_weighting(omitted_ratio, fil_coef)
!!        type(each_filter_coef), intent(inout) :: fil_coef
!!      subroutine allocate_tmp_4_filter_sort(nnod)
!!      subroutine deallocate_tmp_4_filter_sort
!
      module delete_small_weighting
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
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_delete_small_weighting(omitted_ratio, fil_coef)
!
      use t_filter_coefs
!
      real(kind = kreal), intent(in) :: omitted_ratio
      type(each_filter_coef), intent(inout) :: fil_coef
!
      call sorting_by_filter_weights(fil_coef)
      call truncate_tiny_weighting(omitted_ratio, fil_coef)
!
      end subroutine s_delete_small_weighting
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_tmp_4_filter_sort(nnod)
!
      integer(kind = kint), intent(in) :: nnod
!
      allocate(id_org(nnod))
      allocate(wgt_tmp(nnod))
!
      allocate(id_tmp(nnod))
      allocate(coef_tmp(nnod))
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
      subroutine sorting_by_filter_weights(fil_coef)
!
      use t_filter_coefs
      use quicksort
!
      type(each_filter_coef), intent(inout) :: fil_coef
!
      integer(kind = kint) :: id_start
      integer(kind = kint) :: i, j_org
!
!
!
      do i = 1, fil_coef%nnod_4_1nod_w
        wgt_tmp(i) = - abs( fil_coef%weight_1nod(i) )
        id_org(i) = i
      end do
!
      id_start = 0
      do i = fil_coef%nnod_4_1nod_w, 1, -1
        if (fil_coef%filter_1nod(i) .ne. 0.0d0) then
          id_start = i+1
          exit
        end if
      end do
!
      if (id_start .ge. fil_coef%nnod_4_1nod_w) return
!
      call quicksort_real_w_index(fil_coef%nnod, wgt_tmp,               &
     &      id_start, fil_coef%nnod_4_1nod_w, id_org)
!
      id_tmp(1:fil_coef%nnod) =   0
      coef_tmp(1:fil_coef%nnod) = 0
      wgt_tmp(1:fil_coef%nnod) =  0
!
      do i = 1, fil_coef%nnod_4_1nod_w
        id_tmp(i) =   fil_coef%inod_4_1nod_w(i)
        coef_tmp(i) = fil_coef%filter_1nod(i)
        wgt_tmp(i) =  fil_coef%weight_1nod(i)
      end do
!
      do i = id_start, fil_coef%nnod_4_1nod_w
        j_org = id_org(i)
        fil_coef%inod_4_1nod_w(i) = id_tmp(j_org)
        fil_coef%filter_1nod(i) = coef_tmp(j_org)
        fil_coef%weight_1nod(i) = wgt_tmp(j_org)
      end do
!
      end subroutine sorting_by_filter_weights
!
! ----------------------------------------------------------------------
!
      subroutine truncate_tiny_weighting(omitted_ratio, fil_coef)
!
      use t_filter_coefs
!
      real(kind = kreal), intent(in) :: omitted_ratio
      type(each_filter_coef), intent(inout) :: fil_coef
!
      integer(kind = kint) :: i, new_number
      real(kind = kreal) :: ratio, zero_moment
!
!
      new_number = 0
      do i = 1, fil_coef%nnod_4_1nod_w
        if (fil_coef%filter_1nod(i) .eq. 0.0d0) then
          ratio                                                         &
     &       = abs(fil_coef%weight_1nod(i) / fil_coef%weight_1nod(1))
          if(ratio .lt. omitted_ratio) exit
        end if
        new_number = i
      end do
      fil_coef%nnod_4_1nod_w = new_number
!
!
      zero_moment = 0.0d0
      do i = 1, fil_coef%nnod_4_1nod_w
        zero_moment = zero_moment + fil_coef%weight_1nod(i)
      end do
!
      zero_moment = 1.0d0 / zero_moment
      do i = 1, fil_coef%nnod_4_1nod_w
        fil_coef%weight_1nod(i) = fil_coef%weight_1nod(i) * zero_moment
      end do
!
      end subroutine truncate_tiny_weighting
!
! ----------------------------------------------------------------------
!
      end module delete_small_weighting
