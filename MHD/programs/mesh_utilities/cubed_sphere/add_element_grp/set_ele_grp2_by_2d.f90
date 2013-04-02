!
!      module set_ele_grp2_by_2d
!
      module  set_ele_grp2_by_2d
!
!     Written by H. Matsui on Mar., 2008
!
      use m_geometry_parameter
!
      use m_precision
!
      implicit none
!
      private :: set_2d_ele_grp_names, ordering_each_added_egrp
      private :: count_ele_grp_num_add_2d, add_ele_grp_item_by_2d
!
!      subroutine const_ele_grp_item_by_2d(ref_ele1, ref_ele2,          &
!     &          num_ele_grp1, ele_grp_name1, dminmax_grp_1,            &
!     &          num_ele_grp2, ele_grp_name2, dminmax_grp_2)
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine const_ele_grp_item_by_2d(ref_ele1, ref_ele2,           &
     &          num_ele_grp1, ele_grp_name1, dminmax_grp_1,             &
     &          num_ele_grp2, ele_grp_name2, dminmax_grp_2)
!
      use m_element_group
      use m_2nd_group_data
      use m_work_4_add_egrp_sph
      use start_end_4_2d_ele_grping
!
      integer(kind = kint), intent(in) :: num_ele_grp1
      character(len=kchara), intent(in)  :: ele_grp_name1(num_ele_grp1)
      real(kind = kreal), intent(in) :: dminmax_grp_1(num_ele_grp1,2)
      integer(kind = kint), intent(in) :: num_ele_grp2
      character(len=kchara), intent(in)  :: ele_grp_name2(num_ele_grp2)
      real(kind = kreal), intent(in) :: dminmax_grp_2(num_ele_grp2,2)
!
      real(kind = kreal), intent(in) :: ref_ele1(numele)
      real(kind = kreal), intent(in) :: ref_ele2(numele)
!
!
        num_mat_2nd = num_mat + num_ele_grp1*num_ele_grp2
!
        call allocate_2nd_ele_grp_num
        call allocate_work_4_add_egrp_sph
!
        call set_2d_ele_grp_names(num_ele_grp1, ele_grp_name1,          &
     &      num_ele_grp2, ele_grp_name2)
        call count_ele_grp_num_add_2d(ref_ele1, ref_ele2,               &
     &      num_ele_grp1,  dminmax_grp_1, num_ele_grp2, dminmax_grp_2)
!
        call allocate_2nd_ele_grp_item
!
        call add_ele_grp_item_by_2d(ref_ele1, ref_ele2,                 &
     &      num_ele_grp1, dminmax_grp_1, num_ele_grp2, dminmax_grp_2)
        call ordering_each_added_egrp(num_ele_grp1, num_ele_grp2)
!
        call deallocate_work_4_add_egrp_sph
!
      end subroutine const_ele_grp_item_by_2d
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_2d_ele_grp_names(num_ele_grp1, ele_grp_name1,      &
     &          num_ele_grp2, ele_grp_name2)
!
      use m_element_group
      use m_2nd_group_data
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: num_ele_grp1
      character(len=kchara), intent(in)  :: ele_grp_name1(num_ele_grp1)
      integer(kind = kint), intent(in) :: num_ele_grp2
      character(len=kchara), intent(in)  :: ele_grp_name2(num_ele_grp2)
!
      integer(kind = kint) :: i, j, igrp
!
!
      mat_name_2nd(1:num_mat) =   mat_name(1:num_mat)
      mat_istack_2nd(0:num_mat) = mat_istack(0:num_mat)
!
      do i = 1, num_ele_grp1
        do j = 1, num_ele_grp2
          igrp = num_mat + j + (i-1)*num_ele_grp2
          write(mat_name_2nd(igrp),'(a,a1,a)')                          &
     &        trim(ele_grp_name1(i)), '_', trim(ele_grp_name2(j))
        end do
      end do
!
      end subroutine set_2d_ele_grp_names
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_ele_grp_num_add_2d(ref_ele1, ref_ele2,           &
     &       num_ele_grp1, dminmax_grp_1, num_ele_grp2, dminmax_grp_2)
!
      use m_add_ele_grp_parameter
      use m_element_group
      use m_2nd_group_data
!
      use m_work_4_add_egrp_sph
      use start_end_4_2d_ele_grping
!
      integer(kind = kint), intent(in) :: num_ele_grp1
      real(kind = kreal), intent(in) :: dminmax_grp_1(num_ele_grp1,2)
      integer(kind = kint), intent(in) :: num_ele_grp2
      real(kind = kreal), intent(in) :: dminmax_grp_2(num_ele_grp2,2)
!
      real(kind = kreal), intent(in) :: ref_ele1(numele)
      real(kind = kreal), intent(in) :: ref_ele2(numele)
!
      integer(kind = kint) :: i, j
      integer(kind = kint) :: igrp
      integer(kind = kint) :: item_st, item_ed
      integer(kind = kint) :: jtem_st, jtem_ed
      real(kind = kreal) :: max_prev_1, max_prev_2
!
!
      max_prev_1 = 1.0e30
      item_ed = 1
      do i = 1, num_ele_grp1
        call set_start_end_egrping(ref_ele1,                            &
     &      dminmax_grp_1(i,1), dminmax_grp_1(i,2), max_prev_1,         &
     &      item_st, item_ed)
!
        max_prev_2 = 1.0e30
        jtem_ed = item_st
        do j = 1, num_ele_grp2
!
          call set_2nd_start_end_egrp(ref_ele2, dminmax_grp_2(j,1),     &
     &        dminmax_grp_2(j,2), max_prev_2, item_st, item_ed,         &
     &        jtem_st, jtem_ed)
!
          igrp = num_mat + j + (i-1)*num_ele_grp2
          mat_istack_2nd(igrp) = mat_istack_2nd(igrp-1)                 &
     &                          + (jtem_ed-jtem_st+1)
!
        end do
      end do
      num_mat_bc_2nd = mat_istack_2nd(num_mat_2nd)
!
      end subroutine count_ele_grp_num_add_2d
!
!   --------------------------------------------------------------------
!
      subroutine add_ele_grp_item_by_2d(ref_ele1, ref_ele2,             &
     &       num_ele_grp1, dminmax_grp_1, num_ele_grp2, dminmax_grp_2)
!
      use m_work_4_add_egrp_sph
      use start_end_4_2d_ele_grping
      use m_element_group
      use m_2nd_group_data
!
      integer(kind = kint), intent(in) :: num_ele_grp1
      real(kind = kreal), intent(in) :: dminmax_grp_1(num_ele_grp1,2)
      integer(kind = kint), intent(in) :: num_ele_grp2
      real(kind = kreal), intent(in) :: dminmax_grp_2(num_ele_grp2,2)
!
      real(kind = kreal), intent(in) :: ref_ele1(numele)
      real(kind = kreal), intent(in) :: ref_ele2(numele)
!
      integer(kind = kint) :: i, j
      integer(kind = kint) :: item_st, item_ed
      integer(kind = kint) :: jtem_st, jtem_ed
      integer(kind = kint) :: igrp, ngrp
      integer(kind = kint) :: inum, jnum, knum
      real(kind = kreal) :: max_prev_1, max_prev_2
!
!
      max_prev_1 = 1.0e30
      item_ed = 1
      do i = 1, num_ele_grp1
        call set_start_end_egrping(ref_ele1,                            &
     &      dminmax_grp_1(i,1), dminmax_grp_1(i,2), max_prev_1,         &
     &      item_st, item_ed)
!
        max_prev_2 = 1.0e30
        jtem_ed = item_st
        do j = 1, num_ele_grp2
          call set_2nd_start_end_egrp(ref_ele2, dminmax_grp_2(j,1),     &
     &        dminmax_grp_2(j,2), max_prev_2, item_st, item_ed,         &
     &        jtem_st, jtem_ed)
!
          igrp = num_mat + j + (i-1)*num_ele_grp2
          ngrp = mat_istack_2nd(igrp) - mat_istack_2nd(igrp-1)
          do jnum = 1, ngrp
            inum = jtem_st + jnum - 1
            knum = mat_istack_2nd(igrp-1) + jnum
            mat_item_2nd(knum) = iele_4_sort(inum)
          end do
!
        end do
      end do
!
      end subroutine add_ele_grp_item_by_2d
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine ordering_each_added_egrp(num_ele_grp1, num_ele_grp2)
!
      use quicksort
      use m_element_group
      use m_2nd_group_data
!
      integer(kind = kint), intent(in) :: num_ele_grp1
      integer(kind = kint), intent(in) :: num_ele_grp2
      integer(kind = kint) :: i, j
      integer(kind = kint) :: igrp, ist, ied
!
!
      mat_item_2nd(1:num_mat_bc) = mat_item(1:num_mat_bc)
!
      do i = 1, num_ele_grp1
        do j = 1, num_ele_grp2
          igrp = num_mat + j + (i-1)*num_ele_grp2
          ist = mat_istack_2nd(igrp-1) + 1
          ied = mat_istack_2nd(igrp)
          call quicksort_int(num_mat_bc_2nd, mat_item_2nd, ist, ied)
        end do
      end do
!
      end subroutine ordering_each_added_egrp
!
!   --------------------------------------------------------------------
!
      end module set_ele_grp2_by_2d
