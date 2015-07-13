!
!      module set_ele_grp2_by_2d
!
!     Written by H. Matsui on Mar., 2008
!
!      subroutine const_ele_grp_item_by_2d(ref_ele1, ref_ele2,          &
!     &          num_ele_grp1, ele_grp_name1, dminmax_grp_1,            &
!     &          num_ele_grp2, ele_grp_name2, dminmax_grp_2)
!
      module  set_ele_grp2_by_2d
!
      use m_precision
      use m_geometry_parameter
!
!
      implicit none
!
      integer(kind = kint) :: ngrp_added
      integer(kind = kint), allocatable :: nitem_added_lc(:)
      integer(kind = kint), allocatable :: nitem_added_gl(:)
      integer(kind = kint), allocatable :: iflag_added_egrp(:)
!
      private :: set_2d_ele_grp_names, ordering_each_added_egrp
      private :: add_ele_grp_item_by_2d
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_nitem_to_be_added
!
!
      allocate(nitem_added_lc(ngrp_added))
      allocate(nitem_added_gl(ngrp_added))
      allocate(iflag_added_egrp(ngrp_added))
      nitem_added_lc = 0
      nitem_added_gl = 0
      iflag_added_egrp = 0
!
      end subroutine alloc_nitem_to_be_added
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_nitem_to_be_added
!
      deallocate(iflag_added_egrp, nitem_added_lc, nitem_added_gl)
!
      end subroutine dealloc_nitem_to_be_added
!
!   --------------------------------------------------------------------
!
      subroutine const_ele_grp_item_by_2d(ref_ele1, ref_ele2,           &
     &          num_ele_grp1, ele_grp_name1, dminmax_grp_1,             &
     &          num_ele_grp2, ele_grp_name2, dminmax_grp_2)
!
      use m_element_group
      use m_work_4_add_egrp_sph
      use t_group_data
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
      type(group_data) :: new_elegrp
!
!
      call count_ngrp_ele_grps(new_elegrp%num_grp)
      call allocate_grp_type_num(new_elegrp)
!
      call set_2d_ele_grp_names(num_ele_grp1, ele_grp_name1,            &
     &    num_ele_grp2, ele_grp_name2, new_elegrp)
!
      call allocate_grp_type_item(new_elegrp)
!
      call add_ele_grp_item_by_2d(ref_ele1, ref_ele2,                   &
     &    num_ele_grp1, dminmax_grp_1, num_ele_grp2, dminmax_grp_2,     &
     &    new_elegrp)
!
      call ordering_each_added_egrp(new_elegrp)
      call deallocate_grp_type(new_elegrp)
!
      end subroutine const_ele_grp_item_by_2d
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_ngrp_ele_grps(ngrp_new_ele)
!
      use m_element_group
!
      integer(kind = kint), intent(inout)  :: ngrp_new_ele
      integer(kind = kint) :: igrp
!
!
      ngrp_new_ele = ele_grp1%num_grp
      do igrp = 1, ngrp_added
        if(nitem_added_gl(igrp) .gt. 0) ngrp_new_ele = ngrp_new_ele + 1
      end do
!
      end subroutine count_ngrp_ele_grps
!
!   --------------------------------------------------------------------
!
      subroutine set_2d_ele_grp_names(num_ele_grp1, ele_grp_name1,      &
     &          num_ele_grp2, ele_grp_name2, new_elegrp)
!
      use m_element_group
      use set_parallel_file_name
      use t_group_data
!
      integer(kind = kint), intent(in) :: num_ele_grp1
      character(len=kchara), intent(in)  :: ele_grp_name1(num_ele_grp1)
      integer(kind = kint), intent(in) :: num_ele_grp2
      character(len=kchara), intent(in)  :: ele_grp_name2(num_ele_grp2)
!
      type(group_data), intent(inout) :: new_elegrp
!
      integer(kind = kint) :: i, j, igrp, icou
!
!
      new_elegrp%grp_name(1:ele_grp1%num_grp)                           &
     &      = ele_grp1%grp_name(1:ele_grp1%num_grp)
      new_elegrp%istack_grp(0:ele_grp1%num_grp)                         &
     &      = ele_grp1%istack_grp(0:ele_grp1%num_grp)
!
      icou = ele_grp1%num_grp
      do i = 1, num_ele_grp1
        do j = 1, num_ele_grp2
          igrp = j + (i-1)*num_ele_grp2
!
          if(nitem_added_gl(igrp) .gt. 0) then
            icou = icou + 1
            write(new_elegrp%grp_name(icou),'(a,a1,a)')                 &
     &        trim(ele_grp_name1(i)), '_', trim(ele_grp_name2(j))
            new_elegrp%istack_grp(icou) = new_elegrp%istack_grp(icou-1) &
     &                                   + nitem_added_lc(igrp)
          end if
        end do
      end do
      new_elegrp%num_item = new_elegrp%istack_grp(new_elegrp%num_grp)
!
      end subroutine set_2d_ele_grp_names
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_added_egrp_item(ref_ele1, ref_ele2,              &
     &       num_ele_grp1, dminmax_grp_1, num_ele_grp2, dminmax_grp_2)
!
      use m_add_ele_grp_parameter
      use m_element_group
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
      ngrp_added = num_ele_grp1 * num_ele_grp2
      call alloc_nitem_to_be_added
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
          igrp = j + (i-1)*num_ele_grp2
          nitem_added_lc(igrp) = jtem_ed - jtem_st + 1
        end do
      end do
!
      end subroutine count_added_egrp_item
!
!   --------------------------------------------------------------------
!
      subroutine add_ele_grp_item_by_2d(ref_ele1, ref_ele2,             &
     &       num_ele_grp1, dminmax_grp_1, num_ele_grp2, dminmax_grp_2,  &
     &       new_elegrp)
!
      use m_work_4_add_egrp_sph
      use start_end_4_2d_ele_grping
      use m_element_group
      use t_group_data
!
      integer(kind = kint), intent(in) :: num_ele_grp1
      real(kind = kreal), intent(in) :: dminmax_grp_1(num_ele_grp1,2)
      integer(kind = kint), intent(in) :: num_ele_grp2
      real(kind = kreal), intent(in) :: dminmax_grp_2(num_ele_grp2,2)
!
      real(kind = kreal), intent(in) :: ref_ele1(numele)
      real(kind = kreal), intent(in) :: ref_ele2(numele)
!
      type(group_data), intent(inout) :: new_elegrp
!
      integer(kind = kint) :: i, j
      integer(kind = kint) :: item_st, item_ed
      integer(kind = kint) :: jtem_st, jtem_ed
      integer(kind = kint) :: igrp, icou
      integer(kind = kint) :: inum, jnum, knum
      real(kind = kreal) :: max_prev_1, max_prev_2
!
!
      new_elegrp%item_grp(1:ele_grp1%num_item)                          &
     &         = ele_grp1%item_grp(1:ele_grp1%num_item)
!
      max_prev_1 = 1.0e30
      item_ed = 1
      icou = ele_grp1%num_grp
      do i = 1, num_ele_grp1
        call set_start_end_egrping(ref_ele1,                            &
     &      dminmax_grp_1(i,1), dminmax_grp_1(i,2), max_prev_1,         &
     &      item_st, item_ed)
!
        max_prev_2 = 1.0e30
        jtem_ed = item_st
        do j = 1, num_ele_grp2
          igrp = j + (i-1)*num_ele_grp2
          if(nitem_added_gl(igrp) .gt. 0) then
            icou = icou + 1
!
            call set_2nd_start_end_egrp(ref_ele2, dminmax_grp_2(j,1),   &
     &          dminmax_grp_2(j,2), max_prev_2, item_st, item_ed,       &
     &          jtem_st, jtem_ed)
!
            do jnum = 1, nitem_added_lc(igrp)
              inum = jtem_st + jnum - 1
              knum = new_elegrp%istack_grp(icou-1) + jnum
              new_elegrp%item_grp(knum) = iele_4_sort(inum)
            end do
          end if
!
        end do
      end do
!
      end subroutine add_ele_grp_item_by_2d
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine ordering_each_added_egrp(new_elegrp)
!
      use quicksort
      use m_element_group
      use t_group_data
!
      type(group_data), intent(inout) :: new_elegrp
!
      integer(kind = kint) :: igrp, ist, ied
!
!
      do igrp = ele_grp1%num_grp+1, new_elegrp%num_grp
        ist = new_elegrp%istack_grp(igrp-1) + 1
        ied = new_elegrp%istack_grp(igrp)
        call quicksort_int(new_elegrp%num_item, new_elegrp%item_grp,    &
     &      ist, ied)
      end do
!
      call deallocate_material_data
!
      ele_grp1%num_grp =    new_elegrp%num_grp
      ele_grp1%num_item = new_elegrp%num_item
      call allocate_material_data
!
      ele_grp1%grp_name(1:ele_grp1%num_grp)                             &
     &        =    new_elegrp%grp_name(1:ele_grp1%num_grp)
      ele_grp1%istack_grp(0:ele_grp1%num_grp)                           &
     &        =  new_elegrp%istack_grp(0:ele_grp1%num_grp)
      ele_grp1%item_grp(1:ele_grp1%num_item)                            &
     &        = new_elegrp%item_grp(1:ele_grp1%num_item)
!
      end subroutine ordering_each_added_egrp
!
!   --------------------------------------------------------------------
!
      end module set_ele_grp2_by_2d
