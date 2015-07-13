!set_refined_ele_group.f90
!      module set_refined_ele_group
!
!      Writen by H. Matsui on Oct., 2007
!
!      subroutine count_refined_ele_group(new_ele_grp)
!      subroutine s_set_refined_ele_group(new_ele_grp)
!
      module set_refined_ele_group
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_refined_ele_group(new_ele_grp)
!
      use m_element_group
      use t_group_data
      use m_refined_element_data
!
      type(group_data), intent(inout) :: new_ele_grp
!
      integer(kind= kint) :: i, iele, ist, ied, inum
!
!
      do i = 1, ele_grp1%num_grp
        new_ele_grp%grp_name(i) = ele_grp1%grp_name(i)
      end do
!
      new_ele_grp%istack_grp(0) = 0
      do i = 1, ele_grp1%num_grp
        new_ele_grp%istack_grp(i) = new_ele_grp%istack_grp(i-1)
!
        ist = mat_istack(i-1) + 1
        ied = mat_istack(i)
        do inum = ist, ied
          iele = mat_item(inum)
          new_ele_grp%istack_grp(i) = new_ele_grp%istack_grp(i)         &
     &                               + num_ele_refined(iele)
        end do
      end do
      new_ele_grp%num_item = new_ele_grp%istack_grp(ele_grp1%num_grp)
!
      end subroutine count_refined_ele_group
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_refined_ele_group(new_ele_grp)
!
      use m_element_group
      use t_group_data
      use m_refined_element_data
!
      type(group_data), intent(inout) :: new_ele_grp
!
      integer(kind= kint) :: i, iele, ist, ied, inum, icou
      integer(kind= kint) :: jst, jed, jnum
!
!
      do i = 1, ele_grp1%num_grp
        icou = new_ele_grp%istack_grp(i-1)
!
        ist = mat_istack(i-1) + 1
        ied = mat_istack(i)
        do inum = ist, ied
          iele = mat_item(inum)
          jst = istack_ele_refined(iele-1) + 1
          jed = istack_ele_refined(iele)
          do jnum = jst, jed
            icou = icou + 1
            new_ele_grp%item_grp(icou) = jnum
          end do
        end do
!
      end do
!
      end subroutine s_set_refined_ele_group
!
!  ---------------------------------------------------------------------
!
      end module set_refined_ele_group
