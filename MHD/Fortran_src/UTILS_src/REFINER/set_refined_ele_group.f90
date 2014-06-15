!set_refined_ele_group.f90
!      module set_refined_ele_group
!
      module set_refined_ele_group
!
!      Writen by H. Matsui on Oct., 2007
!
      use m_precision
!
      implicit none
!
!      subroutine count_refined_ele_group
!      subroutine s_set_refined_ele_group
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_refined_ele_group
!
      use m_element_group
      use m_2nd_group_data
      use m_refined_element_data
!
      integer(kind= kint) :: i, iele, ist, ied, inum
!
!
      do i = 1, num_mat
        ele_grp_2nd%grp_name(i) = mat_name(i)
      end do
!
      ele_grp_2nd%istack_grp(0) = 0
      do i = 1, num_mat
        ele_grp_2nd%istack_grp(i) = ele_grp_2nd%istack_grp(i-1)
!
        ist = mat_istack(i-1) + 1
        ied = mat_istack(i)
        do inum = ist, ied
          iele = mat_item(inum)
          ele_grp_2nd%istack_grp(i) = ele_grp_2nd%istack_grp(i) + num_ele_refined(iele)
        end do
      end do
      ele_grp_2nd%num_item = ele_grp_2nd%istack_grp(num_mat)
!
      end subroutine count_refined_ele_group
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_refined_ele_group
!
      use m_element_group
      use m_2nd_group_data
      use m_refined_element_data
!
      integer(kind= kint) :: i, iele, ist, ied, inum, icou
      integer(kind= kint) :: jst, jed, jnum
!
!
      do i = 1, num_mat
        icou = ele_grp_2nd%istack_grp(i-1)
!
        ist = mat_istack(i-1) + 1
        ied = mat_istack(i)
        do inum = ist, ied
          iele = mat_item(inum)
          jst = istack_ele_refined(iele-1) + 1
          jed = istack_ele_refined(iele)
          do jnum = jst, jed
            icou = icou + 1
            ele_grp_2nd%item_grp(icou) = jnum
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
