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
        mat_name_2nd(i) = mat_name(i)
      end do
!
      mat_istack_2nd(0) = 0
      do i = 1, num_mat
        mat_istack_2nd(i) = mat_istack_2nd(i-1)
!
        ist = mat_istack(i-1) + 1
        ied = mat_istack(i)
        do inum = ist, ied
          iele = mat_item(inum)
          mat_istack_2nd(i) = mat_istack_2nd(i) + num_ele_refined(iele)
        end do
      end do
      num_mat_bc_2nd = mat_istack_2nd(num_mat)
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
        icou = mat_istack_2nd(i-1)
!
        ist = mat_istack(i-1) + 1
        ied = mat_istack(i)
        do inum = ist, ied
          iele = mat_item(inum)
          jst = istack_ele_refined(iele-1) + 1
          jed = istack_ele_refined(iele)
          do jnum = jst, jed
            icou = icou + 1
            mat_item_2nd(icou) = jnum
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
