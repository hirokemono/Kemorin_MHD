!
!      module set_cutshell_ele_grp
!
      module set_cutshell_ele_grp
!
!     Written by H. Matsui
!
      use m_precision
!
      use m_element_group
      use m_2nd_group_data
      use m_cutshell_nod_ele_flag
!
      implicit none
!
      private :: count_new_ele_group, set_new_ele_group
!
!      subroutine s_set_new_element_grp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_new_element_grp
!
       write(*,*) 'choose element_group'
!
      num_mat_2nd =  num_mat
      call allocate_2nd_ele_grp_num
!
      call count_new_ele_group
      call allocate_2nd_ele_grp_item
!
      call set_new_ele_group
!
      end subroutine s_set_new_element_grp
!
!  ---------------------------------------------------------------------
!
      subroutine count_new_ele_group
!
      integer(kind = kint) :: i, iele, inum
!
!
      mat_name_2nd(1:num_mat) = mat_name(1:num_mat)
!
      mat_istack_2nd(0) = 0
      do i = 1, num_mat
         mat_istack_2nd(i) = mat_istack_2nd(i-1)
         do inum = mat_istack(i-1)+1, mat_istack(i)
           iele = mat_item(inum)
           if ( mark_new_ele(iele) .ne. 0 ) then
             mat_istack_2nd(i) = mat_istack_2nd(i) + 1
           end if
         end do
      end do
      num_mat_bc_2nd = mat_istack_2nd(num_mat_2nd)
!
      end subroutine count_new_ele_group
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_ele_group
!
      integer(kind = kint) :: iele, inum, i, icou
!
      icou = 0
      do i = 1, num_mat
         do inum = mat_istack(i-1)+1, mat_istack(i)
           iele = mat_item(inum)
           if ( mark_new_ele(iele) .ne. 0 ) then
             icou = icou + 1
             mat_item_2nd(icou) = mark_new_ele(iele)
           end if
         end do
      end do
!
      end subroutine set_new_ele_group
!
!  ---------------------------------------------------------------------
!
      end module set_cutshell_ele_grp
