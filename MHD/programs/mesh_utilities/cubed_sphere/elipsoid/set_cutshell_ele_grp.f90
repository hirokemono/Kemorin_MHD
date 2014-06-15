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
      ele_grp_2nd%num_grp =  num_mat
      call allocate_sf_grp_type_num(ele_grp_2nd)
!
      call count_new_ele_group
      call allocate_grp_type_item(ele_grp_2nd)
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
      ele_grp_2nd%grp_name(1:num_mat) = mat_name(1:num_mat)
!
      ele_grp_2nd%istack_grp(0) = 0
      do i = 1, num_mat
         ele_grp_2nd%istack_grp(i) = ele_grp_2nd%istack_grp(i-1)
         do inum = mat_istack(i-1)+1, mat_istack(i)
           iele = mat_item(inum)
           if ( mark_new_ele(iele) .ne. 0 ) then
             ele_grp_2nd%istack_grp(i) = ele_grp_2nd%istack_grp(i) + 1
           end if
         end do
      end do
      ele_grp_2nd%num_item = ele_grp_2nd%istack_grp(ele_grp_2nd%num_grp)
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
             ele_grp_2nd%item_grp(icou) = mark_new_ele(iele)
           end if
         end do
      end do
!
      end subroutine set_new_ele_group
!
!  ---------------------------------------------------------------------
!
      end module set_cutshell_ele_grp
