!
!      module set_cutshell_ele_grp
!
!     Written by H. Matsui
!
!      subroutine s_set_new_element_grp(new_ele_grp)
!
      module set_cutshell_ele_grp
!
      use m_precision
!
      use m_element_group
      use m_cutshell_nod_ele_flag
      use t_group_data
!
      implicit none
!
      private :: count_new_ele_group, set_new_ele_group
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_new_element_grp(new_ele_grp)
!
      type(group_data), intent(inout) :: new_ele_grp
!
!
      new_ele_grp%num_grp =  ele_grp1%num_grp
      call allocate_grp_type_num(new_ele_grp)
!
      call count_new_ele_group(new_ele_grp)
      call allocate_grp_type_item(new_ele_grp)
!
      call set_new_ele_group(new_ele_grp)
!
      end subroutine s_set_new_element_grp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_new_ele_group(new_ele_grp)
!
      type(group_data), intent(inout) :: new_ele_grp
!
      integer(kind = kint) :: i, iele, inum
!
!
      new_ele_grp%grp_name(1:ele_grp1%num_grp)                          &
     &       = ele_grp1%grp_name(1:ele_grp1%num_grp)
!
      new_ele_grp%istack_grp(0) = 0
      do i = 1, ele_grp1%num_grp
         new_ele_grp%istack_grp(i) = new_ele_grp%istack_grp(i-1)
         do inum = mat_istack(i-1)+1, mat_istack(i)
           iele = mat_item(inum)
           if ( mark_new_ele(iele) .ne. 0 ) then
             new_ele_grp%istack_grp(i) = new_ele_grp%istack_grp(i) + 1
           end if
         end do
      end do
      new_ele_grp%num_item                                              &
     &      = new_ele_grp%istack_grp(new_ele_grp%num_grp)
!
      end subroutine count_new_ele_group
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_ele_group(new_ele_grp)
!
      type(group_data), intent(inout) :: new_ele_grp
!
      integer(kind = kint) :: iele, inum, i, icou
!
      icou = 0
      do i = 1, ele_grp1%num_grp
         do inum = mat_istack(i-1)+1, mat_istack(i)
           iele = mat_item(inum)
           if ( mark_new_ele(iele) .ne. 0 ) then
             icou = icou + 1
             new_ele_grp%item_grp(icou) = mark_new_ele(iele)
           end if
         end do
      end do
!
      end subroutine set_new_ele_group
!
!  ---------------------------------------------------------------------
!
      end module set_cutshell_ele_grp
