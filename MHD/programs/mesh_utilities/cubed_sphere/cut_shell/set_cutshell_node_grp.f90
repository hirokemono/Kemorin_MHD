!
!      module set_cutshell_node_grp
!
!     Written by H. Matsui
!
!      subroutine s_set_new_node_grp_4_hemi(new_node, new_nod_grp)
!      subroutine s_set_new_node_grp(new_nod_grp)
!
      module set_cutshell_node_grp
!
      use m_precision
!
      use m_node_group
      use m_cutshell_nod_ele_flag
      use t_geometry_data
      use t_group_data
!
      implicit none
!
      private :: count_new_nod_group, set_new_nod_group
      private :: count_equator_nod_group, set_equator_nod_group
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_new_node_grp_4_hemi(new_node, new_nod_grp)
!
      type(node_data), intent(in) :: new_node
      type(group_data), intent(inout) :: new_nod_grp
!
!
      new_nod_grp%num_grp =  nod_grp1%num_grp + 1
      call allocate_grp_type_num(new_nod_grp)
!
      call count_new_nod_group(new_nod_grp)
      call count_equator_nod_group(new_node, new_nod_grp)
!
!
      call allocate_grp_type_item(new_nod_grp)
!
      call set_new_nod_group(new_nod_grp)
      call set_equator_nod_group(new_node, new_nod_grp)
!
      end subroutine s_set_new_node_grp_4_hemi
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_new_node_grp(new_nod_grp)
!
      type(group_data), intent(inout) :: new_nod_grp
!
!
      new_nod_grp%num_grp =  nod_grp1%num_grp
      call allocate_grp_type_num(new_nod_grp)
!
      call count_new_nod_group(new_nod_grp)
!
      call allocate_grp_type_item(new_nod_grp)
      call set_new_nod_group(new_nod_grp)
!
      end subroutine s_set_new_node_grp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_new_nod_group(new_nod_grp)
!
      type(group_data), intent(inout) :: new_nod_grp
!
      integer(kind = kint) :: i, inod, inum
!
      new_nod_grp%grp_name(1:nod_grp1%num_grp)                          &
     &     = nod_grp1%grp_name(1:nod_grp1%num_grp)
!
      new_nod_grp%istack_grp(0) = 0
      do i = 1, nod_grp1%num_grp
         new_nod_grp%istack_grp(i) = new_nod_grp%istack_grp(i-1)
         do inum = nod_grp1%istack_grp(i-1)+1, nod_grp1%istack_grp(i)
           inod = nod_grp1%item_grp(inum)
           if ( mark_new_node(inod) .ne. 0 ) then
             new_nod_grp%istack_grp(i) = new_nod_grp%istack_grp(i) + 1
           end if
         end do
      end do
      new_nod_grp%num_item = new_nod_grp%istack_grp(nod_grp1%num_grp)
!
      end subroutine count_new_nod_group
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_nod_group(new_nod_grp)
!
      type(group_data), intent(inout) :: new_nod_grp
!
      integer(kind = kint) :: inod, inum, i, icou
!
      icou = 0
      do i = 1, nod_grp1%num_grp
         do inum = nod_grp1%istack_grp(i-1)+1, nod_grp1%istack_grp(i)
           inod = nod_grp1%item_grp(inum)
           if ( mark_new_node(inod) .ne. 0 ) then
             icou = icou + 1
             new_nod_grp%item_grp(icou) = mark_new_node(inod)
           end if
         end do
      end do
!
      end subroutine set_new_nod_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_equator_nod_group(new_node, new_nod_grp)
!
      type(node_data), intent(in) :: new_node
      type(group_data), intent(inout) :: new_nod_grp
!
      integer(kind = kint) :: inod
!
!
      new_nod_grp%grp_name(new_nod_grp%num_grp) = 'equator'
!
      new_nod_grp%istack_grp(new_nod_grp%num_grp)                       &
     &     = new_nod_grp%istack_grp(nod_grp1%num_grp)
      do inod = 1, new_node%numnod
        if ( abs(new_node%xx(inod,3)) .le. 1.0d-11 ) then
          new_nod_grp%istack_grp(new_nod_grp%num_grp) &
     &     = new_nod_grp%istack_grp(new_nod_grp%num_grp) + 1
        end if
      end do
      new_nod_grp%num_item = new_nod_grp%istack_grp(new_nod_grp%num_grp)
!
      end subroutine count_equator_nod_group
!
!  ---------------------------------------------------------------------
!
      subroutine set_equator_nod_group(new_node, new_nod_grp)
!
      type(node_data), intent(in) :: new_node
      type(group_data), intent(inout) :: new_nod_grp
!
      integer(kind = kint) :: inod, icou
!
      icou = new_nod_grp%istack_grp(nod_grp1%num_grp)
      do inod = 1, new_node%numnod
        if ( abs(new_node%xx(inod,3)) .le. 1.0d-11 ) then
          icou = icou + 1
          new_nod_grp%item_grp(icou) = inod
        end if
      end do
!
      end subroutine set_equator_nod_group
!
!  ---------------------------------------------------------------------
!
      end module set_cutshell_node_grp
