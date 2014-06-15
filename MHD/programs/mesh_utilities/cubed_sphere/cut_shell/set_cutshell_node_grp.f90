!
!      module set_cutshell_node_grp
!
      module set_cutshell_node_grp
!
!     Written by H. Matsui
!
      use m_precision
!
      use m_node_group
      use m_2nd_group_data
      use m_cutshell_nod_ele_flag
!
      implicit none
!
      private :: count_new_nod_group, set_new_nod_group
      private :: count_equator_nod_group, set_equator_nod_group
!
!      subroutine s_set_new_node_grp_4_hemi
!      subroutine s_set_new_node_grp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_new_node_grp_4_hemi
!
       write(*,*) 'choose node group'
!
      nod_grp_2nd%num_grp =  num_bc + 1
      call allocate_grp_type_num(nod_grp_2nd)
!
      call count_new_nod_group
      call count_equator_nod_group
!
!
      call allocate_grp_type_item(nod_grp_2nd)
!
      call set_new_nod_group
      call set_equator_nod_group
!
      end subroutine s_set_new_node_grp_4_hemi
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_new_node_grp
!
       write(*,*) 'choose node group'
!
      nod_grp_2nd%num_grp =  num_bc
      call allocate_grp_type_num(nod_grp_2nd)
!
      call count_new_nod_group
!
      call allocate_grp_type_item(nod_grp_2nd)
      call set_new_nod_group
!
      end subroutine s_set_new_node_grp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_new_nod_group
!
      integer(kind = kint) :: i, inod, inum
!
      nod_grp_2nd%grp_name(1:num_bc) = bc_name(1:num_bc)
!
      nod_grp_2nd%istack_grp(0) = 0
      do i = 1, num_bc
         nod_grp_2nd%istack_grp(i) = nod_grp_2nd%istack_grp(i-1)
         do inum = bc_istack(i-1)+1, bc_istack(i)
           inod = bc_item(inum)
           if ( mark_new_node(inod) .ne. 0 ) then
             nod_grp_2nd%istack_grp(i) = nod_grp_2nd%istack_grp(i) + 1
           end if
         end do
      end do
      nod_grp_2nd%num_item = nod_grp_2nd%istack_grp(num_bc)
!
      end subroutine count_new_nod_group
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_nod_group
!
      integer(kind = kint) :: inod, inum, i, icou
!
      icou = 0
      do i = 1, num_bc
         do inum = bc_istack(i-1)+1, bc_istack(i)
           inod = bc_item(inum)
           if ( mark_new_node(inod) .ne. 0 ) then
             icou = icou + 1
             nod_grp_2nd%item_grp(icou) = mark_new_node(inod)
           end if
         end do
      end do
!
      end subroutine set_new_nod_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_equator_nod_group
!
      use m_2nd_geometry_data
!
      integer(kind = kint) :: inod
!
!
      nod_grp_2nd%grp_name(nod_grp_2nd%num_grp) = 'equator'
!
      nod_grp_2nd%istack_grp(nod_grp_2nd%num_grp) = nod_grp_2nd%istack_grp(num_bc)
      do inod = 1, node_2nd%numnod
        if ( abs(node_2nd%xx(inod,3)) .le. 1.0d-11 ) then
          nod_grp_2nd%istack_grp(nod_grp_2nd%num_grp) &
     &     = nod_grp_2nd%istack_grp(nod_grp_2nd%num_grp) + 1
        end if
      end do
      nod_grp_2nd%num_item = nod_grp_2nd%istack_grp(nod_grp_2nd%num_grp)
!
      end subroutine count_equator_nod_group
!
!  ---------------------------------------------------------------------
!
      subroutine set_equator_nod_group
!
      use m_2nd_geometry_data
!
      integer(kind = kint) :: inod, icou
!
      icou = nod_grp_2nd%istack_grp(num_bc)
      do inod = 1, node_2nd%numnod
        if ( abs(node_2nd%xx(inod,3)) .le. 1.0d-11 ) then
          icou = icou + 1
          nod_grp_2nd%item_grp(icou) = inod
        end if
      end do
!
      end subroutine set_equator_nod_group
!
!  ---------------------------------------------------------------------
!
      end module set_cutshell_node_grp
