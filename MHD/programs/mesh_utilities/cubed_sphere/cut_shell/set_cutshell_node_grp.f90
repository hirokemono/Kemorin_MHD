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
      num_bc_2nd =  num_bc + 1
      call allocate_2nd_node_grp_num
!
      call count_new_nod_group
      call count_equator_nod_group
!
!
      call allocate_2nd_node_grp_item
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
      num_bc_2nd =  num_bc
      call allocate_2nd_node_grp_num
!
      call count_new_nod_group
!
      call allocate_2nd_node_grp_item
      call set_new_nod_group
!
      end subroutine s_set_new_node_grp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_new_nod_group
!
      integer(kind = kint) :: i, inod, inum, icou
!
      bc_name_2nd(1:num_bc) = bc_name(1:num_bc)
!
      bc_istack_2nd(0) = 0
      do i = 1, num_bc
         bc_istack_2nd(i) = bc_istack_2nd(i-1)
         do inum = bc_istack(i-1)+1, bc_istack(i)
           inod = bc_item(inum)
           if ( mark_new_node(inod) .ne. 0 ) then
             bc_istack_2nd(i) = bc_istack_2nd(i) + 1
           end if
         end do
      end do
      num_nod_bc_2nd = bc_istack_2nd(num_bc)
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
             bc_item_2nd(icou) = mark_new_node(inod)
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
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      integer(kind = kint) :: inod, icou
!
!
      bc_name_2nd(num_bc_2nd) = 'equator'
!
      bc_istack_2nd(num_bc_2nd) = bc_istack_2nd(num_bc)
      do inod = 1, nnod_2nd
        if ( abs(xx_2nd(inod,3)) .le. 1.0d-11 ) then
          bc_istack_2nd(num_bc_2nd) = bc_istack_2nd(num_bc_2nd) + 1
        end if
      end do
      num_nod_bc_2nd = bc_istack_2nd(num_bc_2nd)
!
      end subroutine count_equator_nod_group
!
!  ---------------------------------------------------------------------
!
      subroutine set_equator_nod_group
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      integer(kind = kint) :: inod, icou
!
      icou = bc_istack_2nd(num_bc)
      do inod = 1, nnod_2nd
        if ( abs(xx_2nd(inod,3)) .le. 1.0d-11 ) then
          icou = icou + 1
          bc_item_2nd(icou) = inod
        end if
      end do
!
      end subroutine set_equator_nod_group
!
!  ---------------------------------------------------------------------
!
      end module set_cutshell_node_grp
