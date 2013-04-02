!set_group_4_subdomain.f90
!      module set_group_4_subdomain
!
      module set_group_4_subdomain
!
!      Written by H. Matsui on Aug., 2007
!
      use m_precision
!
      implicit none
!
!
!        subroutine count_local_node_group
!        subroutine set_local_node_group
!
!        subroutine count_local_ele_group
!        subroutine set_local_ele_group
!
!        subroutine count_local_surf_group
!        subroutine set_local_surf_group
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_local_node_group
!
      use m_node_group
      use m_2nd_group_data
      use m_domain_group_4_partition
!
      integer(kind = kint) :: ig, ist, ied, inum, inod
!
!
      bc_name_2nd(1:num_bc) = bc_name(1:num_bc)
!
      bc_istack_2nd(0) = 0
      do ig = 1, num_bc
        bc_istack_2nd(ig) = bc_istack_2nd(ig-1)
        ist = bc_istack(ig-1) + 1
        ied = bc_istack(ig)
        do inum = ist, ied
          inod = bc_item(inum)
!
          if (inod_local_part(inod) .gt. 0) then
            bc_istack_2nd(ig) = bc_istack_2nd(ig) + 1
          end if
        end do
      end do
      num_nod_bc_2nd = bc_istack_2nd(num_bc)
!
      end subroutine count_local_node_group
!
!   --------------------------------------------------------------------
!
      subroutine set_local_node_group
!
      use m_node_group
      use m_2nd_group_data
      use m_domain_group_4_partition
!
      integer(kind = kint) :: ig, ist, ied, inum, inod, icou
!
!
      icou = 0
      do ig = 1, num_bc
        ist = bc_istack(ig-1) + 1
        ied = bc_istack(ig)
        do inum = ist, ied
          inod = bc_item(inum)
!
          if (inod_local_part(inod) .gt. 0) then
            icou = icou + 1
            bc_item_2nd(icou) = inod_local_part(inod)
          end if
        end do
      end do
!
      end subroutine set_local_node_group
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_local_ele_group
!
      use m_element_group
      use m_2nd_group_data
      use m_domain_group_4_partition
!
      integer(kind = kint) :: ig, ist, ied, inum, iele
!
!
      mat_name_2nd(1:num_mat) = mat_name(1:num_mat)
!
      mat_istack_2nd(0) = 0
      do ig = 1, num_mat
        mat_istack_2nd(ig) = mat_istack_2nd(ig-1)
        ist = mat_istack(ig-1) + 1
        ied = mat_istack(ig)
        do inum = ist, ied
          iele = mat_item(inum)
!
          if (iele_local_part(iele) .gt. 0) then
            mat_istack_2nd(ig) = mat_istack_2nd(ig) + 1
          end if
        end do
      end do
      num_mat_bc_2nd = mat_istack_2nd(num_mat)
!
      end subroutine count_local_ele_group
!
!   --------------------------------------------------------------------
!
      subroutine set_local_ele_group
!
      use m_element_group
      use m_2nd_group_data
      use m_domain_group_4_partition
!
      integer(kind = kint) :: ig, ist, ied, inum, iele, icou
!
!
      icou = 0
      do ig = 1, num_mat
        ist = mat_istack(ig-1) + 1
        ied = mat_istack(ig)
        do inum = ist, ied
          iele = mat_item(inum)
!
          if (iele_local_part(iele) .gt. 0) then
            icou = icou + 1
            mat_item_2nd(icou) = iele_local_part(iele)
          end if
        end do
      end do
!
      end subroutine set_local_ele_group
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_local_surf_group
!
      use m_surface_group
      use m_2nd_group_data
      use m_domain_group_4_partition
!
      integer(kind = kint) :: ig, ist, ied, inum, iele
!
!
      surf_name_2nd(1:num_surf) = surf_name(1:num_surf)
!
      surf_istack_2nd(0) = 0
      do ig = 1, num_surf
        surf_istack_2nd(ig) = surf_istack_2nd(ig-1)
        ist = surf_istack(ig-1) + 1
        ied = surf_istack(ig)
        do inum = ist, ied
          iele = surf_item(1,inum)
          if (iele_local_part(iele) .gt. 0) then
            surf_istack_2nd(ig) = surf_istack_2nd(ig) + 1
          end if
        end do
      end do
      num_surf_bc_2nd = surf_istack_2nd(num_surf)
!
      end subroutine count_local_surf_group
!
!   --------------------------------------------------------------------
!
      subroutine set_local_surf_group
!
      use m_surface_group
      use m_2nd_group_data
      use m_domain_group_4_partition
!
      integer(kind = kint) :: ig, ist, ied, inum, iele, icou
!
!
      icou = 0
      do ig = 1, num_surf
        icou = surf_istack_2nd(ig-1)
        ist = surf_istack(ig-1) + 1
        ied = surf_istack(ig)
        do inum = ist, ied
          iele = surf_item(1,inum)
!
          if (iele_local_part(iele) .gt. 0) then
            icou = icou + 1
            surf_item_2nd(1,icou) = iele_local_part(iele)
            surf_item_2nd(2,icou) = surf_item(2,inum)
          end if
        end do
      end do
!
      end subroutine set_local_surf_group
!
!   --------------------------------------------------------------------
!
      end module set_group_4_subdomain
