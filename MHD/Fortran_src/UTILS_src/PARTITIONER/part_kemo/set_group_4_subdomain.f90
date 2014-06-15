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
      nod_grp_2nd%grp_name(1:num_bc) = bc_name(1:num_bc)
!
      nod_grp_2nd%istack_grp(0) = 0
      do ig = 1, num_bc
        nod_grp_2nd%istack_grp(ig) = nod_grp_2nd%istack_grp(ig-1)
        ist = bc_istack(ig-1) + 1
        ied = bc_istack(ig)
        do inum = ist, ied
          inod = bc_item(inum)
!
          if (inod_local_part(inod) .gt. 0) then
            nod_grp_2nd%istack_grp(ig) = nod_grp_2nd%istack_grp(ig) + 1
          end if
        end do
      end do
      nod_grp_2nd%num_item = nod_grp_2nd%istack_grp(num_bc)
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
            nod_grp_2nd%item_grp(icou) = inod_local_part(inod)
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
      ele_grp_2nd%grp_name(1:num_mat) = mat_name(1:num_mat)
!
      ele_grp_2nd%istack_grp(0) = 0
      do ig = 1, num_mat
        ele_grp_2nd%istack_grp(ig) = ele_grp_2nd%istack_grp(ig-1)
        ist = mat_istack(ig-1) + 1
        ied = mat_istack(ig)
        do inum = ist, ied
          iele = mat_item(inum)
!
          if (iele_local_part(iele) .gt. 0) then
            ele_grp_2nd%istack_grp(ig) = ele_grp_2nd%istack_grp(ig) + 1
          end if
        end do
      end do
      ele_grp_2nd%num_item = ele_grp_2nd%istack_grp(num_mat)
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
            ele_grp_2nd%item_grp(icou) = iele_local_part(iele)
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
      sf_grp_2nd%grp_name(1:num_surf) = surf_name(1:num_surf)
!
      sf_grp_2nd%istack_grp(0) = 0
      do ig = 1, num_surf
        sf_grp_2nd%istack_grp(ig) = sf_grp_2nd%istack_grp(ig-1)
        ist = surf_istack(ig-1) + 1
        ied = surf_istack(ig)
        do inum = ist, ied
          iele = surf_item(1,inum)
          if (iele_local_part(iele) .gt. 0) then
            sf_grp_2nd%istack_grp(ig) = sf_grp_2nd%istack_grp(ig) + 1
          end if
        end do
      end do
      sf_grp_2nd%num_item = sf_grp_2nd%istack_grp(num_surf)
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
        icou = sf_grp_2nd%istack_grp(ig-1)
        ist = surf_istack(ig-1) + 1
        ied = surf_istack(ig)
        do inum = ist, ied
          iele = surf_item(1,inum)
!
          if (iele_local_part(iele) .gt. 0) then
            icou = icou + 1
            sf_grp_2nd%item_sf_grp(1,icou) = iele_local_part(iele)
            sf_grp_2nd%item_sf_grp(2,icou) = surf_item(2,inum)
          end if
        end do
      end do
!
      end subroutine set_local_surf_group
!
!   --------------------------------------------------------------------
!
      end module set_group_4_subdomain
