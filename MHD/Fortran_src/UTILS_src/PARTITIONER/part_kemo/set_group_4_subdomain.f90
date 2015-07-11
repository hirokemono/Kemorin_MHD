!set_group_4_subdomain.f90
!      module set_group_4_subdomain
!
!      Written by H. Matsui on Aug., 2007
!
!        subroutine count_local_node_group(new_nod_grp)
!        subroutine set_local_node_group(new_nod_grp)
!
!        subroutine count_local_ele_group(new_ele_grp)
!        subroutine set_local_ele_group(new_ele_grp)
!
!        subroutine count_local_surf_group(new_sf_grp)
!        subroutine set_local_surf_group(new_sf_grp)
!
      module set_group_4_subdomain
!
      use m_precision
!
      use t_group_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_local_node_group(new_nod_grp)
!
      use m_node_group
      use m_domain_group_4_partition
!
      type(group_data), intent(inout) :: new_nod_grp
!
      integer(kind = kint) :: ig, ist, ied, inum, inod
!
!
      new_nod_grp%grp_name(1:num_bc) = bc_name(1:num_bc)
!
      new_nod_grp%istack_grp(0) = 0
      do ig = 1, num_bc
        new_nod_grp%istack_grp(ig) = new_nod_grp%istack_grp(ig-1)
        ist = bc_istack(ig-1) + 1
        ied = bc_istack(ig)
        do inum = ist, ied
          inod = bc_item(inum)
!
          if (inod_local_part(inod) .gt. 0) then
            new_nod_grp%istack_grp(ig) = new_nod_grp%istack_grp(ig) + 1
          end if
        end do
      end do
      new_nod_grp%num_item = new_nod_grp%istack_grp(num_bc)
!
      end subroutine count_local_node_group
!
!   --------------------------------------------------------------------
!
      subroutine set_local_node_group(new_nod_grp)
!
      use m_node_group
      use m_domain_group_4_partition
!
      type(group_data), intent(inout) :: new_nod_grp
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
            new_nod_grp%item_grp(icou) = inod_local_part(inod)
          end if
        end do
      end do
!
      end subroutine set_local_node_group
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_local_ele_group(new_ele_grp)
!
      use m_element_group
      use m_domain_group_4_partition
!
      type(group_data), intent(inout) :: new_ele_grp
!
      integer(kind = kint) :: ig, ist, ied, inum, iele
!
!
      new_ele_grp%grp_name(1:num_mat) = mat_name(1:num_mat)
!
      new_ele_grp%istack_grp(0) = 0
      do ig = 1, num_mat
        new_ele_grp%istack_grp(ig) = new_ele_grp%istack_grp(ig-1)
        ist = mat_istack(ig-1) + 1
        ied = mat_istack(ig)
        do inum = ist, ied
          iele = mat_item(inum)
!
          if (iele_local_part(iele) .gt. 0) then
            new_ele_grp%istack_grp(ig) = new_ele_grp%istack_grp(ig) + 1
          end if
        end do
      end do
      new_ele_grp%num_item = new_ele_grp%istack_grp(num_mat)
!
      end subroutine count_local_ele_group
!
!   --------------------------------------------------------------------
!
      subroutine set_local_ele_group(new_ele_grp)
!
      use m_element_group
      use m_domain_group_4_partition
!
      type(group_data), intent(inout) :: new_ele_grp
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
            new_ele_grp%item_grp(icou) = iele_local_part(iele)
          end if
        end do
      end do
!
      end subroutine set_local_ele_group
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_local_surf_group(new_sf_grp)
!
      use m_surface_group
      use m_domain_group_4_partition
!
      type(surface_group_data), intent(inout) :: new_sf_grp
!
      integer(kind = kint) :: ig, ist, ied, inum, iele
!
!
      new_sf_grp%grp_name(1:sf_grp1%num_grp)                            &
     &     = sf_grp1%grp_name(1:sf_grp1%num_grp)
!
      new_sf_grp%istack_grp(0) = 0
      do ig = 1, sf_grp1%num_grp
        new_sf_grp%istack_grp(ig) = new_sf_grp%istack_grp(ig-1)
        ist = sf_grp1%istack_grp(ig-1) + 1
        ied = sf_grp1%istack_grp(ig)
        do inum = ist, ied
          iele = sf_grp1%item_sf_grp(1,inum)
          if (iele_local_part(iele) .gt. 0) then
            new_sf_grp%istack_grp(ig) = new_sf_grp%istack_grp(ig) + 1
          end if
        end do
      end do
      new_sf_grp%num_item = new_sf_grp%istack_grp(sf_grp1%num_grp)
!
      end subroutine count_local_surf_group
!
!   --------------------------------------------------------------------
!
      subroutine set_local_surf_group(new_sf_grp)
!
      use m_surface_group
      use m_domain_group_4_partition
!
      type(surface_group_data), intent(inout) :: new_sf_grp
!
      integer(kind = kint) :: ig, ist, ied, inum, iele, icou
!
!
      icou = 0
      do ig = 1, sf_grp1%num_grp
        icou = new_sf_grp%istack_grp(ig-1)
        ist = sf_grp1%istack_grp(ig-1) + 1
        ied = sf_grp1%istack_grp(ig)
        do inum = ist, ied
          iele = sf_grp1%item_sf_grp(1,inum)
!
          if (iele_local_part(iele) .gt. 0) then
            icou = icou + 1
            new_sf_grp%item_sf_grp(1,icou) = iele_local_part(iele)
            new_sf_grp%item_sf_grp(2,icou)                              &
     &                                 = sf_grp1%item_sf_grp(2,inum)
          end if
        end do
      end do
!
      end subroutine set_local_surf_group
!
!   --------------------------------------------------------------------
!
      end module set_group_4_subdomain
