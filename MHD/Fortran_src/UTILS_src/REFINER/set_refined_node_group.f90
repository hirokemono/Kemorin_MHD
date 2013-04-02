!set_refined_node_group.f90
!      module set_refined_node_group
!
      module set_refined_node_group
!
!      Writen by H. Matsui on Oct., 2007
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable, private :: inod_mark(:)
      private :: check_element_in_nod_group, set_new_nod_grp_item
!
!      subroutine allocate_mark_refine_nod_grp
!      subroutine deallocate_mark_refine_nod_grp
!
!      subroutine count_refined_node_group
!      subroutine s_set_refined_node_group
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_mark_refine_nod_grp
!
      use m_geometry_parameter
!
      allocate(inod_mark(numnod))
      inod_mark = 0
!
      end subroutine allocate_mark_refine_nod_grp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_mark_refine_nod_grp
!
      deallocate(inod_mark)
!
      end subroutine deallocate_mark_refine_nod_grp
!
!  ---------------------------------------------------------------------
!
      subroutine count_refined_node_group
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_group
      use m_2nd_group_data
      use m_refined_node_id
!
      integer(kind= kint) :: i, ist, ied, inum, iflag
      integer(kind= kint) :: inod, iele, iedge, isurf
!
!
      do i = 1, num_bc
        bc_name_2nd(i) = bc_name(i)
      end do
!
      bc_istack_2nd(0) = 0
      do i = 1, num_bc
        inod_mark(1:numnod) = 0
!
        ist = bc_istack(i-1) + 1
        ied = bc_istack(i)
        do inum = ist, ied
          inod = bc_item(inum)
          inod_mark(inod) = 1
        end do
!
        bc_istack_2nd(i) = bc_istack_2nd(i-1)                           &
     &                    + bc_istack(i) - bc_istack(i-1)
!
        do iedge = 1, numedge
          call check_element_in_nod_group(iedge, numedge, nnod_4_edge,  &
     &          ie_edge, iflag)
!
          if(iflag .eq. 1) then
            bc_istack_2nd(i) = bc_istack_2nd(i)                         &
     &                        + num_nod_refine_edge(iedge)
          end if
        end do
!
!
        do isurf = 1, numsurf
          call check_element_in_nod_group(isurf, numsurf, nnod_4_surf,  &
     &          ie_surf, iflag)
!
          if(iflag .eq. 1) then
            bc_istack_2nd(i) = bc_istack_2nd(i)                         &
     &                        + num_nod_refine_surf(isurf)
          end if
        end do
!
!
        do iele = 1, numele
          call check_element_in_nod_group(iele, numele, nnod_4_ele,     &
     &          ie, iflag)
!
          if(iflag .eq. 1) then
            bc_istack_2nd(i) = bc_istack_2nd(i)                         &
     &                        + num_nod_refine_ele(iele)
          end if
        end do
!
      end do
      num_nod_bc_2nd = bc_istack_2nd(num_bc)
!
      end subroutine count_refined_node_group
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_refined_node_group
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_group
      use m_2nd_group_data
      use m_refined_node_id
!
      integer(kind = kint) :: i, icou, ist, ied, inum, iflag
      integer(kind = kint) :: inod, iedge, isurf, iele
!
!
      do i = 1, num_bc
        inod_mark(1:numnod) = 0
!
        icou = bc_istack_2nd(i-1)
!
        ist = bc_istack(i-1) + 1
        ied = bc_istack(i)
        do inum = ist, ied
          icou = icou + 1
          inod = bc_item(inum)
          inod_mark(inod) = 1
          bc_item_2nd(icou) = inod
        end do
!
        do iedge = 1, numedge
          call check_element_in_nod_group(iedge, numedge, nnod_4_edge,  &
     &          ie_edge, iflag)
!
          if(iflag .eq. 1) then
            call set_new_nod_grp_item(icou, ntot_nod_refine_edge,       &
     &          istack_nod_refine_edge(iedge-1), inod_refine_edge,      &
     &          num_nod_bc_2nd, bc_item_2nd)
          end if
        end do
!
!
        do isurf = 1, numsurf
          call check_element_in_nod_group(isurf, numsurf, nnod_4_surf,  &
     &          ie_surf, iflag)
!
          if(iflag .eq. 1) then
            call set_new_nod_grp_item(icou, ntot_nod_refine_surf,       &
     &          istack_nod_refine_surf(isurf-1), inod_refine_surf,      &
     &          num_nod_bc_2nd, bc_item_2nd)
          end if
        end do
!
!
        do iele = 1, numele
          call check_element_in_nod_group(iele, numele, nnod_4_ele,     &
     &          ie, iflag)
!
          if(iflag .eq. 1) then
            call set_new_nod_grp_item(icou, ntot_nod_refine_ele,        &
     &          istack_nod_refine_ele(iele-1), inod_refine_ele,         &
     &          num_nod_bc_2nd, bc_item_2nd)
          end if
        end do
!
!
      end do
!
      end subroutine s_set_refined_node_group
!
!  ---------------------------------------------------------------------
!
      subroutine check_element_in_nod_group(iele, numele, nnod_4_ele,   &
     &          ie, iflag)
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(inout) :: iflag
!
      integer(kind = kint) :: inod, k1
!
!
      inod = ie(iele,1)
      iflag = inod_mark(inod)
      do k1 = 2, nnod_4_ele
        inod = ie(iele,k1)
        iflag = iflag * inod_mark(inod)
      end do
!
      end subroutine check_element_in_nod_group
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_nod_grp_item(icou, ntot_refine,                &
     &          istack_nod_in_ele, inod_refine, num_nod_bc_2nd,         &
     &          bc_item_2nd)
!
      integer(kind = kint), intent(in) :: ntot_refine
      integer(kind = kint), intent(in) :: istack_nod_in_ele(0:1)
      integer(kind = kint), intent(in) :: inod_refine(ntot_refine)
      integer(kind = kint), intent(in) :: num_nod_bc_2nd
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: bc_item_2nd(num_nod_bc_2nd)
!
      integer(kind = kint) :: jst, jed, jnum
!
      jst = istack_nod_in_ele(0) + 1
      jed = istack_nod_in_ele(1)
      do jnum = jst, jed
        icou = icou + 1
        bc_item_2nd(icou) = inod_refine(jnum)
      end do
!
      end subroutine set_new_nod_grp_item
!
!  ---------------------------------------------------------------------
!
      end module set_refined_node_group
