!set_refined_node_group.f90
!      module set_refined_node_group
!
!      Writen by H. Matsui on Oct., 2007
!
!      subroutine allocate_mark_refine_nod_grp(numnod)
!      subroutine deallocate_mark_refine_nod_grp
!
!      subroutine count_refined_node_group(nod_grp, new_nod_grp)
!      subroutine s_set_refined_node_group(nod_grp, new_nod_grp)
!
      module set_refined_node_group
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable, private :: inod_mark(:)
      private :: check_element_in_nod_group, set_new_nod_grp_item
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_mark_refine_nod_grp(numnod)
!
      integer(kind = kint), intent(in) :: numnod
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
      subroutine count_refined_node_group(nod_grp, new_nod_grp)
!
      use m_geometry_data
      use m_refined_node_id
      use t_group_data
!
      type(group_data), intent(in) :: nod_grp
      type(group_data), intent(inout) :: new_nod_grp
!
      integer(kind= kint) :: i, ist, ied, inum, iflag
      integer(kind= kint) :: inod, iele, iedge, isurf
!
!
      do i = 1, nod_grp%num_grp
        new_nod_grp%grp_name(i) = nod_grp%grp_name(i)
      end do
!
      new_nod_grp%istack_grp(0) = 0
      do i = 1, nod_grp%num_grp
        inod_mark(1:node1%numnod) = 0
!
        ist = nod_grp%istack_grp(i-1) + 1
        ied = nod_grp%istack_grp(i)
        do inum = ist, ied
          inod = nod_grp%item_grp(inum)
          inod_mark(inod) = 1
        end do
!
        new_nod_grp%istack_grp(i) = new_nod_grp%istack_grp(i-1)         &
     &                             + nod_grp%istack_grp(i)              &
     &                             - nod_grp%istack_grp(i-1)
!
        do iedge = 1, edge1%numedge
          call check_element_in_nod_group(iedge,                        &
     &        edge1%numedge, nnod_4_edge, ie_edge, iflag)
!
          if(iflag .eq. 1) then
            new_nod_grp%istack_grp(i) = new_nod_grp%istack_grp(i)       &
     &                        + num_nod_refine_edge(iedge)
          end if
        end do
!
!
        do isurf = 1, surf1%numsurf
          call check_element_in_nod_group                               &
     &       (isurf, surf1%numsurf, surf1%nnod_4_surf, ie_surf, iflag)
!
          if(iflag .eq. 1) then
            new_nod_grp%istack_grp(i) = new_nod_grp%istack_grp(i)       &
     &                        + num_nod_refine_surf(isurf)
          end if
        end do
!
!
        do iele = 1, ele1%numele
          call check_element_in_nod_group                               &
     &       (iele, ele1%numele, ele1%nnod_4_ele, ele1%ie, iflag)
!
          if(iflag .eq. 1) then
            new_nod_grp%istack_grp(i) = new_nod_grp%istack_grp(i)       &
     &                        + num_nod_refine_ele(iele)
          end if
        end do
!
      end do
      new_nod_grp%num_item = new_nod_grp%istack_grp(nod_grp%num_grp)
!
      end subroutine count_refined_node_group
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_refined_node_group(nod_grp, new_nod_grp)
!
      use m_geometry_data
      use m_refined_node_id
      use t_group_data
!
      type(group_data), intent(in) :: nod_grp
      type(group_data), intent(inout) :: new_nod_grp
!
      integer(kind = kint) :: i, icou, ist, ied, inum, iflag
      integer(kind = kint) :: inod, iedge, isurf, iele
!
!
      do i = 1, nod_grp%num_grp
        inod_mark(1:node1%numnod) = 0
!
        icou = new_nod_grp%istack_grp(i-1)
!
        ist = nod_grp%istack_grp(i-1) + 1
        ied = nod_grp%istack_grp(i)
        do inum = ist, ied
          icou = icou + 1
          inod = nod_grp%item_grp(inum)
          inod_mark(inod) = 1
          new_nod_grp%item_grp(icou) = inod
        end do
!
        do iedge = 1, edge1%numedge
          call check_element_in_nod_group(iedge,                        &
     &        edge1%numedge, nnod_4_edge, ie_edge, iflag)
!
          if(iflag .eq. 1) then
            call set_new_nod_grp_item(icou, ntot_nod_refine_edge,       &
     &          istack_nod_refine_edge(iedge-1), inod_refine_edge,      &
     &          new_nod_grp%num_item, new_nod_grp%item_grp)
          end if
        end do
!
!
        do isurf = 1, surf1%numsurf
          call check_element_in_nod_group                               &
     &       (isurf, surf1%numsurf, surf1%nnod_4_surf, ie_surf, iflag)
!
          if(iflag .eq. 1) then
            call set_new_nod_grp_item(icou, ntot_nod_refine_surf,       &
     &          istack_nod_refine_surf(isurf-1), inod_refine_surf,      &
     &          new_nod_grp%num_item, new_nod_grp%item_grp)
          end if
        end do
!
!
        do iele = 1, ele1%numele
          call check_element_in_nod_group                               &
     &       (iele, ele1%numele, ele1%nnod_4_ele, ele1%ie, iflag)
!
          if(iflag .eq. 1) then
            call set_new_nod_grp_item(icou, ntot_nod_refine_ele,        &
     &          istack_nod_refine_ele(iele-1), inod_refine_ele,         &
     &          new_nod_grp%num_item, new_nod_grp%item_grp)
          end if
        end do
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
     &          istack_nod_in_ele, inod_refine, num_item, item_grp)
!
      integer(kind = kint), intent(in) :: ntot_refine
      integer(kind = kint), intent(in) :: istack_nod_in_ele(0:1)
      integer(kind = kint), intent(in) :: inod_refine(ntot_refine)
      integer(kind = kint), intent(in) :: num_item
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: item_grp(num_item)
!
      integer(kind = kint) :: jst, jed, jnum
!
      jst = istack_nod_in_ele(0) + 1
      jed = istack_nod_in_ele(1)
      do jnum = jst, jed
        icou = icou + 1
        item_grp(icou) = inod_refine(jnum)
      end do
!
      end subroutine set_new_nod_grp_item
!
!  ---------------------------------------------------------------------
!
      end module set_refined_node_group
