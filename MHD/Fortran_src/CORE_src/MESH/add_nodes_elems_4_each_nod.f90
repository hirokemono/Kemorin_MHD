!add_nodes_elems_4_each_nod.f90
!      module add_nodes_elems_4_each_nod
!
!     Written by H. Matsui on Mar., 2008
!
!      subroutine allocate_wk_exp_ele_nod_each(numnod, numele)
!      subroutine deallocate_wk_exp_ele_nod_each
!
!      subroutine expand_near_ele_4_each_nod(numnod, numele,            &
!     &          ntot_ele_4_node, iele_stack_4_node, iele_4_node,       &
!     &          nnod_near_org, inod_near_org, nele_near_org, nele_near,&
!     &          iele_near)
!      subroutine add_nod_4_grp_each_nod(numnod, numele,                &
!     &          nnod_4_ele, ie, num_grp, iele_grp, nnod_grp_org,       &
!     &          nnod_grp, inod_grp, iweight_grp, idist_grp)
!      subroutine sort_added_nod_4_each_nod(numnod,                     &
!     &          nnod_grp_org, nnod_grp, inod_grp, iweight_grp)
!      subroutine sort_added_nod_4_each_by_real(numnod,                 &
!     &          nnod_grp_org, nnod_grp, inod_grp, weight_grp)
!
      module add_nodes_elems_4_each_nod
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable :: imark_4_ele(:)
      integer(kind = kint), allocatable :: imark_4_node(:)
      private :: imark_4_ele, imark_4_node
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_wk_exp_ele_nod_each(numnod, numele)
!
      integer(kind = kint), intent(in) :: numnod, numele
!
      allocate(imark_4_ele(numele) )
      allocate(imark_4_node(numnod) )
!
      imark_4_node = 0
      imark_4_ele = 0
!
      end subroutine allocate_wk_exp_ele_nod_each
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_wk_exp_ele_nod_each
!
      deallocate(imark_4_ele)
      deallocate(imark_4_node)
!
      end subroutine deallocate_wk_exp_ele_nod_each
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine expand_near_ele_4_each_nod(numnod, numele,             &
     &          ntot_ele_4_node, iele_stack_4_node, iele_4_node,        &
     &          nnod_near_org, inod_near_org, nele_near_org, nele_near, &
     &          iele_near)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: ntot_ele_4_node
      integer(kind = kint), intent(in) :: iele_stack_4_node(0:numnod)
      integer(kind = kint), intent(in) :: iele_4_node(ntot_ele_4_node)
      integer(kind = kint), intent(in) :: nnod_near_org
      integer(kind = kint), intent(in) :: inod_near_org(numnod)
      integer(kind = kint), intent(in) :: nele_near_org
!
      integer(kind = kint), intent(inout) :: nele_near
      integer(kind = kint), intent(inout) :: iele_near(numele)
!
      integer(kind = kint) :: jnod, jele
      integer(kind = kint) :: inum, jst, jed, jnum
!
!
      imark_4_ele(1:numele) = 0
      nele_near = nele_near_org
!
!     mark current elements
!
      do jnum = 1, nele_near_org
        jele = iele_near(jnum)
        imark_4_ele(jele) = 1
      end do
!
!     mark expanded elements
!
      do inum = 1, nnod_near_org
!
        jnod = inod_near_org(inum)
        jst = iele_stack_4_node(jnod-1) + 1
        jed = iele_stack_4_node(jnod)
        do jnum = jst, jed
!
          jele = iele_4_node(jnum)
          if ( imark_4_ele(jele) .eq. 0) then
            imark_4_ele(jele) = 1
            nele_near = nele_near + 1
            iele_near(nele_near) = jele
          end if
        end do
!
      end do
!
      jst = nele_near_org + 1
      jed = nele_near
      call quicksort_int(nele_near, iele_near(1), jst, jed)
!
      end subroutine expand_near_ele_4_each_nod
!
!-----------------------------------------------------------------------
!
      subroutine add_nod_4_grp_each_nod(numnod, numele,                 &
     &          nnod_4_ele, ie, num_grp, iele_grp, nnod_grp_org,        &
     &          nnod_grp, inod_grp, iweight_grp, idist_grp)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: num_grp
      integer(kind = kint), intent(in) :: iele_grp(num_grp)
      integer(kind = kint), intent(in) :: nnod_grp_org
!
      integer(kind = kint), intent(inout) :: nnod_grp
      integer(kind = kint), intent(inout) :: inod_grp(numnod)
      integer(kind = kint), intent(inout) :: iweight_grp(numnod)
      integer(kind = kint), intent(inout) :: idist_grp(numnod)
!
      integer(kind = kint) :: iele, inod, k, jnum
!
!
      nnod_grp =  nnod_grp_org
      imark_4_node(1:numnod) = 0
!
      do jnum = 1, nnod_grp_org
        inod = inod_grp(jnum)
        imark_4_node(inod) = iweight_grp(jnum)
      end do
!
      do jnum = 1, num_grp
        iele = abs(iele_grp(jnum))
!
        do k = 1, nnod_4_ele
          inod = abs( ie(iele,k) )
          if (imark_4_node(inod) .eq. 0) then
            nnod_grp = nnod_grp + 1
            inod_grp(nnod_grp) = inod
          end if
          imark_4_node(inod) = imark_4_node(inod) + 1
        end do
!
      end do
!
      do jnum = (nnod_grp_org+1), nnod_grp
        inod = inod_grp(jnum)
        iweight_grp(jnum) = imark_4_node(inod)
        idist_grp(jnum) = idist_grp(nnod_grp_org) + 1
      end do
!
      end subroutine add_nod_4_grp_each_nod
!
!-----------------------------------------------------------------------
!
      subroutine sort_added_nod_4_each_nod(numnod,                      &
     &          nnod_grp_org, nnod_grp, inod_grp, iweight_grp)
!
      use quicksort
!
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: nnod_grp_org
      integer(kind = kint), intent(in) :: nnod_grp
!
      integer(kind = kint), intent(inout) :: inod_grp(numnod)
      integer(kind = kint), intent(inout) :: iweight_grp(numnod)
!
      integer(kind = kint) :: ist, ied, inum, jst, jed, jnum
!
!
      iweight_grp(1:nnod_grp) = - iweight_grp(1:nnod_grp)
!
      ist = nnod_grp_org + 1
      ied = nnod_grp
      call quicksort_w_index(nnod_grp, iweight_grp(1), ist, ied,        &
     &        inod_grp(1) )
!
      iweight_grp(1:nnod_grp) = - iweight_grp(1:nnod_grp)
!
      end subroutine sort_added_nod_4_each_nod
!
!-----------------------------------------------------------------------
!
      subroutine sort_added_nod_4_each_by_real(numnod,                  &
     &          nnod_grp_org, nnod_grp, inod_grp, weight_grp)
!
      use quicksort
!
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: nnod_grp_org
      integer(kind = kint), intent(in) :: nnod_grp
!
      integer(kind = kint), intent(inout) :: inod_grp(numnod)
      real(kind = kreal), intent(inout) :: weight_grp(numnod)
!
      integer(kind = kint) :: ist, ied, inum, jst, jed, jnum
!
!
      ist = nnod_grp_org + 1
      ied = nnod_grp
      call quicksort_real_w_index(nnod_grp, weight_grp(1), ist, ied,    &
     &    inod_grp(1) )
!
      end subroutine sort_added_nod_4_each_by_real
!
!-----------------------------------------------------------------------
!
      end module add_nodes_elems_4_each_nod
