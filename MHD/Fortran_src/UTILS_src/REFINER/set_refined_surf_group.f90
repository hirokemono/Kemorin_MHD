!set_refined_surf_group.f90
!      module set_refined_surf_group
!
!      Writen by H. Matsui on Oct., 2007
!
!      subroutine allocate_mark_refine_sf_grp(nnod_2nd)
!      subroutine deallocate_mark_refine_sf_grp
!
!      subroutine count_refined_surf_group
!      subroutine s_set_refined_surf_group
!
      module set_refined_surf_group
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint), allocatable, private :: inod_mark_2(:)
      private :: mark_refined_node_4_surf_grp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_mark_refine_sf_grp(nnod_2nd)
!
      integer(kind = kint), intent(in) :: nnod_2nd
!
!
      allocate(inod_mark_2(nnod_2nd))
      if(nnod_2nd .gt. 0) inod_mark_2 = 0
!
      end subroutine allocate_mark_refine_sf_grp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_mark_refine_sf_grp
!
      deallocate(inod_mark_2)
!
      end subroutine deallocate_mark_refine_sf_grp
!
!  ---------------------------------------------------------------------
!
      subroutine count_refined_surf_group(new_sf_grp)
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_surface_group
      use t_group_data
      use m_refined_element_data
!
      type(surface_group_data), intent(inout) :: new_sf_grp
!
      integer(kind = kint) :: i, ist, ied, inum
      integer(kind = kint) :: iele, isf
      integer(kind = kint) :: jst, jed, jele, jnod
      integer(kind = kint) :: k1, k2, k
      integer(kind = kint) :: iflag
!
!
      do i = 1, sf_grp1%num_grp
        new_sf_grp%grp_name(i) = surf_name(i)
      end do
!
      new_sf_grp%istack_grp(0) = 0
      do i = 1, sf_grp1%num_grp
        new_sf_grp%istack_grp(i) = new_sf_grp%istack_grp(i-1)
!
        ist = sf_grp1%istack_grp(i-1) + 1
        ied = sf_grp1%istack_grp(i)
        do inum = ist, ied
!
          iele = surf_item(1,inum)
          isf =  surf_item(2,inum)
          call mark_refined_node_4_surf_grp(iele, isf, ione)
!
          jst = istack_ele_refined(iele-1) + 1
          jed = istack_ele_refined(iele)
          do jele = jst, jed
            do k1 = 1, nsurf_4_ele
!
              iflag = 1
              do k2 = 1, nnod_4_surf
                k = node_on_sf(k2,k1)
                jnod = ie_refined(jele,k)
                iflag = iflag * inod_mark_2(jnod)
              end do
!
              if (iflag .eq. 1) then
                new_sf_grp%istack_grp(i) = new_sf_grp%istack_grp(i) + 1
              end if
!
            end do
          end do
!
          call mark_refined_node_4_surf_grp(iele, isf, izero)
!
        end do
      end do
      new_sf_grp%num_item = new_sf_grp%istack_grp(sf_grp1%num_grp)
!
      end subroutine count_refined_surf_group
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_refined_surf_group(new_sf_grp)
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_surface_group
      use t_group_data
      use m_refined_element_data
!
      type(surface_group_data), intent(inout) :: new_sf_grp
!
      integer(kind = kint) :: i, icou, ist, ied, inum
      integer(kind = kint) :: iele, isf
      integer(kind = kint) :: jst, jed, jele, jnod
      integer(kind = kint) :: k1, k2, k
      integer(kind = kint) :: iflag
!
!
      do i = 1, sf_grp1%num_grp
        icou = new_sf_grp%istack_grp(i-1)
!
        ist = sf_grp1%istack_grp(i-1) + 1
        ied = sf_grp1%istack_grp(i)
        do inum = ist, ied
!
          iele = surf_item(1,inum)
          isf =  surf_item(2,inum)
          call mark_refined_node_4_surf_grp(iele, isf, ione)
!
          jst = istack_ele_refined(iele-1) + 1
          jed = istack_ele_refined(iele)
          do jele = jst, jed
            do k1 = 1, nsurf_4_ele
!
              iflag = 1
              do k2 = 1, nnod_4_surf
                k = node_on_sf(k2,k1)
                jnod = ie_refined(jele,k)
                iflag = iflag * inod_mark_2(jnod)
              end do
!
              if (iflag .eq. 1) then
                icou = icou + 1
                new_sf_grp%item_sf_grp(1,icou) = jele
                new_sf_grp%item_sf_grp(2,icou) = k1
              end if
!
            end do
          end do
!
          call mark_refined_node_4_surf_grp(iele, isf, izero)
!
        end do
      end do
!
      end subroutine s_set_refined_surf_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mark_refined_node_4_surf_grp(iele, isf, mark_no)
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_refined_node_id
!
      integer(kind = kint), intent(in) :: iele, isf
      integer(kind = kint), intent(in) :: mark_no
!
      integer(kind = kint) :: isurf, inod, iedge, k1, k2
      integer(kind = kint) :: jst, jed, jnum, jnod
!
!
      isurf = abs(isf_4_ele(iele,isf))
      do k1 = 1, nnod_4_surf
        inod = ie_surf(isurf,k1)
        inod_mark_2(inod) = mark_no
      end do
!
      do k2 = 1, nedge_4_surf
        iedge = abs( iedge_4_sf(isurf,k2) )
        jst = istack_nod_refine_edge(iedge-1) + 1
        jed = istack_nod_refine_edge(iedge)
        do jnum = jst, jed
          jnod = jnum + ntot_nod_refine_nod
          inod_mark_2(jnod) = mark_no
        end do
      end do
!
      jst = istack_nod_refine_surf(isurf-1) + 1
      jed = istack_nod_refine_surf(isurf)
      do jnum = jst, jed
        jnod = jnum + ntot_nod_refine_nod + ntot_nod_refine_edge
        inod_mark_2(jnod) = mark_no
      end do
!
      end subroutine mark_refined_node_4_surf_grp
!
!  ---------------------------------------------------------------------
!
      end module set_refined_surf_group
