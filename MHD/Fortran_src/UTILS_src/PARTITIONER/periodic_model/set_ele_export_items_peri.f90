!
!      module set_ele_export_items_peri
!
      module set_ele_export_items_peri
!
!     Written by H. Matsui on Aug., 2007
!
      use m_precision
!
      implicit  none
!
!      subroutine set_ele_export_item_peri(id_glnod_org)
!      subroutine set_surf_export_item_peri(id_glnod_org)
!      subroutine set_edge_export_item_peri(id_glnod_org)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_ele_export_item_peri(id_glnod_org)
!
      use m_ele_comm_table
      use m_geometry_parameter
      use m_geometry_data
      use m_element_id_4_node
!
      integer(kind = kint), intent(in) :: id_glnod_org(numnod)
!
      integer(kind = kint) :: inum, iele_im
      integer(kind = kint) :: inod, inod_org, k1
      integer(kind = kint) :: jst, jed, jnum, jele, jnod
      integer(kind = kint) :: iflag
!
!
      do inum = 1, num_import_ele(1)
        iele_im = item_import_ele(inum)
        inod = ie(iele_im,1)
        inod_org = id_glnod_org(inod)
        jst = iele_stack_4_node(inod_org-1) +1
        jed = iele_stack_4_node(inod_org)
        do jnum = jst, jed
          jele = iele_4_node(jnum)
          iflag = 0
          do k1 = 1, nnod_4_ele
            inod = ie(iele_im,k1)
            jnod = ie(jele,k1)
            if ( id_glnod_org(inod) .ne. id_glnod_org(jnod) ) then
              iflag = 1
              exit
            end if
          end do
!
          if (iflag .eq. 0) then
            item_export_ele(inum) = jele
            go to 11
          end if
        end do
 11     continue
      end do
!
      end subroutine set_ele_export_item_peri
!
!------------------------------------------------------------------
!
      subroutine set_surf_export_item_peri(id_glnod_org)
!
      use m_surf_comm_table
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_element_id_4_node
!
      integer(kind = kint), intent(in) :: id_glnod_org(numnod)
!
      integer(kind = kint) :: inum, isurf_im
      integer(kind = kint) :: inod, inod_org, k1, k2
      integer(kind = kint) :: jst, jed, jnum, jele, jsurf, jnod
      integer(kind = kint) :: iflag
!
!
      do inum = 1, num_import_surf(1)
        isurf_im = item_import_surf(inum)
        inod = ie_surf(isurf_im,1)
        inod_org = id_glnod_org(inod)
        jst = iele_stack_4_node(inod_org-1) +1
        jed = iele_stack_4_node(inod_org)
        do jnum = jst, jed
          jele = iele_4_node(jnum)
          do k2 = 1, nsurf_4_ele
            jsurf = abs(isf_4_ele(jele,k2))
            iflag = 0
            do k1 = 1, nnod_4_surf
              inod = ie_surf(isurf_im,k1)
              jnod = ie_surf(jsurf,k1)
              if ( id_glnod_org(inod) .ne. id_glnod_org(jnod) ) then
                iflag = 1
                exit
              end if
            end do
!
            if (iflag .eq. 0) then
              item_export_surf(inum) = jsurf
              go to 12
            end if
          end do
        end do
 12     continue
      end do
!
      end subroutine set_surf_export_item_peri
!
!------------------------------------------------------------------
!
      subroutine set_edge_export_item_peri(id_glnod_org)
!
      use m_edge_comm_table
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_element_id_4_node
!
      integer(kind = kint), intent(in) :: id_glnod_org(numnod)
!
      integer(kind = kint) :: inum, iedge_im
      integer(kind = kint) :: inod, inod_org, k1, k2
      integer(kind = kint) :: jst, jed, jnum, jele, jedge, jnod
      integer(kind = kint) :: iflag
!
      do inum = 1, num_import_edge(1)
        iedge_im = item_import_edge(inum)
        inod = ie_edge(iedge_im,1)
        inod_org = id_glnod_org(inod)
        jst = iele_stack_4_node(inod_org-1) +1
        jed = iele_stack_4_node(inod_org)
        do jnum = jst, jed
          jele = iele_4_node(jnum)
          do k2 = 1, nedge_4_ele
            jedge = abs(iedge_4_ele(jele,k2))
            iflag = 0
            do k1 = 1, nnod_4_edge
              inod = ie_edge(iedge_im,k1)
              jnod = ie_edge(jedge,k1)
              if ( id_glnod_org(inod) .ne. id_glnod_org(jnod) ) then
                iflag = 1
                exit
              end if
            end do
!
            if (iflag .eq. 0) then
              item_export_edge(inum) = jedge
              go to 13
            end if
          end do
        end do
 13     continue
      end do
!
      end subroutine set_edge_export_item_peri
!
!------------------------------------------------------------------
!
      end module set_ele_export_items_peri
