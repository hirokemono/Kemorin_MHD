!
!      module const_node_list_4_viewer
!
      module const_node_list_4_viewer
!
!      Written by Kemorin on Jan., 2007
!
      use m_precision
!
      use m_surface_mesh_4_merge
      use m_pickup_table_4_viewer
!
      implicit none
!
!      subroutine mark_node_4_domain_viewer(nnod_4_surf, nnod_4_edge)
!      subroutine mark_node_4_ele_grp_viewer                            &
!     &         (igrp, nnod_4_surf, nnod_4_edge)
!      subroutine mark_node_4_surf_grp_viewer                           &
!     &         (igrp, nnod_4_surf, nnod_4_edge)
!
!      subroutine count_nod_stack_4_domain_viewer
!      subroutine count_nod_stack_4_ele_gp_viewer(igrp)
!      subroutine count_nod_stack_4_sf_gp_viewer(igrp)
!
!      subroutine const_nod_4_domain_viewer
!      subroutine const_nod_4_ele_gp_viewer(igrp)
!      subroutine const_nod_4_sf_gp_viewer(igrp)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine mark_node_4_domain_viewer(nnod_4_surf, nnod_4_edge)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
!
      integer(kind = kint) :: k1, inum, isurf, iedge, inod
!
!
      imark_node = 0
      do k1 = 1, nnod_4_edge
        do inum = 1, nedge_domain_sf
          iedge = abs(edge_item_domain_sf(inum))
          inod = ie_edge_viewer(iedge,k1)
          imark_node(inod) = 1
        end do
      end do
!
      if (nnod_4_surf .eq. num_lag_sf) then
        do inum = 1, nedge_domain_sf
          isurf = isurf_domain_sf(inum)
          inod = ie_sf_viewer(isurf,num_lag_sf)
          imark_node(inod) = 1
        end do
      end if
!
      end subroutine mark_node_4_domain_viewer
!
!------------------------------------------------------------------
!
      subroutine mark_node_4_ele_grp_viewer                             &
     &         (igrp, nnod_4_surf, nnod_4_edge)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: igrp
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
      integer(kind = kint) :: k1, ist, ied, inum, isurf, iedge, inod
!
!
      imark_node = 0
!
      ist = ele_edge_stack_sf( (igrp-1)*num_pe_sf ) + 1
      ied = ele_edge_stack_sf( (igrp  )*num_pe_sf )
      do k1 = 1, nnod_4_edge
        do inum = ist, ied
          iedge = abs(ele_edge_item_sf(inum))
          inod = ie_edge_viewer(iedge,k1)
          imark_node(inod) = 1
        end do
      end do
!
      if (nnod_4_surf .eq. num_lag_sf) then
        ist = ele_stack_sf( (igrp-1)*num_pe_sf ) + 1
        ied = ele_stack_sf( (igrp  )*num_pe_sf )
        do inum = 1, nedge_domain_sf
          isurf = abs(ele_item_sf(inum))
          inod = ie_sf_viewer(isurf,num_lag_sf)
          imark_node(inod) = 1
        end do
      end if
!
      end subroutine mark_node_4_ele_grp_viewer
!
!------------------------------------------------------------------
!
      subroutine mark_node_4_surf_grp_viewer                            &
     &         (igrp, nnod_4_surf, nnod_4_edge)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: igrp
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
      integer(kind = kint) :: k1, ist, ied, inum, isurf, iedge, inod
!
!
      imark_node = 0
!
      ist = sf_edge_grp%istack_sf( (igrp-1)*num_pe_sf ) + 1
      ied = sf_edge_grp%istack_sf( (igrp  )*num_pe_sf )
      do k1 = 1, nnod_4_edge
        do inum = ist, ied
          iedge = abs( sf_edge_grp%item_sf(inum) )
          inod = ie_edge_viewer(iedge,k1)
          imark_node(inod) = 1
        end do
      end do
!
      if (nnod_4_surf .eq. num_lag_sf) then
        ist = sf_surf_grp%istack_sf( (igrp-1)*num_pe_sf ) + 1
        ied = sf_surf_grp%istack_sf( (igrp  )*num_pe_sf )
        do inum = 1, nedge_domain_sf
          isurf = sf_surf_grp%item_sf(inum)
          inod = ie_sf_viewer(isurf,num_lag_sf)
          imark_node(inod) = 1
        end do
      end if
!
      end subroutine mark_node_4_surf_grp_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_nod_stack_4_domain_viewer
!
      integer(kind = kint) :: ip, ist, ied, inod
!
      do ip = 1, num_pe_sf
        ist = inod_sf_stack(ip-1) + 1
        ied = inod_sf_stack(ip)
        nod_stack_domain_sf(ip) = nod_stack_domain_sf(ip-1)
        do inod = ist, ied
          nod_stack_domain_sf(ip) = nod_stack_domain_sf(ip)             &
     &                             + imark_node(inod)
        end do
      end do
      nnod_domain_sf = nod_stack_domain_sf(num_pe_sf)
!
      end subroutine count_nod_stack_4_domain_viewer
!
!------------------------------------------------------------------
!
      subroutine count_nod_stack_4_ele_gp_viewer(igrp)
!
      integer(kind = kint), intent(in) :: igrp
      integer(kind = kint) :: ip, idx, ist, ied, inod
!
      do ip = 1, num_pe_sf
        idx = (igrp-1)*num_pe_sf + ip
        ist = inod_sf_stack(ip-1) + 1
        ied = inod_sf_stack(ip)
        ele_nod_stack_sf(idx) = ele_nod_stack_sf(idx-1)
        do inod = ist, ied
          ele_nod_stack_sf(idx) = ele_nod_stack_sf(idx)                 &
     &                           + imark_node(inod)
        end do
      end do
      nnod_ele_sf = ele_nod_stack_sf( igrp*num_pe_sf )
!
      end subroutine count_nod_stack_4_ele_gp_viewer
!
!------------------------------------------------------------------
!
      subroutine count_nod_stack_4_sf_gp_viewer(igrp)
!
      integer(kind = kint), intent(in) :: igrp
      integer(kind = kint) :: ip, idx, ist, ied, inod
!
      do ip = 1, num_pe_sf
        idx = (igrp-1)*num_pe_sf + ip
        ist = inod_sf_stack(ip-1) + 1
        ied = inod_sf_stack(ip)
        sf_nod_grp%istack_sf(idx) = sf_nod_grp%istack_sf(idx-1)
        do inod = ist, ied
          sf_nod_grp%istack_sf(idx) = sf_nod_grp%istack_sf(idx)         &
     &                               + imark_node(inod)
        end do
      end do
      sf_nod_grp%num_item = sf_nod_grp%istack_sf(igrp*num_pe_sf)
!
      end subroutine count_nod_stack_4_sf_gp_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine const_nod_4_domain_viewer
!
      integer(kind = kint) :: ip, ist, ied, inod, icou
!
      do ip = 1, num_pe_sf
        ist = inod_sf_stack(ip-1) + 1
        ied = inod_sf_stack(ip)
        icou = nod_stack_domain_sf(ip-1)
        do inod = ist, ied
          if (imark_node(inod) .eq. 1) then
            icou = icou + 1
            nod_item_domain_sf(icou) = inod
          end if
        end do
      end do
!
      end subroutine const_nod_4_domain_viewer
!
!------------------------------------------------------------------
!
      subroutine const_nod_4_ele_gp_viewer(igrp)
!
      integer(kind = kint), intent(in) :: igrp
      integer(kind = kint) :: ip, idx, ist, ied, inod, icou
!
      do ip = 1, num_pe_sf
        idx = (igrp-1)*num_pe_sf + ip
        ist = inod_sf_stack(ip-1) + 1
        ied = inod_sf_stack(ip)
        icou = ele_nod_stack_sf(idx-1)
        do inod = ist, ied
          if (imark_node(inod) .eq. 1) then
            icou = icou + 1
            ele_nod_item_sf(icou) = inod
          end if
        end do
      end do
!
      end subroutine const_nod_4_ele_gp_viewer
!
!------------------------------------------------------------------
!
      subroutine const_nod_4_sf_gp_viewer(igrp)
!
      integer(kind = kint), intent(in) :: igrp
      integer(kind = kint) :: ip, idx, ist, ied, inod, icou
!
      do ip = 1, num_pe_sf
        idx = (igrp-1)*num_pe_sf + ip
        ist = inod_sf_stack(ip-1) + 1
        ied = inod_sf_stack(ip)
        icou = sf_nod_grp%istack_sf(idx-1)
        do inod = ist, ied
          if (imark_node(inod) .eq. 1) then
            icou = icou + 1
            sf_nod_grp%item_sf(icou) = inod
          end if
        end do
      end do
!
      end subroutine const_nod_4_sf_gp_viewer
!
!------------------------------------------------------------------
!
      end module const_node_list_4_viewer
