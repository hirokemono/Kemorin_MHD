!
!      module count_edge_domain_4_viewer
!
!     Written by H. Matsui on Jan., 2007
!
!!      subroutine count_nedge_4_each_domain
!!      subroutine count_nedge_domain_4_domain
!!      subroutine count_nedge_ele_grp_4_domain(ngrp_ele_sf)
!!      subroutine count_nedge_surf_grp_4_domain                        &
!!     &         (ngrp_surf_sf, sf_edge_grp)
!!       type(viewer_group_data), intent(inout)  :: sf_edge_grp
!
      module count_edge_domain_4_viewer
!
      use m_precision
      use m_surface_mesh_4_merge
!
      implicit    none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine count_nedge_4_each_domain
!
      integer(kind = kint) :: ip, iref, ist, iedge, inod
!
!
      do ip = 1, num_pe_sf
        iref = inod_sf_stack(ip)
        ist =  iedge_sf_stack(ip-1) + 1
        do iedge = ist, edgepetot_viewer
          inod = ie_edge_viewer(iedge,1)
          if ( inod .gt. iref ) exit
          iedge_sf_stack(ip:num_pe_sf) = iedge
        end do
      end do
!
      end subroutine count_nedge_4_each_domain
!
!------------------------------------------------------------------
!
      subroutine count_nedge_domain_4_domain
!
      integer(kind = kint) :: ip, iref, ist, inum, iedge, inod
!
!
      do ip = 1, num_pe_sf
        iref = inod_sf_stack(ip)
        ist =  domain_edge_grp%istack_sf(ip-1) + 1
        do inum = ist, domain_edge_grp%num_item
          iedge = abs(domain_edge_grp%item_sf(inum) )
          inod = ie_edge_viewer(iedge,1)
          if ( inod .gt. iref ) exit
          domain_edge_grp%istack_sf(ip:num_pe_sf) = inum
        end do
      end do
!
      end subroutine count_nedge_domain_4_domain
!
!------------------------------------------------------------------
!
      subroutine count_nedge_ele_grp_4_domain(ngrp_ele_sf)
!
      integer(kind = kint), intent(in) :: ngrp_ele_sf
!
      integer(kind = kint) :: igrp, ip, iref, ist, inum, iedge, inod
      integer(kind = kint) :: nn, ist_grp
!
!
      do igrp = 1, ngrp_ele_sf
        ist_grp = (igrp-1)*num_pe_sf
        nn = ele_edge_grp%istack_sf(igrp*num_pe_sf)
        do ip = 1, num_pe_sf
          iref = inod_sf_stack(ip)
          ist =  ele_edge_grp%istack_sf(ist_grp+ip-1) + 1
          ele_edge_grp%istack_sf(ist_grp+ip)                            &
     &          = ele_edge_grp%istack_sf(ist_grp+ip-1)
          do inum = ist, nn
            iedge = abs( ele_edge_grp%item_sf(inum) )
            inod = ie_edge_viewer(iedge,1)
            if ( inod .gt. iref ) exit
            ele_edge_grp%istack_sf(ist_grp+ip) = inum
          end do
        end do
      end do
!
      end subroutine count_nedge_ele_grp_4_domain
!
!------------------------------------------------------------------
!
      subroutine count_nedge_surf_grp_4_domain                          &
     &         (ngrp_surf_sf, sf_edge_grp)
!
      integer(kind = kint), intent(in) :: ngrp_surf_sf
      type(viewer_group_data), intent(inout)  :: sf_edge_grp
!
      integer(kind = kint) :: igrp, ip, iref, ist, inum, iedge, inod
      integer(kind = kint) :: nn, ist_grp
!
!
      do igrp = 1, ngrp_surf_sf
        ist_grp = (igrp-1)*num_pe_sf
        nn = sf_edge_grp%istack_sf(igrp*num_pe_sf)
        do ip = 1, num_pe_sf
          iref = inod_sf_stack(ip)
          ist =  sf_edge_grp%istack_sf(ist_grp+ip-1) + 1
          sf_edge_grp%istack_sf(ist_grp+ip)                             &
     &          = sf_edge_grp%istack_sf(ist_grp+ip-1)
          do inum = ist, nn
            iedge = abs( sf_edge_grp%item_sf(inum) )
            inod = ie_edge_viewer(iedge,1)
            if ( inod .gt. iref ) exit
            sf_edge_grp%istack_sf(ist_grp+ip) = inum
          end do
        end do
      end do
!
      end subroutine count_nedge_surf_grp_4_domain
!
!------------------------------------------------------------------
!
      end module count_edge_domain_4_viewer
