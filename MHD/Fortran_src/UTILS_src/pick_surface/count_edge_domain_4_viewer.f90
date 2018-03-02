!
!      module count_edge_domain_4_viewer
!
!     Written by H. Matsui on Jan., 2007
!
!!      subroutine count_nedge_4_each_domain                            &
!!     &         (num_pe, inod_sf_stack, iedge_sf_stack, view_mesh)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!      subroutine count_nedge_domain_4_domain                          &
!!     &        s (num_pe, inod_sf_stack, view_mesh, domain_edge_grp)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!        type(viewer_group_data), intent(inout) :: domain_edge_grp
!!      subroutine count_nedge_ele_grp_4_domain                         &
!!     &         (num_pe, inod_sf_stack, view_mesh,                     &
!!     &          ngrp_ele_sf, ele_edge_grp)
!!     &         (num_pe, view_mesh, ngrp_ele_sf, ele_edge_grp)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!        type(viewer_group_data), intent(inout) :: ele_edge_grp
!!      subroutine count_nedge_surf_grp_4_domain                        &
!!     &         (num_pe, inod_sf_stack, view_mesh,                     &
!!     &          ngrp_surf_sf, sf_edge_grp)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!        type(viewer_group_data), intent(inout)  :: sf_edge_grp
!
      module count_edge_domain_4_viewer
!
      use m_precision
      use t_surface_mesh_4_merge
!
      implicit    none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine count_nedge_4_each_domain                              &
     &         (num_pe, inod_sf_stack, iedge_sf_stack, view_mesh)
!
      type(viewer_mesh_data), intent(in) :: view_mesh
!
      integer(kind = kint), intent(in) :: num_pe
      integer(kind = kint), intent(in) :: inod_sf_stack(0:num_pe)
      integer(kind = kint), intent(inout) :: iedge_sf_stack(0:num_pe)
!
      integer(kind = kint) :: ip, iref, ist, iedge, inod
!
!
      do ip = 1, num_pe
        iref = inod_sf_stack(ip)
        ist =  iedge_sf_stack(ip-1) + 1
        do iedge = ist, view_mesh%edgepetot_viewer
          inod = view_mesh%ie_edge_viewer(iedge,1)
          if ( inod .gt. iref ) exit
          iedge_sf_stack(ip:num_pe) = iedge
        end do
      end do
!
      end subroutine count_nedge_4_each_domain
!
!------------------------------------------------------------------
!
      subroutine count_nedge_domain_4_domain                            &
     &         (num_pe, inod_sf_stack, view_mesh, domain_edge_grp)
!
      type(viewer_mesh_data), intent(in) :: view_mesh
      integer(kind = kint), intent(in) :: num_pe
      integer(kind = kint), intent(in) :: inod_sf_stack(0:num_pe)
      type(viewer_group_data), intent(inout) :: domain_edge_grp
!
      integer(kind = kint) :: ip, iref, ist, inum, iedge, inod
!
!
      do ip = 1, num_pe
        iref = inod_sf_stack(ip)
        ist =  domain_edge_grp%istack_sf(ip-1) + 1
        do inum = ist, domain_edge_grp%num_item
          iedge = abs(domain_edge_grp%item_sf(inum) )
          inod = view_mesh%ie_edge_viewer(iedge,1)
          if ( inod .gt. iref ) exit
          domain_edge_grp%istack_sf(ip:num_pe) = inum
        end do
      end do
!
      end subroutine count_nedge_domain_4_domain
!
!------------------------------------------------------------------
!
      subroutine count_nedge_ele_grp_4_domain                           &
     &         (num_pe, inod_sf_stack, view_mesh,                       &
     &          ngrp_ele_sf, ele_edge_grp)
!
      type(viewer_mesh_data), intent(in) :: view_mesh
      integer(kind = kint), intent(in) :: num_pe
      integer(kind = kint), intent(in) :: inod_sf_stack(0:num_pe)
      integer(kind = kint), intent(in) :: ngrp_ele_sf
!
      type(viewer_group_data), intent(inout) :: ele_edge_grp
!
      integer(kind = kint) :: igrp, ip, iref, ist, inum, iedge, inod
      integer(kind = kint) :: nn, ist_grp
!
!
      do igrp = 1, ngrp_ele_sf
        ist_grp = (igrp-1)*num_pe
        nn = ele_edge_grp%istack_sf(igrp*num_pe)
        do ip = 1, num_pe
          iref = inod_sf_stack(ip)
          ist =  ele_edge_grp%istack_sf(ist_grp+ip-1) + 1
          ele_edge_grp%istack_sf(ist_grp+ip)                            &
     &          = ele_edge_grp%istack_sf(ist_grp+ip-1)
          do inum = ist, nn
            iedge = abs( ele_edge_grp%item_sf(inum) )
            inod = view_mesh%ie_edge_viewer(iedge,1)
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
     &         (num_pe, inod_sf_stack, view_mesh,                       &
     &          ngrp_surf_sf, sf_edge_grp)
!
      type(viewer_mesh_data), intent(in) :: view_mesh
      integer(kind = kint), intent(in) :: num_pe
      integer(kind = kint), intent(in) :: inod_sf_stack(0:num_pe)
      integer(kind = kint), intent(in) :: ngrp_surf_sf
!
      type(viewer_group_data), intent(inout)  :: sf_edge_grp
!
      integer(kind = kint) :: igrp, ip, iref, ist, inum, iedge, inod
      integer(kind = kint) :: nn, ist_grp
!
!
      do igrp = 1, ngrp_surf_sf
        ist_grp = (igrp-1)*num_pe
        nn = sf_edge_grp%istack_sf(igrp*num_pe)
        do ip = 1, num_pe
          iref = inod_sf_stack(ip)
          ist =  sf_edge_grp%istack_sf(ist_grp+ip-1) + 1
          sf_edge_grp%istack_sf(ist_grp+ip)                             &
     &          = sf_edge_grp%istack_sf(ist_grp+ip-1)
          do inum = ist, nn
            iedge = abs( sf_edge_grp%item_sf(inum) )
            inod = view_mesh%ie_edge_viewer(iedge,1)
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
