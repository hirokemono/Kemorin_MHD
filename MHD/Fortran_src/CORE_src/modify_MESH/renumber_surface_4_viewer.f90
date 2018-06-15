!
!      module renumber_surface_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!
!!      subroutine set_surf_domain_stack_viewer                         &
!!     &         (num_pe, isurf_sf_stack, domain_surf_grp)
!!        type(viewer_group_data), intent(inout) :: domain_surf_grp
!!      subroutine set_element_group_stack_viewer                       &
!!     &         (num_pe, isurf_sf_stack, mgd_sf_grp,                   &
!!     &          ngrp_ele_sf, ele_surf_grp)
!!        type(group_data_merged_surf), intent(in) :: mgd_sf_grp
!!        type(viewer_group_data), intent(inout) :: ele_surf_grp
!!      subroutine set_surface_group_stack_viewer                       &
!!     &         (num_pe, isurf_sf_stack, merged_grp,                   &
!!     &          ngrp_surf_sf, sf_surf_grp)
!!        type(viewer_group_data), intent(inout)  :: sf_surf_grp
!!      subroutine set_node_group_stack_viewer                          &
!!     &         (nnod_viewer, merged_grp, ngrp_nod_sf, nod_nod_grp)
!!        type(mesh_groups), intent(in) :: merged_grp
!!        type(viewer_group_data), intent(inout) :: nod_nod_grp
!
      module renumber_surface_4_viewer
!
      use m_precision
!
      use t_viewer_mesh
      use t_viewer_group
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_surf_domain_stack_viewer                           &
     &         (num_pe, isurf_sf_stack, domain_surf_grp)
!
      integer(kind = kint), intent(in) :: num_pe
      integer(kind = kint), intent(in) :: isurf_sf_stack(0:num_pe)
      type(viewer_group_data), intent(inout) :: domain_surf_grp
!
      integer(kind = kint) :: ip, iref, ist, inum, isurf
!
!
      do ip = 1, num_pe
        iref = isurf_sf_stack(ip)
        ist = domain_surf_grp%istack_sf(ip-1) + 1
        domain_surf_grp%istack_sf(ip) = domain_surf_grp%istack_sf(ip-1)
        do inum = ist, domain_surf_grp%num_item
          isurf = abs( domain_surf_grp%item_sf(inum) )
          if ( isurf .gt. iref ) exit
          domain_surf_grp%istack_sf(ip) = inum
        end do
      end do
!
      end subroutine set_surf_domain_stack_viewer
!
!------------------------------------------------------------------
!
      subroutine set_element_group_stack_viewer                         &
     &         (num_pe, isurf_sf_stack, mgd_sf_grp,                     &
     &          ngrp_ele_sf, ele_surf_grp)
!
      use t_grp_data_merged_surfaces
!
      integer(kind = kint), intent(in) :: num_pe
      integer(kind = kint), intent(in) :: isurf_sf_stack(0:num_pe)
      type(group_data_merged_surf), intent(in) :: mgd_sf_grp
      integer(kind = kint), intent(in) :: ngrp_ele_sf
!
      type(viewer_group_data), intent(inout) :: ele_surf_grp
!
      integer(kind = kint) :: igrp, ip, idx, iref, ist, inum, isurf
!
!
      do igrp = 1, ngrp_ele_sf
        do ip = 1, num_pe
          idx = ip + (igrp-1) * num_pe
          iref = isurf_sf_stack(ip)
          ist = ele_surf_grp%istack_sf(idx-1) + 1
          ele_surf_grp%istack_sf(idx) = ele_surf_grp%istack_sf(idx-1)
          do inum = ist, mgd_sf_grp%istack_sf_iso_ele_grp_m(igrp)
            isurf = abs( ele_surf_grp%item_sf(inum) )
            if ( isurf .gt. iref ) exit
            ele_surf_grp%istack_sf(idx) = inum
          end do
        end do
      end do
!
      end subroutine set_element_group_stack_viewer
!
!------------------------------------------------------------------
!
      subroutine set_surface_group_stack_viewer                         &
     &         (num_pe, isurf_sf_stack, merged_grp,                     &
     &          ngrp_surf_sf, sf_surf_grp)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: num_pe
      integer(kind = kint), intent(in) :: isurf_sf_stack(0:num_pe)
      type(mesh_groups), intent(in) :: merged_grp
      integer(kind = kint), intent(in) :: ngrp_surf_sf
!
      type(viewer_group_data), intent(inout)  :: sf_surf_grp
!
      integer(kind = kint) :: igrp, ip, idx, iref, ist, ied
      integer(kind = kint) :: inum, isurf
!
!
      do igrp = 1, ngrp_surf_sf
        do ip = 1, num_pe
          idx = ip + (igrp-1) * num_pe
          iref = isurf_sf_stack(ip)
          ist = sf_surf_grp%istack_sf(idx-1) + 1
          ied = merged_grp%surf_grp%istack_grp(igrp)
!
          sf_surf_grp%istack_sf(idx) = sf_surf_grp%istack_sf(idx-1)
          do inum = ist, ied
            isurf = abs( sf_surf_grp%item_sf(inum) )
            if ( isurf .gt. iref ) exit
            sf_surf_grp%istack_sf(idx) = inum
          end do
        end do
      end do
!
      end subroutine set_surface_group_stack_viewer
!
!------------------------------------------------------------------
!
      subroutine set_node_group_stack_viewer                            &
     &         (nnod_viewer, merged_grp, ngrp_nod_sf, nod_nod_grp)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: nnod_viewer
      type(mesh_groups), intent(in) :: merged_grp
      integer(kind = kint), intent(in) :: ngrp_nod_sf
      type(viewer_group_data), intent(inout) :: nod_nod_grp
!
      integer(kind = kint) :: igrp, ist, ied, inum, inod
!
!
      do igrp = 1, ngrp_nod_sf
        ist = nod_nod_grp%istack_sf(igrp-1) + 1
        ied = merged_grp%nod_grp%istack_grp(igrp)
!
        nod_nod_grp%istack_sf(igrp) = nod_nod_grp%istack_sf(igrp-1)
        do inum = ist, ied
          inod = abs( nod_nod_grp%item_sf(inum) )
          if ( inod .gt. nnod_viewer ) exit
          nod_nod_grp%istack_sf(igrp:ngrp_nod_sf) = inum
        end do
      end do
!
      end subroutine set_node_group_stack_viewer
!
!------------------------------------------------------------------
!
      end module renumber_surface_4_viewer
