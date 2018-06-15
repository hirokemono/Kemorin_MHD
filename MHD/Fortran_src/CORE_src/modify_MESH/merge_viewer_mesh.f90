!>@file   merge_viewer_mesh.f90
!!@brief  module merge_viewer_mesh
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief Surface mesh data generator for kemoviewer
!!
!!@verbatim
!!      subroutine s_merge_viewer_mesh(nprocs, vmesh, domain_grps_p,    &
!!     &          view_nod_grps_p, view_ele_grps_p, view_sf_grps_p,     &
!!     &          mgd_vmesh)
!!        type(viewer_mesh_data), intent(in) :: vmesh(nprocs)
!!        type(viewer_surface_groups), intent(inout)                    &
!!       &                             :: domain_grps_p(nprocs)
!!        type(viewer_node_groups), intent(inout)                       &
!!       &                             :: view_nod_grps_p(nprocs)
!!        type(viewer_surface_groups), intent(inout)                    &
!!       &                             :: view_ele_grps_p(nprocs)
!!        type(viewer_surface_groups), intent(inout)                    &
!!       &                             :: view_sf_grps_p(nprocs)
!!@endverbatim
!
      module merge_viewer_mesh
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use t_merged_viewer_mesh
      use t_viewer_mesh
      use t_viewer_group
!
      implicit none
!
      private :: count_merged_viewer_node, copy_2_merged_viewer_node
      private :: copy_2_merged_viewer_surf, copy_2_merged_viewer_edge
      private :: set_global_node_group_items, set_global_groups_items
      private :: merged_viewer_nod_groups, merged_viewer_groups
      private :: merge_viewer_group_stack, merge_viewer_group_item
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_merge_viewer_mesh(nprocs, vmesh, domain_grps_p,      &
     &          view_nod_grps_p, view_ele_grps_p, view_sf_grps_p,       &
     &          mgd_vmesh)
!
      integer(kind = kint), intent(in) :: nprocs
      type(viewer_mesh_data), intent(in) :: vmesh(nprocs)
!
      type(viewer_surface_groups), intent(inout)                        &
     &                             :: domain_grps_p(nprocs)
      type(viewer_node_groups), intent(inout)                           &
     &                             :: view_nod_grps_p(nprocs)
      type(viewer_surface_groups), intent(inout)                        &
     &                             :: view_ele_grps_p(nprocs)
      type(viewer_surface_groups), intent(inout)                        &
     &                             :: view_sf_grps_p(nprocs)
!
      type(merged_viewer_mesh), intent(inout) :: mgd_vmesh
!
!
      call count_merged_viewer_node(nprocs, vmesh, mgd_vmesh)
!
      call alloc_nod_position_viewer(mgd_vmesh%view_mesh)
      call copy_2_merged_viewer_node(nprocs, vmesh, mgd_vmesh)
!
      call alloc_surf_type_viewer(mgd_vmesh%view_mesh)
      call alloc_surf_connect_viewer                                    &
     &   (vmesh(1)%nnod_v_surf, mgd_vmesh%view_mesh)
      call copy_2_merged_viewer_surf(nprocs, vmesh, mgd_vmesh)
!
      call alloc_edge_data_4_sf                                         &
     &   (vmesh(1)%nnod_v_edge, mgd_vmesh%view_mesh)
      call copy_2_merged_viewer_edge(nprocs, vmesh, mgd_vmesh)
!
      call set_global_groups_items                                      &
     &   (nprocs, mgd_vmesh, domain_grps_p)
!
      call set_global_node_group_items                                  &
     &         (nprocs, mgd_vmesh, view_nod_grps_p)
      call set_global_groups_items                                      &
     &         (nprocs, mgd_vmesh, view_ele_grps_p)
      call set_global_groups_items                                      &
     &         (nprocs, mgd_vmesh, view_sf_grps_p)
!
      call merged_viewer_groups                                         &
     &   (nprocs, domain_grps_p, mgd_vmesh%domain_grps)
!
      call merged_viewer_nod_groups                                     &
     &   (nprocs, view_nod_grps_p, mgd_vmesh%view_nod_grps)
      call merged_viewer_groups                                         &
     &   (nprocs, view_ele_grps_p, mgd_vmesh%view_ele_grps)
      call merged_viewer_groups                                         &
     &   (nprocs, view_sf_grps_p, mgd_vmesh%view_sf_grps)
!
      end subroutine s_merge_viewer_mesh
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_merged_viewer_node                               &
     &         (nprocs, sgl_vmesh, mgd_vmesh)
!
      integer(kind = kint), intent(in) :: nprocs
      type(viewer_mesh_data), intent(in) :: sgl_vmesh(nprocs)
!
      type(merged_viewer_mesh), intent(inout) :: mgd_vmesh
!
      integer(kind= kint) :: ip
!
!
      do ip = 1, nprocs
        mgd_vmesh%inod_sf_stack(ip)                                     &
     &    = mgd_vmesh%inod_sf_stack(ip-1) + sgl_vmesh(ip)%nnod_viewer
        mgd_vmesh%isurf_sf_stack(ip)                                    &
     &    = mgd_vmesh%isurf_sf_stack(ip-1) + sgl_vmesh(ip)%nsurf_viewer
        mgd_vmesh%iedge_sf_stack(ip)                                    &
     &    = mgd_vmesh%iedge_sf_stack(ip-1) + sgl_vmesh(ip)%nedge_viewer
      end do
      mgd_vmesh%view_mesh%nnod_viewer                                   &
     &    = mgd_vmesh%inod_sf_stack(nprocs)
      mgd_vmesh%view_mesh%nsurf_viewer                                  &
     &    = mgd_vmesh%isurf_sf_stack(nprocs)
      mgd_vmesh%view_mesh%nedge_viewer                                  &
     &    = mgd_vmesh%iedge_sf_stack(nprocs)
!
      end subroutine count_merged_viewer_node
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_2_merged_viewer_node                              &
     &         (nprocs, sgl_vmesh, mgd_vmesh)
!
      integer(kind = kint), intent(in) :: nprocs
      type(viewer_mesh_data), intent(in) :: sgl_vmesh(nprocs)
!
      type(merged_viewer_mesh), intent(inout) :: mgd_vmesh
!
      integer(kind= kint) :: ip, ist, num, inum
!
!
!$omp parallel
      do ip = 1, nprocs
        ist = mgd_vmesh%inod_sf_stack(ip-1)
        num = mgd_vmesh%inod_sf_stack(ip) - ist
!$omp do private(inum)
        do inum = 1, num
          mgd_vmesh%view_mesh%inod_gl_view(ist+inum) = ist + inum
          mgd_vmesh%view_mesh%xx_view(ist+inum,1)                       &
     &            = sgl_vmesh(ip)%xx_view(inum,1)
          mgd_vmesh%view_mesh%xx_view(ist+inum,2)                       &
     &            = sgl_vmesh(ip)%xx_view(inum,2)
          mgd_vmesh%view_mesh%xx_view(ist+inum,3)                       &
     &            = sgl_vmesh(ip)%xx_view(inum,3)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_2_merged_viewer_node
!
!------------------------------------------------------------------
!
      subroutine copy_2_merged_viewer_surf                              &
     &         (nprocs, sgl_vmesh, mgd_vmesh)
!
      integer(kind = kint), intent(in) :: nprocs
      type(viewer_mesh_data), intent(in) :: sgl_vmesh(nprocs)
!
      type(merged_viewer_mesh), intent(inout) :: mgd_vmesh
!
      integer(kind= kint) :: ip, ist, num, inum, k1
!
!
!$omp parallel
      do ip = 1, nprocs
        ist = mgd_vmesh%isurf_sf_stack(ip-1)
        num = mgd_vmesh%isurf_sf_stack(ip) - ist
!$omp do private(inum)
        do inum = 1, num
          mgd_vmesh%view_mesh%isurf_gl_view(ist+inum) = ist + inum
          mgd_vmesh%view_mesh%surftyp_viewer(ist+inum)                  &
     &            = sgl_vmesh(ip)%surftyp_viewer(inum)
          do k1 = 1, mgd_vmesh%view_mesh%nnod_v_surf
            mgd_vmesh%view_mesh%ie_sf_viewer(ist+inum,k1)               &
     &          = sgl_vmesh(ip)%ie_sf_viewer(inum,k1)                   &
     &           + mgd_vmesh%inod_sf_stack(ip-1)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_2_merged_viewer_surf
!
!------------------------------------------------------------------
!
      subroutine copy_2_merged_viewer_edge                              &
     &         (nprocs, sgl_vmesh, mgd_vmesh)
!
      integer(kind = kint), intent(in) :: nprocs
      type(viewer_mesh_data), intent(in) :: sgl_vmesh(nprocs)
!
      type(merged_viewer_mesh), intent(inout) :: mgd_vmesh
!
      integer(kind= kint) :: ip, ist, num, inum, k1
!
!$omp parallel
      do ip = 1, nprocs
        ist = mgd_vmesh%iedge_sf_stack(ip-1)
        num = mgd_vmesh%iedge_sf_stack(ip) - ist
!$omp do private(inum)
        do inum = 1, num
          mgd_vmesh%view_mesh%iedge_gl_view(ist+inum) = ist + inum
          do k1 = 1, mgd_vmesh%view_mesh%nnod_v_edge
            mgd_vmesh%view_mesh%ie_edge_viewer(ist+inum,k1)             &
     &          = sgl_vmesh(ip)%ie_edge_viewer(inum,k1)                 &
     &           + mgd_vmesh%inod_sf_stack(ip-1)
          end do
        end do
!$omp end do nowait
!
        ist = mgd_vmesh%isurf_sf_stack(ip-1)
        num = mgd_vmesh%isurf_sf_stack(ip) - ist
!$omp do private(inum)
        do inum = 1, num
          do k1 = 1, nedge_4_surf
            mgd_vmesh%view_mesh%iedge_sf_viewer(ist+inum,k1)            &
     &          = sgl_vmesh(ip)%iedge_sf_viewer(inum,k1)                &
     &           + sign(mgd_vmesh%iedge_sf_stack(ip-1),                 &
     &                  sgl_vmesh(ip)%iedge_sf_viewer(inum,k1))
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_2_merged_viewer_edge
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_global_node_group_items                            &
     &         (nprocs, mgd_view_mesh, sgl_grps)
!
      use renumber_para_viewer_mesh
!
      integer(kind = kint), intent(in) :: nprocs
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
      type(viewer_node_groups), intent(inout) :: sgl_grps(nprocs)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, nprocs
        call set_global_group_items                                     &
     &     (mgd_view_mesh%inod_sf_stack(ip-1), sgl_grps(ip)%node_grp)
      end do
!
      end subroutine set_global_node_group_items
!
!------------------------------------------------------------------
!
      subroutine set_global_groups_items                                &
     &         (nprocs, mgd_vmesh, sgl_grps)
!
      use renumber_para_viewer_mesh
!
      integer(kind = kint), intent(in) :: nprocs
      type(merged_viewer_mesh), intent(in) :: mgd_vmesh
      type(viewer_surface_groups), intent(inout) :: sgl_grps(nprocs)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, nprocs
        call shift_global_surf_grps_items                               &
     &     (mgd_vmesh%inod_sf_stack(ip-1),                              &
     &      mgd_vmesh%isurf_sf_stack(ip-1),                             &
     &      mgd_vmesh%iedge_sf_stack(ip-1),                             &
     &      sgl_grps(ip))
      end do
!
      end subroutine set_global_groups_items
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine merged_viewer_nod_groups(nprocs, sgl_grps, mgd_grps)
!
      integer(kind = kint), intent(in) :: nprocs
      type(viewer_node_groups), intent(in) :: sgl_grps(nprocs)
      type(viewer_node_groups), intent(inout) :: mgd_grps
!
      integer(kind = kint) :: igrp, i, ip
!
!
      mgd_grps%num_grp = sgl_grps(1)%num_grp
      call alloc_merged_node_grps_stack(nprocs, mgd_grps)
!
      do igrp = 1, mgd_grps%num_grp
        mgd_grps%grp_name(igrp) = sgl_grps(1)%grp_name(igrp)
      end do
!
      do ip = 1, nprocs
        call merge_viewer_group_stack(ip, nprocs, sgl_grps(ip)%num_grp, &
     &      sgl_grps(ip)%node_grp, mgd_grps%node_grp)
      end do
      do i = 1, nprocs*mgd_grps%num_grp
        mgd_grps%node_grp%istack_sf(i) = mgd_grps%node_grp%istack_sf(i) &
     &                               + mgd_grps%node_grp%istack_sf(i-1)
      end do
      mgd_grps%node_grp%num_item                                        &
     &     = mgd_grps%node_grp%istack_sf(nprocs*mgd_grps%num_grp)
!
      call alloc_merged_group_item(mgd_grps%node_grp)
!
      do ip = 1, nprocs
        call merge_viewer_group_item(ip, nprocs, sgl_grps(ip)%num_grp,  &
     &      sgl_grps(ip)%node_grp, mgd_grps%node_grp)
      end do
!
      end subroutine merged_viewer_nod_groups
!
!------------------------------------------------------------------
!
      subroutine merged_viewer_groups(nprocs, sgl_grps, mgd_grps)
!
      integer(kind = kint), intent(in) :: nprocs
      type(viewer_surface_groups), intent(in) :: sgl_grps(nprocs)
      type(viewer_surface_groups), intent(inout) :: mgd_grps
!
      integer(kind = kint) :: igrp, i, ip
!
!
      mgd_grps%num_grp = sgl_grps(1)%num_grp
      call alloc_merged_surf_grps_stack(nprocs, mgd_grps)
!
      do igrp = 1, mgd_grps%num_grp
        mgd_grps%grp_name(igrp) = sgl_grps(1)%grp_name(igrp)
      end do
!
      do ip = 1, nprocs
        call merge_viewer_group_stack                                   &
     &     (ip, nprocs, sgl_grps(ip)%num_grp, sgl_grps(ip)%node_grp,    &
     &      mgd_grps%node_grp)
        call merge_viewer_group_stack                                   &
     &     (ip, nprocs, sgl_grps(ip)%num_grp, sgl_grps(ip)%surf_grp,    &
     &      mgd_grps%surf_grp)
        call merge_viewer_group_stack                                   &
     &     (ip, nprocs, sgl_grps(ip)%num_grp, sgl_grps(ip)%edge_grp,    &
     &      mgd_grps%edge_grp)
      end do
      do i = 1, nprocs*mgd_grps%num_grp
        mgd_grps%node_grp%istack_sf(i) = mgd_grps%node_grp%istack_sf(i) &
     &                               + mgd_grps%node_grp%istack_sf(i-1)
        mgd_grps%surf_grp%istack_sf(i) = mgd_grps%surf_grp%istack_sf(i) &
     &                               + mgd_grps%surf_grp%istack_sf(i-1)
        mgd_grps%edge_grp%istack_sf(i) = mgd_grps%edge_grp%istack_sf(i) &
     &                               + mgd_grps%edge_grp%istack_sf(i-1)
      end do
      mgd_grps%node_grp%num_item                                        &
     &     = mgd_grps%node_grp%istack_sf(nprocs*mgd_grps%num_grp)
      mgd_grps%surf_grp%num_item                                        &
     &     = mgd_grps%surf_grp%istack_sf(nprocs*mgd_grps%num_grp)
      mgd_grps%edge_grp%num_item                                        &
     &     = mgd_grps%edge_grp%istack_sf(nprocs*mgd_grps%num_grp)
!
      call alloc_merged_group_item(mgd_grps%node_grp)
      call alloc_merged_group_item(mgd_grps%surf_grp)
      call alloc_merged_group_item(mgd_grps%edge_grp)
!
      do ip = 1, nprocs
        call merge_viewer_group_item(ip, nprocs, sgl_grps(ip)%num_grp,  &
     &      sgl_grps(ip)%node_grp, mgd_grps%node_grp)
        call merge_viewer_group_item(ip, nprocs, sgl_grps(ip)%num_grp,  &
     &      sgl_grps(ip)%surf_grp, mgd_grps%surf_grp)
        call merge_viewer_group_item(ip, nprocs, sgl_grps(ip)%num_grp,  &
     &      sgl_grps(ip)%edge_grp, mgd_grps%edge_grp)
      end do
!
      end subroutine merged_viewer_groups
!
!------------------------------------------------------------------
!
      subroutine merge_viewer_group_stack                               &
     &         (ip, nprocs, num_grp, sgl_grp, mgd_grp)
!
      integer(kind = kint), intent(in) :: ip, nprocs
      integer(kind = kint), intent(in) :: num_grp
      type(viewer_group_data), intent(in) :: sgl_grp
!
      type(viewer_group_data), intent(inout) :: mgd_grp
!
      integer(kind = kint) :: igrp, igrp_m
!
!
      do igrp = 1, num_grp
        igrp_m = ip + (igrp-1) * nprocs
        mgd_grp%istack_sf(igrp_m)                                       &
     &       = sgl_grp%istack_sf(igrp) - sgl_grp%istack_sf(igrp-1)
      end do
!
      end subroutine merge_viewer_group_stack
!
!------------------------------------------------------------------
!
      subroutine merge_viewer_group_item                                &
     &          (ip, nprocs, num_grp, sgl_grp, mgd_grp)
!
      integer(kind = kint), intent(in) :: ip, nprocs
      integer(kind = kint), intent(in) :: num_grp
      type(viewer_group_data), intent(in) :: sgl_grp
!
      type(viewer_group_data), intent(inout) :: mgd_grp
!
      integer(kind = kint) :: igrp, ist, num, igrp_m, ist_m, inum
!
!
      do igrp = 1, num_grp
        ist = sgl_grp%istack_sf(igrp-1)
        num = sgl_grp%istack_sf(igrp) - sgl_grp%istack_sf(igrp-1)
        igrp_m = ip + (igrp-1) * nprocs
        ist_m = mgd_grp%istack_sf(igrp_m-1)
        do inum = 1, num
          mgd_grp%item_sf(inum+ist_m) = sgl_grp%item_sf(inum+ist)
        end do
      end do
!
      end subroutine merge_viewer_group_item
!
!------------------------------------------------------------------
!
      end module merge_viewer_mesh
