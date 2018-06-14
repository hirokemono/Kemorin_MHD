!>@file   renumber_para_viewer_mesh.f90
!!@brief  module renumber_para_viewer_mesh
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief Surface mesh data generator for kemoviewer
!!
!!@verbatim
!!      subroutine s_renumber_para_viewer_mesh                          &
!!     &         (surf, edge, mgd_v_mesh_p, mgd_view_mesh)
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(merged_viewer_mesh), intent(inout) :: mgd_v_mesh_p
!!        type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!!@endverbatim
!
      module renumber_para_viewer_mesh
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use t_viewer_mesh
      use t_merged_viewer_mesh
      use t_surface_data
      use t_edge_data
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_renumber_para_viewer_mesh                            &
     &         (surf, edge, mgd_v_mesh_p, mgd_view_mesh)
!
      use const_global_element_ids
!
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(merged_viewer_mesh), intent(inout) :: mgd_v_mesh_p
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
!
      call count_number_of_node_stack4(mgd_v_mesh_p%inod_sf_stack(1),   &
     &     mgd_view_mesh%inod_sf_stack)
      call count_number_of_node_stack4(mgd_v_mesh_p%isurf_sf_stack(1),  &
     &     mgd_view_mesh%isurf_sf_stack)
      call count_number_of_node_stack4(mgd_v_mesh_p%iedge_sf_stack(1),  &
     &     mgd_view_mesh%iedge_sf_stack)
!
      call num_merged_viewer_nod_surf_edge(mgd_view_mesh)
!
!
      call set_global_node_info_4_viewer                                &
     &   (my_rank, mgd_view_mesh, mgd_v_mesh_p%view_mesh)
      call set_global_surf_info_4_viewer(my_rank, surf%nnod_4_surf,     &
     &    mgd_view_mesh, mgd_v_mesh_p%view_mesh)
      call set_global_edge_info_4_viewer(my_rank, edge%nnod_4_edge,     &
     &    mgd_view_mesh, mgd_v_mesh_p%view_mesh)
!
!
      call set_global_node_grp_items                                    &
     &   (my_rank, mgd_view_mesh, mgd_v_mesh_p%domain_grps%node_grp)
      call set_global_surf_grp_items                                    &
     &   (my_rank,mgd_view_mesh, mgd_v_mesh_p%domain_grps%surf_grp)
      call set_global_edge_grp_items                                    &
     &   (my_rank,mgd_view_mesh, mgd_v_mesh_p%domain_grps%edge_grp)
!
      call set_global_node_grp_items                                    &
     &   (my_rank,mgd_view_mesh, mgd_v_mesh_p%view_nod_grps%node_grp)
!
      call set_global_node_grp_items                                    &
     &   (my_rank,mgd_view_mesh, mgd_v_mesh_p%view_ele_grps%node_grp)
      call set_global_surf_grp_items                                    &
     &   (my_rank,mgd_view_mesh, mgd_v_mesh_p%view_ele_grps%surf_grp)
      call set_global_edge_grp_items                                    &
     &   (my_rank,mgd_view_mesh, mgd_v_mesh_p%view_ele_grps%edge_grp)
!
      call set_global_node_grp_items                                    &
     &   (my_rank,mgd_view_mesh, mgd_v_mesh_p%view_sf_grps%node_grp)
      call set_global_surf_grp_items                                    &
     &   (my_rank,mgd_view_mesh, mgd_v_mesh_p%view_sf_grps%surf_grp)
      call set_global_edge_grp_items                                    &
     &   (my_rank,mgd_view_mesh, mgd_v_mesh_p%view_sf_grps%edge_grp)
!
      end subroutine s_renumber_para_viewer_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_global_node_info_4_viewer                          &
     &         (my_rank, mgd_view_mesh, view_mesh)
!
      integer(kind = kint), intent(in) :: my_rank
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: i, k
!
!
      do i = 1, view_mesh%nodpetot_viewer
        view_mesh%inod_gl_view(i)                                       &
     &          = i + mgd_view_mesh%inod_sf_stack(my_rank)
      end do
!
      end subroutine set_global_node_info_4_viewer
!
! -----------------------------------------------------------------------
!
      subroutine set_global_surf_info_4_viewer                          &
     &         (my_rank, nnod_4_surf, mgd_view_mesh, view_mesh)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nnod_4_surf
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: i, k
!
!
      do i = 1, view_mesh%surfpetot_viewer
        view_mesh%isurf_gl_view(i)                                      &
     &          = i + mgd_view_mesh%isurf_sf_stack(my_rank)
      end do
      do k = 1, nnod_4_surf
        do i = 1, view_mesh%surfpetot_viewer
          view_mesh%ie_sf_viewer(i,k)                                   &
     &          = view_mesh%ie_sf_viewer(i,k)                           &
     &           + mgd_view_mesh%inod_sf_stack(my_rank)
        end do
      end do
!
      end subroutine set_global_surf_info_4_viewer
!
! -----------------------------------------------------------------------
!
      subroutine set_global_edge_info_4_viewer                          &
     &         (my_rank, nnod_4_edge, mgd_view_mesh, view_mesh)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nnod_4_edge
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: i, k
!
!
      do i = 1, view_mesh%edgepetot_viewer
        view_mesh%iedge_gl_view(i)                                      &
     &          = i + mgd_view_mesh%iedge_sf_stack(my_rank)
      end do
      do k = 1, nnod_4_edge
        do i = 1, view_mesh%edgepetot_viewer
          view_mesh%ie_edge_viewer(i,k)                                 &
     &          = view_mesh%ie_edge_viewer(i,k)                         &
     &           + mgd_view_mesh%inod_sf_stack(my_rank)
        end do
      end do
!
!
      do k = 1, nedge_4_surf
        do i = 1, view_mesh%surfpetot_viewer
          if(view_mesh%iedge_sf_viewer(i,k) .gt. 0) then
            view_mesh%iedge_sf_viewer(i,k)                              &
     &          = view_mesh%iedge_sf_viewer(i,k)                        &
     &           + mgd_view_mesh%iedge_sf_stack(my_rank)
          else
            view_mesh%iedge_sf_viewer(i,k)                              &
     &          = view_mesh%iedge_sf_viewer(i,k)                        &
     &           - mgd_view_mesh%iedge_sf_stack(my_rank)
          end if
        end do
      end do
!
      end subroutine set_global_edge_info_4_viewer
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_global_node_grp_items                              &
     &         (my_rank, mgd_view_mesh, node_grp)
!
      integer(kind = kint), intent(in) :: my_rank
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
      type(viewer_group_data), intent(inout) :: node_grp
!
      integer(kind = kint) :: i
!
      do i = 1, node_grp%num_item
        node_grp%item_sf(i) = node_grp%item_sf(i)                       &
     &                       + mgd_view_mesh%inod_sf_stack(my_rank)
      end do
!
      end subroutine set_global_node_grp_items
!
! -----------------------------------------------------------------------
!
      subroutine set_global_surf_grp_items                              &
     &         (my_rank, mgd_view_mesh, surf_grp)
!
      integer(kind = kint), intent(in) :: my_rank
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
      type(viewer_group_data), intent(inout) :: surf_grp
!
      integer(kind = kint) :: i
!
      do i = 1, surf_grp%num_item
        if(surf_grp%item_sf(i) .gt. 0) then
          surf_grp%item_sf(i) = surf_grp%item_sf(i)                     &
     &                       + mgd_view_mesh%isurf_sf_stack(my_rank)
        else
          surf_grp%item_sf(i) = surf_grp%item_sf(i)                     &
     &                       - mgd_view_mesh%isurf_sf_stack(my_rank)
        end if
      end do
!
      end subroutine set_global_surf_grp_items
!
! -----------------------------------------------------------------------
!
      subroutine set_global_edge_grp_items                              &
     &         (my_rank, mgd_view_mesh, edge_grp)
!
      integer(kind = kint), intent(in) :: my_rank
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
      type(viewer_group_data), intent(inout) :: edge_grp
!
      integer(kind = kint) :: i
!
      do i = 1, edge_grp%num_item
        if(edge_grp%item_sf(i) .gt. 0) then
          edge_grp%item_sf(i) = edge_grp%item_sf(i)                     &
     &                         + mgd_view_mesh%iedge_sf_stack(my_rank)
        else
          edge_grp%item_sf(i) = edge_grp%item_sf(i)                     &
     &                         - mgd_view_mesh%iedge_sf_stack(my_rank)
        end if
      end do
!
      end subroutine set_global_edge_grp_items
!
! -----------------------------------------------------------------------
!
      end module renumber_para_viewer_mesh
