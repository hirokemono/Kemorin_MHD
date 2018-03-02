!
!      module set_nodes_4_groups_viewer
!
!      Written by Kemorin on Jan., 2007
!
!!      subroutine s_set_nodes_4_groups_viewer(nnod_4_surf, nnod_4_edge)
!
      module set_nodes_4_groups_viewer
!
      use m_precision
!
      use m_machine_parameter
      use const_node_list_4_viewer
!
      implicit none
!
      integer(kind = kint), allocatable :: ele_nod_item_tmp(:)
      integer(kind = kint), allocatable :: surf_nod_item_tmp(:)
!
      private :: ele_nod_item_tmp, surf_nod_item_tmp
!
      private :: set_nod_4_domain_viewer
      private :: set_nod_4_ele_group_viewer
      private :: set_nod_4_surf_group_viewer
!
      private :: allocate_ele_gp_nod_item_tmp
      private :: allocate_sf_gp_nod_item_tmp
      private :: deallocate_ele_gp_nod_item_tmp
      private :: deallocate_sf_gp_nod_item_tmp
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_set_nodes_4_groups_viewer(nnod_4_surf, nnod_4_edge)
!
      use m_surface_mesh_4_merge
!
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
!
!
      if(iflag_debug .gt. 0) write(*,*) 'allocate_imark_node_4_list'
      call allocate_imark_node_4_list(view_mesh%nodpetot_viewer)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_nod_4_domain_viewer'
      call set_nod_4_domain_viewer(nnod_4_surf, nnod_4_edge)
!
      call alloc_merged_group_item(view_ele_grps%node_grp)
      call alloc_merged_group_item(view_sf_grps%node_grp)
      if(iflag_debug .gt. 0) write(*,*) 'set_nod_4_ele_group_viewer'
      call set_nod_4_ele_group_viewer(nnod_4_surf, nnod_4_edge)
      if(iflag_debug .gt. 0) write(*,*) 'set_nod_4_surf_group_viewer'
      call set_nod_4_surf_group_viewer(nnod_4_surf, nnod_4_edge)
!
      call deallocate_imark_node_4_list
!
      end subroutine s_set_nodes_4_groups_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_nod_4_domain_viewer(nnod_4_surf, nnod_4_edge)
!
      use m_surface_mesh_4_merge
!
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
!
!
      call mark_node_4_domain_viewer(nnod_4_surf, nnod_4_edge,          &
     &    domain_grps%edge_grp, domain_grps%surf_grp)
      call count_nod_stack_4_domain_viewer(domain_grps%node_grp)
      call alloc_merged_group_item(domain_grps%node_grp)
      call const_nod_4_domain_viewer(domain_grps%node_grp)
!
      end subroutine set_nod_4_domain_viewer
!
!------------------------------------------------------------------
!
      subroutine set_nod_4_ele_group_viewer(nnod_4_surf, nnod_4_edge)
!
      use m_surface_mesh_4_merge
!
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
!
      integer(kind = kint) :: igrp, ied
!
!
      do igrp = 1, view_ele_grps%num_grp
        call mark_node_4_ele_grp_viewer                                 &
     &     (igrp, nnod_4_surf, nnod_4_edge, domain_grps%edge_grp,       &
     &      view_ele_grps%surf_grp, view_ele_grps%edge_grp)
!
        call allocate_ele_gp_nod_item_tmp(view_ele_grps%node_grp)
        ied = view_ele_grps%node_grp%num_item
        ele_nod_item_tmp(1:ied) = view_ele_grps%node_grp%item_sf(1:ied)
        call dealloc_merged_group_item(view_ele_grps%node_grp)
!
        call count_nod_stack_4_ele_gp_viewer                           &
     &     (igrp, view_ele_grps%node_grp)
!
        call alloc_merged_group_item(view_ele_grps%node_grp)
        view_ele_grps%node_grp%item_sf(1:ied) = ele_nod_item_tmp(1:ied)
        call deallocate_ele_gp_nod_item_tmp
!
        call const_nod_4_ele_gp_viewer(igrp, view_ele_grps%node_grp)
      end do
!
      end subroutine set_nod_4_ele_group_viewer
!
!------------------------------------------------------------------
!
      subroutine set_nod_4_surf_group_viewer(nnod_4_surf, nnod_4_edge)
!
      use m_surface_mesh_4_merge

      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
!
      integer(kind = kint) :: igrp, ied
!
!
      do igrp = 1, view_sf_grps%num_grp
        call mark_node_4_surf_grp_viewer                                &
     &     (igrp, nnod_4_surf, nnod_4_edge, domain_grps%edge_grp,       &
     &      view_sf_grps%surf_grp, view_sf_grps%edge_grp)
!
        call allocate_sf_gp_nod_item_tmp(view_sf_grps%node_grp)
        ied = view_sf_grps%node_grp%num_item
        surf_nod_item_tmp(1:ied) = view_sf_grps%node_grp%item_sf(1:ied)
        call dealloc_merged_group_item(view_sf_grps%node_grp)
!
        call count_nod_stack_4_sf_gp_viewer                             &
     &     (igrp, view_sf_grps%node_grp)
!
        call alloc_merged_group_item(view_sf_grps%node_grp)
        view_sf_grps%node_grp%item_sf(1:ied) = surf_nod_item_tmp(1:ied)
        call deallocate_sf_gp_nod_item_tmp
!
        call const_nod_4_sf_gp_viewer(igrp, view_sf_grps%node_grp)
      end do
!
      end subroutine set_nod_4_surf_group_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_ele_gp_nod_item_tmp(ele_nod_grp)
!
      use t_surface_mesh_4_merge
!
      type(viewer_group_data), intent(in) :: ele_nod_grp
!
      allocate( ele_nod_item_tmp(ele_nod_grp%num_item) )
      ele_nod_item_tmp = 0
!
      end subroutine allocate_ele_gp_nod_item_tmp
!
!------------------------------------------------------------------
!
      subroutine allocate_sf_gp_nod_item_tmp(sf_nod_grp)
!
      use t_surface_mesh_4_merge
!
      type(viewer_group_data), intent(in) :: sf_nod_grp
!
      allocate( surf_nod_item_tmp(sf_nod_grp%num_item) )
      surf_nod_item_tmp = 0
!
      end subroutine allocate_sf_gp_nod_item_tmp
!
!------------------------------------------------------------------
!
      subroutine deallocate_ele_gp_nod_item_tmp
!
      deallocate( ele_nod_item_tmp )
!
      end subroutine deallocate_ele_gp_nod_item_tmp
!
!------------------------------------------------------------------
!
      subroutine deallocate_sf_gp_nod_item_tmp
!
      deallocate( surf_nod_item_tmp )
!
      end subroutine deallocate_sf_gp_nod_item_tmp
!
!------------------------------------------------------------------
!
      end module set_nodes_4_groups_viewer
