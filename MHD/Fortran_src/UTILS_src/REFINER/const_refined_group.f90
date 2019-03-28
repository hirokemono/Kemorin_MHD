!const_refined_group.f90
!      module const_refined_group
!
!      Writen by H. Matsui on Oct., 2007
!
!!      subroutine s_const_refined_group(org_mesh, org_e_mesh, org_grp, &
!!     &          ref_ids, refine_tbl, newmesh, newgroup)
!!        type(refined_node_id), intent(in) :: ref_ids
!!        type(element_refine_table), intent(in) :: refine_tbl
!!        type(mesh_geometry), intent(in) :: org_mesh
!!        type(element_geometry), intent(in) :: org_e_mesh
!!        type(mesh_groups), intent(inout) :: org_grp
!!        type(mesh_geometry), intent(inout) :: newmesh
!!        type(mesh_groups), intent(inout) :: newgroup
!
      module const_refined_group
!
      use m_precision
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_refined_node_id
      use t_refined_element_data
!
      implicit none
!
      private :: const_refined_node_group, const_refined_ele_group
      private :: const_refined_surf_group
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_const_refined_group(org_mesh, org_e_mesh, org_grp,   &
     &          ref_ids, refine_tbl, newmesh, newgroup)
!
      type(refined_node_id), intent(in) :: ref_ids
      type(element_refine_table), intent(in) :: refine_tbl
      type(mesh_geometry), intent(in) :: org_mesh
      type(element_geometry), intent(in) :: org_e_mesh
      type(mesh_groups), intent(inout) :: org_grp
!
      type(mesh_geometry), intent(inout) :: newmesh
      type(mesh_groups), intent(inout) :: newgroup
!
!
      call const_refined_node_group                                     &
     &   (org_mesh%node, org_mesh%ele, org_e_mesh%surf,                 &
     &    org_e_mesh%edge, org_grp%nod_grp, ref_ids, newgroup%nod_grp)
!
      call const_refined_ele_group                                      &
     &   (org_grp%ele_grp, refine_tbl, newgroup%ele_grp)
!
      call const_refined_surf_group                                     &
     &   (org_e_mesh%surf, org_e_mesh%edge, org_grp%surf_grp,           &
     &    ref_ids, refine_tbl, newmesh%node%numnod, newgroup%surf_grp)
!
      end subroutine s_const_refined_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_refined_node_group                               &
     &         (node, ele, surf, edge, nod_grp, ref_ids, new_nod_grp)
!
      use set_refined_node_group
      use find_hanging_surface
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(group_data), intent(in) :: nod_grp
      type(refined_node_id), intent(in) :: ref_ids
!
      type(group_data), intent(inout) :: new_nod_grp
!
!
      call allocate_mark_refine_nod_grp(node%numnod)
!
      new_nod_grp%num_grp = nod_grp%num_grp
      call add_hanging_node_group_num(new_nod_grp)
      call allocate_grp_type_num(new_nod_grp)
!
      call count_refined_node_group(node, ele, surf, edge, nod_grp,     &
     &    ref_ids%refine_ele, ref_ids%refine_surf, ref_ids%refine_edge, &
     &    new_nod_grp)
      call add_hanging_node_group_name(nod_grp%num_grp, new_nod_grp)
      call allocate_grp_type_item(new_nod_grp)
!
      call s_set_refined_node_group(node, ele, surf, edge, nod_grp,     &
     &    ref_ids%refine_ele, ref_ids%refine_surf, ref_ids%refine_edge, &
     &    new_nod_grp)
      call add_hanging_node_group_item(nod_grp%num_grp, new_nod_grp)
      call deallocate_mark_refine_nod_grp
!
      end subroutine const_refined_node_group
!
!  ---------------------------------------------------------------------
!
      subroutine const_refined_ele_group                                &
     &         (ele_grp, refine_tbl, new_ele_grp)
!
      use set_refined_ele_group
!
      type(group_data), intent(in) :: ele_grp
      type(element_refine_table), intent(in) :: refine_tbl
!
      type(group_data), intent(inout) :: new_ele_grp
!
!
      new_ele_grp%num_grp = ele_grp%num_grp
      call allocate_grp_type_num(new_ele_grp)
!
      call count_refined_ele_group(refine_tbl, ele_grp, new_ele_grp)
      call allocate_grp_type_item(new_ele_grp)
!
      call s_set_refined_ele_group(refine_tbl, ele_grp, new_ele_grp)
!
      end subroutine const_refined_ele_group
!
!  ---------------------------------------------------------------------
!
      subroutine const_refined_surf_group(surf, edge, sf_grp,           &
     &          ref_ids, refine_tbl, nnod_2nd, new_sf_grp)
!
      use set_refined_surf_group
!
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      integer(kind = kint), intent(in) :: nnod_2nd
      type(surface_group_data), intent(in) :: sf_grp
      type(refined_node_id), intent(in) :: ref_ids
      type(element_refine_table), intent(in) :: refine_tbl
!
      type(surface_group_data), intent(inout) :: new_sf_grp
!
!
      call allocate_mark_refine_sf_grp(nnod_2nd)
!
      new_sf_grp%num_grp = sf_grp%num_grp
      call allocate_sf_grp_type_num(new_sf_grp)
!
      call count_refined_surf_group                                     &
     &   (surf, edge, sf_grp, ref_ids, refine_tbl, new_sf_grp)
      call allocate_sf_grp_type_item(new_sf_grp)
!
      call s_set_refined_surf_group                                     &
     &   (surf, edge, sf_grp, ref_ids, refine_tbl, new_sf_grp)
      call deallocate_mark_refine_sf_grp
!
      end subroutine const_refined_surf_group
!
!  ---------------------------------------------------------------------
!
      end module const_refined_group
