!const_refined_group.f90
!      module const_refined_group
!
!      Writen by H. Matsui on Oct., 2007
!
!      subroutine s_const_refined_group(newmesh, newgroup)
!
      module const_refined_group
!
      use m_precision
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
      subroutine s_const_refined_group(newmesh, newgroup)
!
      use m_group_data
      use t_mesh_data
!
      type(mesh_geometry), intent(inout) :: newmesh
      type(mesh_groups), intent(inout) :: newgroup
!
!
      call const_refined_node_group(nod_grp1, newgroup%nod_grp)
!
      call const_refined_ele_group(ele_grp1, newgroup%ele_grp)
!
      call const_refined_surf_group                                     &
     &   (sf_grp1, newmesh%node%numnod, newgroup%surf_grp)
!
      end subroutine s_const_refined_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_refined_node_group(nod_grp, new_nod_grp)
!
      use set_refined_node_group
      use find_hanging_surface
      use t_group_data
!
      type(group_data), intent(in) :: nod_grp
      type(group_data), intent(inout) :: new_nod_grp
!
!
      call allocate_mark_refine_nod_grp
!
      new_nod_grp%num_grp = nod_grp%num_grp
      call add_hanging_node_group_num(new_nod_grp)
      call allocate_grp_type_num(new_nod_grp)
!
      call count_refined_node_group(nod_grp, new_nod_grp)
      call add_hanging_node_group_name(nod_grp%num_grp, new_nod_grp)
      call allocate_grp_type_item(new_nod_grp)
!
      call s_set_refined_node_group(nod_grp, new_nod_grp)
      call add_hanging_node_group_item(nod_grp%num_grp, new_nod_grp)
      call deallocate_mark_refine_nod_grp
!
      end subroutine const_refined_node_group
!
!  ---------------------------------------------------------------------
!
      subroutine const_refined_ele_group(ele_grp, new_ele_grp)
!
      use t_group_data
      use set_refined_ele_group
!
      type(group_data), intent(in) :: ele_grp
      type(group_data), intent(inout) :: new_ele_grp
!
!
      new_ele_grp%num_grp = ele_grp%num_grp
      call allocate_grp_type_num(new_ele_grp)
!
      call count_refined_ele_group(ele_grp, new_ele_grp)
      call allocate_grp_type_item(new_ele_grp)
!
      call s_set_refined_ele_group(ele_grp, new_ele_grp)
!
      end subroutine const_refined_ele_group
!
!  ---------------------------------------------------------------------
!
      subroutine const_refined_surf_group                               &
     &         (sf_grp, nnod_2nd, new_sf_grp)
!
      use m_geometry_parameter
      use t_group_data
      use set_refined_surf_group
!
      integer(kind = kint), intent(in) :: nnod_2nd
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_data), intent(inout) :: new_sf_grp
!
!
      call allocate_mark_refine_sf_grp(nnod_2nd)
!
      new_sf_grp%num_grp = sf_grp%num_grp
      call allocate_sf_grp_type_num(new_sf_grp)
!
      call count_refined_surf_group                                     &
     &   (nnod_4_surf, node_on_sf, sf_grp, new_sf_grp)
      call allocate_sf_grp_type_item(new_sf_grp)
!
      call s_set_refined_surf_group                                     &
     &   (nnod_4_surf, node_on_sf, sf_grp, new_sf_grp)
      call deallocate_mark_refine_sf_grp
!
      end subroutine const_refined_surf_group
!
!  ---------------------------------------------------------------------
!
      end module const_refined_group
