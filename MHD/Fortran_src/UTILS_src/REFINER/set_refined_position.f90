!set_refined_position.f90
!      module set_refined_position
!
!!      subroutine s_set_refined_position(node, ele, surf, edge,        &
!!     &          refine_ele, refine_surf, refine_edge)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(table_4_refine), intent(inout) :: refine_ele
!!        type(table_4_refine), intent(inout) :: refine_surf, refine_edge
!
!     Written by H. Matsui on Oct., 2007
!
      module set_refined_position
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_refined_position(node, ele, surf, edge,          &
     &          refine_ele, refine_surf, refine_edge)
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_refined_node_id
!
      use m_control_param_4_refiner
!
      use cal_xyz_4_refine
      use cal_sph_4_refine
      use cal_r_4_refine
      use set_refined_nod_2_sphere
      use cal_refined_nod_near_pole
      use coordinate_converter
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(table_4_refine), intent(inout) :: refine_ele
      type(table_4_refine), intent(inout) :: refine_surf, refine_edge
!
!
      if (iflag_interpolate_type .eq. 1) then
        call cal_sph_on_edge_4_refine(node%numnod, edge%numedge,        &
     &      edge%ie_edge, node%rr, node%theta, node%phi,                &
     &      refine_edge%ntot_nod_refine, refine_edge%istack_nod_refine, &
     &      refine_edge%xi_refine, refine_edge%sph_refine)
        call cal_sph_on_surf_4_refine(node%numnod, surf%numsurf,        &
     &      surf%ie_surf, node%rr, node%theta, node%phi,                &
     &      refine_surf%ntot_nod_refine, refine_surf%istack_nod_refine, &
     &      refine_surf%xi_refine, refine_surf%sph_refine)
        call cal_sph_on_ele_4_refine(node%numnod, ele%numele,           &
     &      ele%ie, node%rr, node%theta, node%phi,                      &
     &      refine_ele%ntot_nod_refine, refine_ele%istack_nod_refine,   &
     &      refine_ele%xi_refine, refine_ele%sph_refine)
!
        call s_cal_refined_nod_near_pole(node, surf, edge,              &
     &      refine_surf, refine_edge)
!
        call position_2_xyz                                             &
     &     (refine_ele%ntot_nod_refine, refine_ele%sph_refine(1,1),     &
     &      refine_ele%sph_refine(1,2), refine_ele%sph_refine(1,3),     &
     &      refine_ele%x_refine(1,1), refine_ele%x_refine(1,2),         &
     &      refine_ele%x_refine(1,3))
        call position_2_xyz                                             &
     &     (refine_surf%ntot_nod_refine, refine_surf%sph_refine(1,1),   &
     &      refine_surf%sph_refine(1,2), refine_surf%sph_refine(1,3),   &
     &      refine_surf%x_refine(1,1), refine_surf%x_refine(1,2),       &
     &      refine_surf%x_refine(1,3))
        call position_2_xyz                                             &
     &     (refine_edge%ntot_nod_refine, refine_edge%sph_refine(1,1),   &
     &      refine_edge%sph_refine(1,2), refine_edge%sph_refine(1,3),   &
     &      refine_edge%x_refine(1,1), refine_edge%x_refine(1,2),       &
     &      refine_edge%x_refine(1,3))
!
      else if (iflag_interpolate_type .eq. 2) then
!
        call cal_xyz_on_edge_4_refine                                   &
     &     (node%numnod, edge%numedge, edge%ie_edge, node%xx,           &
     &      refine_edge%ntot_nod_refine, refine_edge%istack_nod_refine, &
     &      refine_edge%xi_refine, refine_edge%x_refine)
        call cal_xyz_on_surf_4_refine                                   &
     &     (node%numnod, surf%numsurf, surf%ie_surf, node%xx,           &
     &      refine_surf%ntot_nod_refine, refine_surf%istack_nod_refine, &
     &      refine_surf%xi_refine, refine_surf%x_refine)
        call cal_xyz_on_ele_4_refine                                    &
     &     (node%numnod, ele%numele, ele%ie, node%xx,                   &
     &      refine_ele%ntot_nod_refine, refine_ele%istack_nod_refine,   &
     &      refine_ele%xi_refine, refine_ele%x_refine)
!
        call cal_r_on_edge_4_refine (node%numnod, edge%numedge,         &
     &      edge%ie_edge, node%xx, node%rr,                             &
     &      refine_edge%ntot_nod_refine, refine_edge%istack_nod_refine, &
     &      refine_edge%x_refine, refine_edge%xi_refine,                &
     &      refine_edge%sph_refine)
        call cal_r_on_surf_4_refine                                     &
     &     (node%numnod, surf%numsurf, surf%ie_surf, node%rr,           &
     &      refine_surf%ntot_nod_refine, refine_surf%istack_nod_refine, &
     &      refine_surf%xi_refine, refine_surf%sph_refine)
        call cal_r_on_ele_4_refine                                      &
     &     (node%numnod, ele%numele, ele%ie, node%rr,                   &
     &      refine_ele%ntot_nod_refine, refine_ele%istack_nod_refine,   &
     &      refine_ele%xi_refine, refine_ele%sph_refine)
!
        call set_x_refine_2_sphere(refine_edge%ntot_nod_refine,         &
     &      refine_edge%x_refine, refine_edge%sph_refine)
        call set_x_refine_2_sphere(refine_surf%ntot_nod_refine,         &
     &      refine_surf%x_refine, refine_surf%sph_refine)
        call set_x_refine_2_sphere(refine_ele%ntot_nod_refine,          &
     &      refine_ele%x_refine, refine_ele%sph_refine)
!
      else
!
        call cal_xyz_on_edge_4_refine                                   &
     &     (node%numnod, edge%numedge, edge%ie_edge, node%xx,           &
     &      refine_edge%ntot_nod_refine, refine_edge%istack_nod_refine, &
     &      refine_edge%xi_refine, refine_edge%x_refine)
        call cal_xyz_on_surf_4_refine                                   &
     &     (node%numnod, surf%numsurf, surf%ie_surf, node%xx,           &
     &      refine_surf%ntot_nod_refine, refine_surf%istack_nod_refine, &
     &      refine_surf%xi_refine, refine_surf%x_refine)
        call cal_xyz_on_ele_4_refine                                    &
     &     (node%numnod, ele%numele, ele%ie, node%xx,                   &
     &      refine_ele%ntot_nod_refine, refine_ele%istack_nod_refine,   &
     &      refine_ele%xi_refine, refine_ele%x_refine)
!
      end if
!
      end subroutine s_set_refined_position
!
!  ---------------------------------------------------------------------
!
      end module set_refined_position
