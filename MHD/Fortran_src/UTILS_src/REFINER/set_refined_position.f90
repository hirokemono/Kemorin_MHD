!set_refined_position.f90
!      module set_refined_position
!
!      subroutine s_set_refined_position(node, ele, surf, edge)
!        type(node_data), intent(in) :: node
!        type(element_data), intent(in) :: ele
!        type(surface_data), intent(in) :: surf
!        type(edge_data), intent(in) :: edge
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
      subroutine s_set_refined_position(node, ele, surf, edge)
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      use m_control_param_4_refiner
      use m_refined_node_id
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
!
!
      if (iflag_interpolate_type .eq. 1) then
        call cal_sph_on_edge_4_refine(node%numnod, edge%numedge,        &
     &      edge%ie_edge, node%rr, node%theta, node%phi)
        call cal_sph_on_surf_4_refine(node%numnod, surf%numsurf,        &
     &      surf%ie_surf, node%rr, node%theta, node%phi)
        call cal_sph_on_ele_4_refine(node%numnod, ele%numele,           &
     &      ele%ie, node%rr, node%theta, node%phi)
!
        call s_cal_refined_nod_near_pole(node%numnod,                   &
     &      surf%numsurf, edge%numedge, surf%ie_surf,                   &
     &      edge%ie_edge, node%phi)
!
        call position_2_xyz(ntot_nod_refine_ele, sph_refine_ele(1,1),   &
     &      sph_refine_ele(1,2), sph_refine_ele(1,3),                   &
     &      x_refine_ele(1,1), x_refine_ele(1,2), x_refine_ele(1,3))
        call position_2_xyz(ntot_nod_refine_surf, sph_refine_surf(1,1), &
     &      sph_refine_surf(1,2), sph_refine_surf(1,3),                 &
     &      x_refine_surf(1,1), x_refine_surf(1,2), x_refine_surf(1,3))
        call position_2_xyz(ntot_nod_refine_edge, sph_refine_edge(1,1), &
     &      sph_refine_edge(1,2), sph_refine_edge(1,3),                 &
     &      x_refine_edge(1,1), x_refine_edge(1,2), x_refine_edge(1,3))
!
      else if (iflag_interpolate_type .eq. 2) then
!
        call cal_xyz_on_edge_4_refine                                   &
     &     (node%numnod, edge%numedge, edge%ie_edge, node%xx)
        call cal_xyz_on_surf_4_refine                                   &
     &     (node%numnod, surf%numsurf, surf%ie_surf, node%xx)
        call cal_xyz_on_ele_4_refine                                    &
     &     (node%numnod, ele%numele, ele%ie, node%xx)
!
        call cal_r_on_edge_4_refine (node%numnod, edge%numedge,         &
     &      edge%ie_edge, node%xx, node%rr)
        call cal_r_on_surf_4_refine                                     &
     &     (node%numnod, surf%numsurf, surf%ie_surf, node%rr)
        call cal_r_on_ele_4_refine                                      &
     &     (node%numnod, ele%numele, ele%ie, node%rr)
!
        call set_x_refine_edge_2_sphere
        call set_x_refine_surf_2_sphere
        call set_x_refine_ele_2_sphere
!
      else
!
        call cal_xyz_on_edge_4_refine                                   &
     &     (node%numnod, edge%numedge, edge%ie_edge, node%xx)
        call cal_xyz_on_surf_4_refine                                   &
     &     (node%numnod, surf%numsurf, surf%ie_surf, node%xx)
        call cal_xyz_on_ele_4_refine                                    &
     &     (node%numnod, ele%numele, ele%ie, node%xx)
!
      end if
!
      call deallocate_refined_sph
!
      end subroutine s_set_refined_position
!
!  ---------------------------------------------------------------------
!
      end module set_refined_position
