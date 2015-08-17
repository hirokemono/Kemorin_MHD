!set_refined_position.f90
!      module set_refined_position
!      subroutine s_set_refined_position
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
      subroutine s_set_refined_position
!
      use m_control_param_4_refiner
      use m_refined_node_id
      use m_geometry_data
!
      use cal_xyz_4_refine
      use cal_sph_4_refine
      use cal_r_4_refine
      use set_refined_nod_2_sphere
      use cal_refined_nod_near_pole
      use coordinate_converter
!
!
      if (iflag_interpolate_type .eq. 1) then
        call cal_sph_on_edge_4_refine(node1%numnod, edge1%numedge,      &
     &      edge1%ie_edge, node1%rr, colatitude, longitude)
        call cal_sph_on_surf_4_refine(node1%numnod, surf1%numsurf,      &
     &      surf1%ie_surf, node1%rr, colatitude, longitude)
        call cal_sph_on_ele_4_refine(node1%numnod, ele1%numele,         &
     &      ele1%ie, node1%rr, colatitude, longitude)
!
        call s_cal_refined_nod_near_pole(node1%numnod,                  &
     &      surf1%numsurf, edge1%numedge, surf1%ie_surf,                &
     &      edge1%ie_edge, longitude)
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
     &     (node1%numnod, edge1%numedge, edge1%ie_edge, node1%xx)
        call cal_xyz_on_surf_4_refine                                   &
     &     (node1%numnod, surf1%numsurf, surf1%ie_surf, node1%xx)
        call cal_xyz_on_ele_4_refine                                    &
     &     (node1%numnod, ele1%numele, ele1%ie, node1%xx)
!
        call cal_r_on_edge_4_refine (node1%numnod, edge1%numedge,       &
     &      edge1%ie_edge, node1%xx, node1%rr)
        call cal_r_on_surf_4_refine                                     &
     &     (node1%numnod, surf1%numsurf, surf1%ie_surf, node1%rr)
        call cal_r_on_ele_4_refine                                      &
     &     (node1%numnod, ele1%numele, ele1%ie, node1%rr)
!
        call set_x_refine_edge_2_sphere
        call set_x_refine_surf_2_sphere
        call set_x_refine_ele_2_sphere
!
      else
!
        call cal_xyz_on_edge_4_refine                                   &
     &     (node1%numnod, edge1%numedge, edge1%ie_edge, node1%xx)
        call cal_xyz_on_surf_4_refine                                   &
     &     (node1%numnod, surf1%numsurf, surf1%ie_surf, node1%xx)
        call cal_xyz_on_ele_4_refine                                    &
     &     (node1%numnod, ele1%numele, ele1%ie, node1%xx)
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
