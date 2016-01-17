!
!      module set_surface_id_MHD
!
!      Written by H. Matsui on Sep. 2005
!
!!      subroutine set_surface_id                                       &
!!     &         (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v)
!
      module set_surface_id_MHD
!
      use m_precision
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_surface_group_geometry
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine set_surface_id                                         &
     &         (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v)
!
      use m_control_parameter
!
      use m_node_phys_data
!
      use m_surf_data_torque
      use m_scalar_surf_id
      use m_vector_surf_id
!
      use set_normal_field
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(surface_group_geometry), intent(in) :: sf_grp_v
!
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        call set_surf_temp_id(sf_grp)
        call set_surf_heat_flux_id(sf_grp)
      end if
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call set_surf_velo_id                                           &
     &     (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v)
        call set_surf_torque_id(sf_grp)
!
        call set_surf_press_id(sf_grp)
        call set_surf_grad_press_id(sf_grp)
        call set_wall_press_id(sf_grp)
!
!     set normal velocity
        call set_normal_velocity                                        &
     &     (sf_grp, sf_grp_nod, sf_bc1_norm_v, iphys%i_velo, nod_fld1)
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &      .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call set_surf_magne_id                                          &
     &     (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v)
        call set_surf_grad_b_id(sf_grp)
!
        call set_surf_current_id                                        &
     &     (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v)
        call set_surf_grad_j_id(sf_grp)
!
        call set_surf_magne_p_id(sf_grp)
        call set_surf_grad_magne_p_id(sf_grp)
        call set_wall_magne_p_id(sf_grp)
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call set_surf_vect_p_id                                         &
     &     (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v)
        call set_surf_grad_vecp_id(sf_grp)
      end if
! 
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        call set_surf_fix_composition_id(sf_grp)
        call set_surf_grad_composition_id(sf_grp)
      end if
! 
      end subroutine set_surface_id
!
!-----------------------------------------------------------------------
!
      end module set_surface_id_MHD
