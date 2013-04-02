!
!      module set_surface_type_id
!
!      Written by H. Matsui on Feb., 2009
!
!      subroutine s_set_surface_type_id(grp, sf_dat)
!        type(mesh_groups), intent(in) :: grp
!        type(surface_boundarty_conditions), intent(inout) :: sf_dat
!
      module set_surface_type_id
!
      use m_precision
!
      use t_mesh_data
      use t_surface_bc_data
!
      implicit none
!
      private :: set_bc_torque_type_id, set_bc_wall_type_id
      private :: set_bc_vect_p_surf_type_id, set_bc_magne_surf_type_id
      private :: set_bc_current_surf_type_id, set_surf_mag_p_type_id
      private :: set_bc_h_flux_type_id, set_bc_composition_type_id
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine s_set_surface_type_id(grp, sf_dat)
!
      use m_control_parameter
!
      type(mesh_groups), intent(in) :: grp
      type(surface_boundarty_conditions), intent(inout) :: sf_dat
!
!
      if ( iflag_t_evo_4_temp .ge. 1 ) then
        call set_bc_h_flux_type_id(grp%surf_grp, sf_dat%temp)
      end if
!
      if ( iflag_t_evo_4_velo .ge. 1 ) then
        call set_bc_torque_type_id(grp%surf_grp, grp%surf_nod_grp,      &
     &      sf_dat%velo)
        call set_bc_wall_type_id(grp%surf_grp, sf_dat%press)
      end if
!
      if ( iflag_t_evo_4_magne.ge.1                                     &
     &      .or. iflag_t_evo_4_vect_p.ge.1 ) then
        call set_bc_magne_surf_type_id(grp%surf_grp, grp%surf_nod_grp,  &
     &      sf_dat%magne)
        call set_bc_current_surf_type_id(grp%surf_grp,                  &
     &      grp%surf_nod_grp, sf_dat%current)
        call set_surf_mag_p_type_id(grp%surf_grp, sf_dat%magne_p)
      end if
!
      if ( iflag_t_evo_4_vect_p .ge. 1 ) then
        call set_bc_vect_p_surf_type_id(grp%surf_grp, grp%surf_nod_grp, &
     &      sf_dat%vector_p)
      end if
! 
      if ( iflag_t_evo_4_composit .ge. 1 ) then
        call set_bc_composition_type_id(grp%surf_grp, sf_dat%comp_sf)
      end if
! 
      end subroutine s_set_surface_type_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_bc_h_flux_type_id(sf_grp, temp)
!
      use set_sf_scalar_type_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type),  intent(inout) :: temp
!
      call set_surf_temp_type_id(sf_grp, temp)
      call set_surf_heat_flux_type_id(sf_grp, temp)
!
      end subroutine set_bc_h_flux_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_torque_type_id(sf_grp, sf_nod, velo)
!
      use set_sf_vector_type_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(velocity_surf_bc_type), intent(inout) :: velo
!
!
      call set_surf_velo_type_id(sf_grp, sf_nod, velo)
      call set_surf_torque_type_id(sf_grp, velo)
!
      end subroutine set_bc_torque_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_wall_type_id(sf_grp, press)
!
      use set_sf_scalar_type_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(potential_surf_bc_type),  intent(inout) :: press
!
      call set_surf_press_type_id(sf_grp, press)
      call set_surf_grad_press_type_id(sf_grp, press)
      call set_wall_press_type_id(sf_grp, press)
!
      end subroutine set_bc_wall_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_vect_p_surf_type_id(sf_grp, sf_nod, vector_p)
!
      use set_sf_vector_type_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(vector_surf_bc_type), intent(inout) :: vector_p
!
!
      call set_surf_vect_p_type_id(sf_grp, sf_nod, vector_p)
      call set_surf_grad_vecp_type_id(sf_grp, vector_p)
!
      end subroutine set_bc_vect_p_surf_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_magne_surf_type_id(sf_grp, sf_nod, magne)
!
      use set_sf_vector_type_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(vector_surf_bc_type), intent(inout) :: magne
!
!
      call set_surf_magne_type_id(sf_grp, sf_nod, magne)
      call set_surf_grad_b_type_id(sf_grp, magne)
!
      end subroutine set_bc_magne_surf_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_current_surf_type_id(sf_grp, sf_nod, current)
!
      use set_sf_vector_type_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(vector_surf_bc_type), intent(inout) :: current
!
!
      call set_surf_current_type_id(sf_grp, sf_nod, current)
      call set_surf_grad_j_type_id(sf_grp, current)
!
      end subroutine set_bc_current_surf_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_mag_p_type_id(sf_grp, magne_p)
!
      use set_sf_scalar_type_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(potential_surf_bc_type),  intent(inout) :: magne_p
!
!
      call set_surf_magne_p_type_id(sf_grp, magne_p)
      call set_surf_grad_magne_p_type_id(sf_grp, magne_p)
      call set_wall_magne_p_type_id(sf_grp, magne_p)
!
      end subroutine set_surf_mag_p_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_composition_type_id(sf_grp, comp_sf)
!
      use set_sf_scalar_type_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type),  intent(inout) :: comp_sf
!
!
      call set_surf_composit_type_id(sf_grp, comp_sf)
      call set_surf_grad_composit_type_id(sf_grp, comp_sf)
!
      end subroutine set_bc_composition_type_id
!
!-----------------------------------------------------------------------
!
      end module set_surface_type_id
