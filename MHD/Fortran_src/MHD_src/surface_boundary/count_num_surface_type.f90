!
!      module count_num_surface_type
!
!      Written by H. Matsui on Feb., 2009
!
!      subroutine s_count_num_bc_surface_type(grp, sf_dat)
!        type(mesh_groups), intent(in) :: grp
!        type(surface_boundarty_conditions), intent(inout) :: sf_dat
!
      module count_num_surface_type
!
      use m_precision
!
      use t_mesh_data
      use t_surface_bc_data
!
      implicit none
!
      private :: count_num_bc_h_flux
      private :: count_num_bc_torque,     count_num_bc_press_sf
      private :: count_num_bc_vecp_sf,    count_num_bc_magne_sf
      private :: count_num_bc_current_sf, count_num_surf_mag_p
      private :: count_num_bc_composition_sf
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine s_count_num_bc_surface_type(grp, sf_dat)
!
      use m_machine_parameter
!
      type(mesh_groups), intent(in) :: grp
      type(surface_boundarty_conditions), intent(inout) :: sf_dat
!
!
      if (iflag_debug.eq.1)  write(*,*) 'count_num_bc_h_flux'
      call count_num_bc_h_flux(grp%surf_grp, sf_dat%temp)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_num_bc_torque'
      call count_num_bc_torque(grp%surf_grp, grp%surf_nod_grp,          &
     &    sf_dat%velo)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_num_bc_press_sf'
      call count_num_bc_press_sf(grp%surf_grp, sf_dat%press)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_num_bc_vecp_sf'
      call count_num_bc_vecp_sf(grp%surf_grp, grp%surf_nod_grp,         &
     &    sf_dat%vector_p)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_num_bc_magne_sf'
      call count_num_bc_magne_sf(grp%surf_grp, grp%surf_nod_grp,        &
     &    sf_dat%magne)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_num_bc_current_sf'
      call count_num_bc_current_sf(grp%surf_grp, grp%surf_nod_grp,      &
     &    sf_dat%current)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_surf_mag_p'
      call count_num_surf_mag_p(grp%surf_grp, sf_dat%magne_p)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_composition_sf'
      call count_num_bc_composition_sf(grp%surf_grp, sf_dat%comp_sf)
!
      end subroutine s_count_num_bc_surface_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_bc_h_flux(sf_grp, temp)
!
      use count_num_surf_scalar_type
!
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type),  intent(inout) :: temp
!
!
      write(*,*) 'count_num_surf_temp_type'
      call count_num_surf_temp_type(sf_grp, temp)
      write(*,*) 'count_num_surf_h_flux_type'
      call count_num_surf_h_flux_type(sf_grp, temp)
!
      end subroutine count_num_bc_h_flux
!
!-----------------------------------------------------------------------
!
      subroutine count_num_bc_torque(sf_grp, sf_nod, velo)
!
      use count_num_surf_vector_type
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(velocity_surf_bc_type), intent(inout) :: velo
!
!
      call count_num_surf_velo_type(sf_grp, sf_nod, velo)
      call count_num_surf_torque_type(sf_grp, velo)
!
      end subroutine count_num_bc_torque
!
!-----------------------------------------------------------------------
!
      subroutine count_num_bc_press_sf(sf_grp, press)
!
      use count_num_surf_scalar_type
!
      type(surface_group_data), intent(in) :: sf_grp
      type(potential_surf_bc_type),  intent(inout) :: press
!
!
      call count_num_surf_press_type(sf_grp, press)
      call count_num_surf_press_grad_type(sf_grp, press)
      call count_num_wall_press_type(sf_grp, press)
!
      end subroutine count_num_bc_press_sf
!
!-----------------------------------------------------------------------
!
      subroutine count_num_bc_vecp_sf(sf_grp, sf_nod, vector_p)
!
      use count_num_surf_vector_type
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(velocity_surf_bc_type), intent(inout) :: vector_p
!
!
      call count_num_surf_vect_p_type(sf_grp, sf_nod, vector_p)
      call count_num_surf_grad_vecp_type(sf_grp, vector_p)
!
      end subroutine count_num_bc_vecp_sf
!
!-----------------------------------------------------------------------
!
      subroutine count_num_bc_magne_sf(sf_grp, sf_nod, magne)
!
      use count_num_surf_vector_type
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(vector_surf_bc_type), intent(inout) :: magne
!
!
      call count_num_surf_magne_type(sf_grp, sf_nod, magne)
      call count_num_surf_grad_b_type(sf_grp, magne)
!
      end subroutine count_num_bc_magne_sf
!
!-----------------------------------------------------------------------
!
      subroutine count_num_bc_current_sf(sf_grp, sf_nod, current)
!
      use count_num_surf_vector_type
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(vector_surf_bc_type), intent(inout) :: current
!
      call count_num_surf_current_type(sf_grp, sf_nod, current)
      call count_num_surf_grad_j_type(sf_grp, current)
!
      end subroutine count_num_bc_current_sf
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_mag_p(sf_grp, magne_p)
!
      use count_num_surf_scalar_type
!
      type(surface_group_data), intent(in) :: sf_grp
      type(potential_surf_bc_type),  intent(inout) :: magne_p
!
!
      call count_num_surf_magne_p_type(sf_grp, magne_p)
      call count_num_surf_magp_grad_type(sf_grp, magne_p)
      call count_num_wall_magne_p_type(sf_grp, magne_p)
!
      end subroutine count_num_surf_mag_p
!
!-----------------------------------------------------------------------
!
      subroutine count_num_bc_composition_sf(sf_grp, comp_sf)
!
      use count_num_surf_scalar_type
!
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type), intent(inout) :: comp_sf
!
!
      call count_num_surf_composit_type(sf_grp, comp_sf)
      call count_num_surf_cmpst_grad_type(sf_grp, comp_sf)
!
      end subroutine count_num_bc_composition_sf
!
!-----------------------------------------------------------------------
!
      end module count_num_surface_type
