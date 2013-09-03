!
!      module set_surface_id_MHD
!
!      Written by H. Matsui on Sep. 2005
!
!      subroutine set_surface_id
!
      module set_surface_id_MHD
!
      use m_precision
!
      use m_scalar_surf_id
      use m_vector_surf_id
!
      implicit none
!
      private :: set_bc_h_flux_id
      private :: set_bc_torque_id, set_bc_wall_id
      private :: set_bc_magne_surf_id, set_bc_current_surf_id
      private :: set_bc_vect_p_surf_id, set_surf_mag_p_id
      private :: set_surf_composition_id
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine set_surface_id
!
      use m_control_parameter
!
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        call set_bc_h_flux_id
      end if
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call set_bc_torque_id
        call set_bc_wall_id
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &      .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call set_bc_magne_surf_id
        call set_bc_current_surf_id
        call set_surf_mag_p_id
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call set_bc_vect_p_surf_id
      end if
! 
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        call set_surf_composition_id
      end if
! 
      end subroutine set_surface_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_bc_h_flux_id
!
      call set_surf_temp_id
      call set_surf_heat_flux_id
!
      end subroutine set_bc_h_flux_id
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_torque_id
!
!
      call set_surf_velo_id
      call set_surf_torque_id
!
      end subroutine set_bc_torque_id
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_wall_id
!
      call set_surf_press_id
      call set_surf_grad_press_id
      call set_wall_press_id
!
      end subroutine set_bc_wall_id
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_vect_p_surf_id
!
      call set_surf_vect_p_id
      call set_surf_grad_vecp_id
!
      end subroutine set_bc_vect_p_surf_id
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_magne_surf_id
!
      call set_surf_magne_id
      call set_surf_grad_b_id
!
      end subroutine set_bc_magne_surf_id
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_current_surf_id
!
      call set_surf_current_id
      call set_surf_grad_j_id
!
      end subroutine set_bc_current_surf_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_mag_p_id
!
      call set_surf_magne_p_id
      call set_surf_grad_magne_p_id
      call set_wall_magne_p_id
!
      end subroutine set_surf_mag_p_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_composition_id
!
      call set_surf_fix_composition_id
      call set_surf_grad_composition_id
!
      end subroutine set_surf_composition_id
!
!-----------------------------------------------------------------------
!
      end module set_surface_id_MHD
