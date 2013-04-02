!
!      module count_num_surface_bc
!
!      Written by H. Matsui on Sep. 2005
!
!      subroutine count_num_surf_bc
!
      module count_num_surface_bc
!
      use m_precision
!
      use m_count_num_surf_scalar
      use m_count_num_surf_vector
!
      implicit none
!
      private :: count_num_bc_h_flux
      private :: count_num_bc_torque,     count_num_bc_press_sf
      private :: count_num_bc_vecp_sf,    count_num_bc_magne_sf
      private :: count_num_bc_current_sf, count_num_surf_mag_p
      private :: count_num_bc_d_scalar_sf
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_bc
!
      use m_machine_parameter
!
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_h_flux'
      call count_num_bc_h_flux
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_torque'
      call count_num_bc_torque
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_press_sf'
      call count_num_bc_press_sf
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_vecp_sf'
      call count_num_bc_vecp_sf
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_magne_sf'
      call count_num_bc_magne_sf
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_current_sf'
      call count_num_bc_current_sf
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_surf_mag_p'
      call count_num_surf_mag_p
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_d_scalar_sf'
      call count_num_bc_d_scalar_sf
!
      end subroutine count_num_surf_bc
!
!-----------------------------------------------------------------------
!
      subroutine count_num_bc_h_flux
!
      call count_num_surf_temp
      call count_num_surf_h_flux
!
      end subroutine count_num_bc_h_flux
!
!-----------------------------------------------------------------------
!
      subroutine count_num_bc_torque
!
      call count_num_surf_velo
      call count_num_surf_torque
!
      end subroutine count_num_bc_torque
!
!-----------------------------------------------------------------------
!
      subroutine count_num_bc_press_sf
!
      call count_num_surf_press
      call count_num_surf_press_grad
      call count_num_wall_press
!
      end subroutine count_num_bc_press_sf
!
!-----------------------------------------------------------------------
!
      subroutine count_num_bc_vecp_sf
!
      call count_num_surf_vect_p
      call count_num_surf_grad_vecp
!
      end subroutine count_num_bc_vecp_sf
!
!-----------------------------------------------------------------------
!
      subroutine count_num_bc_magne_sf
!
      call count_num_surf_magne
      call count_num_surf_grad_b
!
      end subroutine count_num_bc_magne_sf
!
!-----------------------------------------------------------------------
!
      subroutine count_num_bc_current_sf
!
      call count_num_surf_current
      call count_num_surf_grad_j
!
      end subroutine count_num_bc_current_sf
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_mag_p
!
      call count_num_surf_magne_p
      call count_num_surf_magp_grad
      call count_num_wall_magne_p
!
      end subroutine count_num_surf_mag_p
!
!-----------------------------------------------------------------------
!
      subroutine count_num_bc_d_scalar_sf
!
      call count_num_surf_composition
      call count_num_surf_composition_grad
!
      end subroutine count_num_bc_d_scalar_sf
!
!-----------------------------------------------------------------------
!
      end module count_num_surface_bc
