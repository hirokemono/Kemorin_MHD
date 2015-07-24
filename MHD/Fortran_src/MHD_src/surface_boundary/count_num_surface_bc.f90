!
!      module count_num_surface_bc
!
!      Written by H. Matsui on Sep. 2005
!
!!      subroutine count_num_surf_bc(sf_grp, sf_grp_nod)
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
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
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_bc(sf_grp, sf_grp_nod)
!
      use m_machine_parameter
      use t_group_data
      use t_surface_group_connect
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
!
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_h_flux'
      call count_num_surf_temp(sf_grp)
      call count_num_surf_h_flux(sf_grp)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_torque'
      call count_num_surf_velo(sf_grp, sf_grp_nod)
      call count_num_surf_torque(sf_grp)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_press_sf'
      call count_num_surf_press(sf_grp)
      call count_num_surf_press_grad(sf_grp)
      call count_num_wall_press(sf_grp)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_vecp_sf'
      call count_num_surf_vect_p(sf_grp, sf_grp_nod)
      call count_num_surf_grad_vecp(sf_grp)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_magne_sf'
      call count_num_surf_magne(sf_grp, sf_grp_nod)
      call count_num_surf_grad_b(sf_grp)
!
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_current_sf'
      call count_num_surf_current(sf_grp, sf_grp_nod)
      call count_num_surf_grad_j(sf_grp)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_surf_mag_p'
      call count_num_surf_magne_p(sf_grp)
      call count_num_surf_magp_grad(sf_grp)
      call count_num_wall_magne_p(sf_grp)
!
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_d_scalar_sf'
      call count_num_surf_composition(sf_grp)
      call count_num_surf_composition_grad(sf_grp)
!
      end subroutine count_num_surf_bc
!
!-----------------------------------------------------------------------
!
      end module count_num_surface_bc
