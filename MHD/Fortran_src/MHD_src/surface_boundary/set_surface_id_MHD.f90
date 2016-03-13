!
!      module set_surface_id_MHD
!
!      Written by H. Matsui on Sep. 2005
!
!!      subroutine set_surface_id(node, ele, surf, sf_grp, sf_grp_nod,  &
!!     &          sf_grp_v, iphys, nod_fld)
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
      use t_phys_data
      use t_phys_address
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
      use m_surf_data_torque
      use m_surf_data_magne
      use m_surf_data_temp
      use m_scalar_surf_id
      use m_vector_surf_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
!
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_h_flux'
      call count_num_surf_gradient                                      &
     &   (name_hf, sf_grp, h_flux_surf, Tsf1_bcs)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_torque'
      call count_num_surf_grad_velo(name_svn, name_vg,                  &
     &    sf_grp, sf_grp_nod, torque_surf, Vsf1_bcs)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_press_sf'
      call count_num_wall_potential                                     &
     &   (name_pg, sf_grp, wall_surf, Psf1_bcs)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_vecp_sf'
      call count_num_surf_grad_velo(name_san, name_ag,                  &
     &    sf_grp, sf_grp_nod, a_potential_surf, Asf1_bcs)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_magne_sf'
      call count_num_surf_grad_vector(name_sbn, name_bg,                &
     &    sf_grp, sf_grp_nod, magne_surf, Bsf1_bcs)
!
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_current_sf'
      call count_num_surf_grad_vector(name_sjn, name_jg,                &
     &    sf_grp, sf_grp_nod, current_surf, Jsf1_bcs)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_surf_mag_p'
      call count_num_wall_potential                                     &
     &   (name_mpg, sf_grp, e_potential_surf, Fsf1_bcs)
!
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_d_scalar_sf'
      call count_num_surf_gradient                                      &
     &   (name_dsg, sf_grp, light_surf, Csf1_bcs)
!
      end subroutine count_num_surf_bc
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_surface_id(node, ele, surf, sf_grp, sf_grp_nod,    &
     &          sf_grp_v, iphys, nod_fld)
!
      use m_control_parameter
!
      use m_surf_data_torque
      use m_surf_data_magne
      use m_surf_data_temp
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
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call set_surf_grad_velo(name_svn, name_vg,                      &
     &      node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,              &
     &      torque_surf, Vsf1_bcs)
!
        call set_wall_potential_id(sf_grp, wall_surf, Psf1_bcs)
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &      .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call set_surf_grad_vector(name_sbn, name_bg,                    &
     &      node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,              &
     &      magne_surf, Bsf1_bcs)
!
        call set_surf_grad_vector(name_sjn, name_jg,                    &
     &      node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,              &
     &      current_surf, Jsf1_bcs)
!
        call set_wall_potential_id(sf_grp, e_potential_surf, Fsf1_bcs)
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call set_surf_grad_velo(name_san, name_ag,                      &
     &      node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,              &
     &      a_potential_surf, Asf1_bcs)
      end if
! 
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        call set_surf_grad_scalar_id(sf_grp, h_flux_surf, Tsf1_bcs)
      end if
!
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        call set_surf_grad_scalar_id(sf_grp, light_surf, Csf1_bcs)
      end if
!
!     set normal velocity
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call set_normal_velocity                                        &
     &     (sf_grp, sf_grp_nod, Vsf1_bcs%normal, iphys%i_velo, nod_fld)
      end if
!
      end subroutine set_surface_id
!
!-----------------------------------------------------------------------
!
      end module set_surface_id_MHD
