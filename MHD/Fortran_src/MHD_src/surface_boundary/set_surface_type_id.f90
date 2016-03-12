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
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine s_count_num_bc_surface_type(grp, sf_dat)
!
      use m_machine_parameter
      use m_surf_data_list
      use m_scalar_surf_id
      use m_vector_surf_id
!
      type(mesh_groups), intent(in) :: grp
      type(surface_boundarty_conditions), intent(inout) :: sf_dat
!
!
      if (iflag_debug.eq.1)  write(*,*) 'count_num_bc_h_flux'
      call count_num_surf_gradient                                      &
     &   (name_hf, grp%surf_grp, h_flux_surf, sf_dat%temp)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_num_bc_torque'
      call count_num_surf_grad_velo(name_svn, name_vg,                  &
     &    grp%surf_grp, grp%surf_nod_grp, torque_surf, sf_dat%velo)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_num_bc_press_sf'
      call count_num_wall_potential                                     &
     &   (name_pg, grp%surf_grp, wall_surf, sf_dat%press)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_num_bc_vecp_sf'
      call count_num_surf_grad_velo(name_san, name_ag,                  &
     &    grp%surf_grp, grp%surf_nod_grp, a_potential_surf,             &
     &    sf_dat%vector_p)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_num_bc_magne_sf'
      call count_num_surf_grad_vector(name_sbn, name_bg,                &
     &   grp%surf_grp, grp%surf_nod_grp, magne_surf, sf_dat%magne)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_num_bc_current_sf'
      call count_num_surf_grad_vector(name_sjn, name_jg,                &
     &    grp%surf_grp, grp%surf_nod_grp, current_surf, sf_dat%current)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_surf_mag_p'
      call count_num_wall_potential                                     &
     &   (name_mpg, grp%surf_grp, e_potential_surf, sf_dat%magne_p)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_composition_sf'
      call count_num_surf_gradient                                      &
     &   (name_dsg, grp%surf_grp, light_surf, sf_dat%comp_sf)
!
      end subroutine s_count_num_bc_surface_type
!
!-----------------------------------------------------------------------
!
      subroutine s_set_surface_type_id(node, ele, surf, grp, sf_dat)
!
      use m_control_parameter
      use m_surf_data_list
      use m_scalar_surf_id
      use m_vector_surf_id
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: grp
      type(surface_boundarty_conditions), intent(inout) :: sf_dat
!
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        call set_surf_grad_scalar_id                                    &
     &     (grp%surf_grp, h_flux_surf, sf_dat%temp)
      end if
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call set_surf_grad_velo(name_svn, name_vg,                      &
     &      node, ele, surf, grp%surf_grp,  grp%surf_nod_grp, grp%surf_grp_geom,              &
     &      torque_surf, sf_dat%velo)
!
        call set_wall_potential_id                                      &
     &     (grp%surf_grp, wall_surf, sf_dat%press)
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &      .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call set_surf_grad_vector(name_sbn, name_bg,                    &
     &      node, ele, surf, grp%surf_grp, grp%surf_nod_grp, grp%surf_grp_geom,  &
     &      magne_surf, sf_dat%magne)
        call set_surf_grad_vector(name_sjn, name_jg,                    &
     &      node, ele, surf, grp%surf_grp, grp%surf_nod_grp, grp%surf_grp_geom,  &
     &      current_surf, sf_dat%current)
!
        call set_wall_potential_id                                      &
     &     (grp%surf_grp, e_potential_surf, sf_dat%magne_p)
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call set_surf_grad_velo(name_san, name_ag,                      &
     &      node, ele, surf, grp%surf_grp, grp%surf_nod_grp, grp%surf_grp_geom,  &
     &      a_potential_surf, sf_dat%vector_p)
      end if
! 
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        call set_surf_grad_scalar_id                                    &
     &     (grp%surf_grp, light_surf, sf_dat%comp_sf)
      end if
! 
      end subroutine s_set_surface_type_id
!
!-----------------------------------------------------------------------
!
      end module set_surface_type_id
