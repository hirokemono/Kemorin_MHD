!
!      module set_surface_id_MHD
!
!      Written by H. Matsui on Sep. 2005
!
!!      subroutine set_bc_surface_data(IO_bc, node, ele, surf,          &
!!     &          sf_grp, sf_grp_nod, sf_grp_v, MHD_prop, surf_bcs)
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(surface_group_geometry), intent(in) :: sf_grp_v
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(surface_boundarty_conditions), intent(inout) :: surf_bcs
!!
      module set_surface_id_MHD
!
      use m_precision
!
      use t_control_parameter
      use t_physical_property
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_surface_group_geometry
      use t_phys_data
      use t_phys_address
      use t_MHD_boundary_data
      use t_boundary_field_IO
!
      implicit none
!
      private :: count_num_surf_bc, set_surface_id
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_surface_data(IO_bc, node, ele, surf,            &
     &          sf_grp, sf_grp_nod, sf_grp_v, MHD_prop, surf_bcs)
!
      use m_machine_parameter
!
      use set_surface_values
      use set_normal_field
!
      type(IO_boundary), intent(in) :: IO_bc
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(surface_group_geometry), intent(in) :: sf_grp_v
!
      type(surface_boundarty_conditions), intent(inout) :: surf_bcs
!
!
      call allocate_work_4_surf_bc_dat(node%numnod)
!
! ---  set boundary conditions
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_surf_bc'
      call count_num_surf_bc(IO_bc, sf_grp, sf_grp_nod, surf_bcs)
!
      call alloc_surf_bc_data_type                                      &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, surf_bcs)
!
      call set_surface_id                                               &
     &   (IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,         &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, surf_bcs)
!
      call deallocate_work_4_surf_bc_dat
! 
      end subroutine set_bc_surface_data
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_bc                                      &
     &         (IO_bc, sf_grp, sf_grp_nod, surf_bcs)
!
      use m_machine_parameter
      use m_surf_data_list
      use m_scalar_surf_id
      use m_vector_surf_id
!
      type(IO_boundary), intent(in) :: IO_bc
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
!
      type(surface_boundarty_conditions), intent(inout) :: surf_bcs
!
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_h_flux'
      call count_num_surf_gradient                                      &
     &   (name_hf, IO_bc, sf_grp, h_flux_surf, surf_bcs%Tsf_bcs)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_torque'
      call count_num_surf_grad_velo(name_svn, name_vg,                  &
     &    IO_bc, sf_grp, sf_grp_nod, torque_surf, surf_bcs%Vsf_bcs)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_press_sf'
      call count_num_wall_potential                                     &
     &   (name_pg, IO_bc, sf_grp, wall_surf, surf_bcs%Psf_bcs)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_vecp_sf'
      call count_num_surf_grad_velo(name_san, name_ag,                  &
     &    IO_bc, sf_grp, sf_grp_nod, a_potential_surf,                  &
     &    surf_bcs%Asf_bcs)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_magne_sf'
      call count_num_surf_grad_vector(name_sbn, name_bg,                &
     &    IO_bc, sf_grp, sf_grp_nod, magne_surf, surf_bcs%Bsf_bcs)
!
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_current_sf'
      call count_num_surf_grad_vector(name_sjn, name_jg,                &
     &    IO_bc, sf_grp, sf_grp_nod, current_surf, surf_bcs%Jsf_bcs)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_surf_mag_p'
      call count_num_wall_potential                                     &
     &   (name_mpg, IO_bc, sf_grp, e_potential_surf, surf_bcs%Fsf_bcs)
!
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_bc_d_scalar_sf'
      call count_num_surf_gradient                                      &
     &   (name_dsg, IO_bc, sf_grp, light_surf, surf_bcs%Csf_bcs)
!
      end subroutine count_num_surf_bc
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_surface_id                                         &
     &         (IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,   &
     &          fl_prop, cd_prop, ht_prop, cp_prop, surf_bcs)
!
      use m_surf_data_list
      use m_scalar_surf_id
      use m_vector_surf_id
!
      use set_normal_field
!
      type(IO_boundary), intent(in) :: IO_bc
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(surface_group_geometry), intent(in) :: sf_grp_v
!
      type(surface_boundarty_conditions), intent(inout) :: surf_bcs
!
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        call set_surf_grad_velo(name_svn, name_vg,                      &
     &      IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,       &
     &      torque_surf, surf_bcs%Vsf_bcs)
!
        call set_wall_potential_id                                      &
     &     (IO_bc, sf_grp, wall_surf, surf_bcs%Psf_bcs)
      end if
!
      if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution                &
     &      .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call set_surf_grad_vector(name_sbn, name_bg,                    &
     &      IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,       &
     &      magne_surf, surf_bcs%Bsf_bcs)
!
        call set_surf_grad_vector(name_sjn, name_jg,                    &
     &      IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,       &
     &      current_surf, surf_bcs%Jsf_bcs)
!
        call set_wall_potential_id                                      &
     &     (IO_bc, sf_grp, e_potential_surf, surf_bcs%Fsf_bcs)
      end if
!
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call set_surf_grad_velo(name_san, name_ag,                      &
     &      IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,       &
     &      a_potential_surf, surf_bcs%Asf_bcs)
      end if
! 
      if (ht_prop%iflag_scheme .gt. id_no_evolution) then
        call set_surf_grad_scalar_id                                    &
     &     (IO_bc, sf_grp, h_flux_surf, surf_bcs%Tsf_bcs)
      end if
!
      if (cp_prop%iflag_scheme .gt. id_no_evolution) then
        call set_surf_grad_scalar_id                                    &
     &     (IO_bc, sf_grp, light_surf, surf_bcs%Csf_bcs)
      end if
!
      end subroutine set_surface_id
!
!-----------------------------------------------------------------------
!
      end module set_surface_id_MHD
