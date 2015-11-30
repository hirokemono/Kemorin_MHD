!
!      module m_scalar_surf_id
!
!      Written by H. Matsui on Sep. 2005
!
!      subroutine set_surf_temp_id(sf_grp)
!      subroutine set_surf_press_id(sf_grp)
!      subroutine set_surf_magne_p_id(sf_grp)
!      subroutine set_surf_fix_composition_id(sf_grp)
!
!      subroutine set_surf_heat_flux_id(sf_grp)
!      subroutine set_surf_grad_press_id(sf_grp)
!      subroutine set_surf_grad_magne_p_id(sf_grp)
!      subroutine set_surf_grad_composition_id(sf_grp)
!
!      subroutine set_wall_press_id(sf_grp)
!      subroutine set_wall_magne_p_id(sf_grp)
!
      module m_scalar_surf_id
!
      use m_precision
!
      use m_surf_data_list
      use m_header_4_surface_bc
      use t_group_data
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_temp_id(sf_grp)
!
      use m_surf_data_temp
      use set_surf_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_set_surf_scalar_id(sf_grp%num_grp, sf_grp%grp_name,        &
     &   h_flux_surf%num_bc, h_flux_surf%bc_name, h_flux_surf%ibc_type, &
     &   sf_sgs1_grad_t%ngrp_sf_dat, sf_sgs1_grad_t%id_grp_sf_dat)
!
      end subroutine set_surf_temp_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_press_id(sf_grp)
!
      use m_surf_data_press
      use set_surf_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_set_surf_scalar_id(sf_grp%num_grp, sf_grp%grp_name,        &
     &    wall_surf%num_bc, wall_surf%bc_name, wall_surf%ibc_type,      &
     &    sf_sgs1_grad_p%ngrp_sf_dat, sf_sgs1_grad_p%id_grp_sf_dat)
!
      end subroutine set_surf_press_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_magne_p_id(sf_grp)
!
      use m_surf_data_magne_p
      use set_surf_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_set_surf_scalar_id(sf_grp%num_grp, sf_grp%grp_name,        &
     &   e_potential_surf%num_bc, e_potential_surf%bc_name,             &
     &   e_potential_surf%ibc_type, sf_sgs1_grad_f%ngrp_sf_dat,         &
     &   sf_sgs1_grad_f%id_grp_sf_dat)
!
      end subroutine set_surf_magne_p_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_fix_composition_id(sf_grp)
!
      use m_surf_data_composition
      use set_surf_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_set_surf_scalar_id(sf_grp%num_grp, sf_grp%grp_name,        &
     &   light_surf%num_bc, light_surf%bc_name, light_surf%ibc_type,    &
     &   sf_sgs1_grad_c%ngrp_sf_dat, sf_sgs1_grad_c%id_grp_sf_dat)
!
      end subroutine set_surf_fix_composition_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_surf_heat_flux_id(sf_grp)
!
      use m_surf_data_temp
      use set_sf_grad_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_set_surf_grad_scalar_id(sf_grp,                            &
     &   h_flux_surf%num_bc, h_flux_surf%bc_name,                       &
     &   h_flux_surf%ibc_type, h_flux_surf%bc_magnitude, name_hf,       &
     &   sf_bc1_grad_t%ngrp_sf_fix_fx, sf_bc1_grad_t%id_grp_sf_fix_fx,  &
     &   sf_bc1_grad_t%nitem_sf_fix_fx,                                 &
     &   sf_bc1_grad_t%ist_ele_sf_fix_fx, sf_bc1_grad_t%sf_apt_fix_fx,  &
     &   sf_bc1_lead_gd_t%ngrp_sf_dat, sf_bc1_lead_gd_t%id_grp_sf_dat)
!
      end subroutine set_surf_heat_flux_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_press_id(sf_grp)
!
      use m_surf_data_press
      use set_sf_grad_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_set_surf_grad_scalar_id(sf_grp,                            &
     &   wall_surf%num_bc, wall_surf%bc_name,                           &
     &   wall_surf%ibc_type, wall_surf%bc_magnitude, name_pg,           &
     &   sf_bc1_grad_p%ngrp_sf_fix_fx, sf_bc1_grad_p%id_grp_sf_fix_fx,  &
     &   sf_bc1_grad_p%nitem_sf_fix_fx,                                 &
     &   sf_bc1_grad_p%ist_ele_sf_fix_fx, sf_bc1_grad_p%sf_apt_fix_fx,  &
     &   sf_bc1_lead_gd_p%ngrp_sf_dat, sf_bc1_lead_gd_p%id_grp_sf_dat)
!
      end subroutine set_surf_grad_press_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_magne_p_id(sf_grp)
!
      use m_surf_data_magne_p
      use set_sf_grad_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_set_surf_grad_scalar_id(sf_grp,                            &
     &   e_potential_surf%num_bc, e_potential_surf%bc_name,             &
     &   e_potential_surf%ibc_type, e_potential_surf%bc_magnitude,      &
     &   name_mpg, sf_bc1_grad_f%ngrp_sf_fix_fx,                        &
     &   sf_bc1_grad_f%id_grp_sf_fix_fx, sf_bc1_grad_f%nitem_sf_fix_fx, &
     &   sf_bc1_grad_f%ist_ele_sf_fix_fx, sf_bc1_grad_f%sf_apt_fix_fx,  &
     &   sf_bc1_lead_gd_f%ngrp_sf_dat, sf_bc1_lead_gd_f%id_grp_sf_dat)
!
      end subroutine set_surf_grad_magne_p_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_composition_id(sf_grp)
!
      use m_surf_data_composition
      use set_sf_grad_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_set_surf_grad_scalar_id(sf_grp,                            &
     &   light_surf%num_bc, light_surf%bc_name,                         &
     &   light_surf%ibc_type, light_surf%bc_magnitude, name_dsg,        &
     &   sf_bc1_grad_c%ngrp_sf_fix_fx, sf_bc1_grad_c%id_grp_sf_fix_fx,  &
     &   sf_bc1_grad_c%nitem_sf_fix_fx,                                 &
     &   sf_bc1_grad_c%ist_ele_sf_fix_fx, sf_bc1_grad_c%sf_apt_fix_fx,  &
     &   sf_bc1_lead_gd_c%ngrp_sf_dat, sf_bc1_lead_gd_c%id_grp_sf_dat)
!
      end subroutine set_surf_grad_composition_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_wall_press_id(sf_grp)
!
      use m_surf_data_press
      use set_wall_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_set_wall_scalar_id(sf_grp%num_grp, sf_grp%grp_name,        &
     &    wall_surf%num_bc, wall_surf%bc_name, wall_surf%ibc_type,      &
     &    sf_bc1_wall_p%ngrp_sf_dat, sf_bc1_wall_p%id_grp_sf_dat,       &
     &    sf_bc1_spin_p%ngrp_sf_dat, sf_bc1_spin_p%id_grp_sf_dat,       &
     &    sf_bc1_spout_p%ngrp_sf_dat, sf_bc1_spout_p%id_grp_sf_dat)
!
      end subroutine set_wall_press_id
!
!-----------------------------------------------------------------------
!
      subroutine set_wall_magne_p_id(sf_grp)
!
      use m_surf_data_magne_p
      use set_wall_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_set_wall_scalar_id(sf_grp%num_grp, sf_grp%grp_name,        &
     &    e_potential_surf%num_bc, e_potential_surf%bc_name,            &
     &    e_potential_surf%ibc_type,                                    &
     &    sf_bc1_wall_f%ngrp_sf_dat, sf_bc1_wall_f%id_grp_sf_dat,       &
     &    sf_bc1_spin_f%ngrp_sf_dat, sf_bc1_spin_f%id_grp_sf_dat,       &
     &    sf_bc1_spout_f%ngrp_sf_dat, sf_bc1_spout_f%id_grp_sf_dat)
!
      end subroutine set_wall_magne_p_id
!
!-----------------------------------------------------------------------
!
      end module m_scalar_surf_id
