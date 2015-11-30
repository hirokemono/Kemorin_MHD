!
!      module m_count_num_surf_scalar
!
!      Written by H. Matsui on Sep., 2005
!      Modified by H. Matsui on Feb., 2009
!
!      subroutine count_num_surf_temp(sf_grp)
!      subroutine count_num_surf_press(sf_grp)
!      subroutine count_num_surf_magne_p(sf_grp)
!      subroutine count_num_surf_composition(sf_grp)
!
!      subroutine count_num_surf_h_flux(sf_grp)
!      subroutine count_num_surf_press_grad(sf_grp)
!      subroutine count_num_surf_magp_grad(sf_grp)
!      subroutine count_num_surf_composition_grad(sf_grp)
!
!      subroutine count_num_wall_press(sf_grp)
!      subroutine count_num_wall_magne_p(sf_grp)
!
      module m_count_num_surf_scalar
!
      use m_precision
!
      use t_group_data
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
      subroutine count_num_surf_temp(sf_grp)
!
      use m_surf_data_temp
      use set_surf_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_count_num_surf_scalar(sf_grp%num_grp, sf_grp%grp_name,     &
     &    h_flux_surf%num_bc, h_flux_surf%bc_name,                      &
     &    h_flux_surf%ibc_type, sf_sgs1_grad_t%ngrp_sf_dat)
!
      end subroutine count_num_surf_temp
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_press(sf_grp)
!
      use m_surf_data_press
      use set_surf_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_count_num_surf_scalar(sf_grp%num_grp, sf_grp%grp_name,     &
     &    wall_surf%num_bc, wall_surf%bc_name, wall_surf%ibc_type,      &
     &    sf_sgs1_grad_p%ngrp_sf_dat)
!
      end subroutine count_num_surf_press
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_magne_p(sf_grp)
!
      use m_surf_data_magne_p
      use set_surf_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_count_num_surf_scalar(sf_grp%num_grp, sf_grp%grp_name,     &
     &    e_potential_surf%num_bc, e_potential_surf%bc_name,            &
     &    e_potential_surf%ibc_type, sf_sgs1_grad_f%ngrp_sf_dat)
!
      end subroutine count_num_surf_magne_p
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_composition(sf_grp)
!
      use m_surf_data_composition
      use set_surf_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_count_num_surf_scalar(sf_grp%num_grp, sf_grp%grp_name,     &
     &    light_surf%num_bc, light_surf%bc_name,                        &
     &    light_surf%ibc_type, sf_sgs1_grad_c%ngrp_sf_dat)
!
      end subroutine count_num_surf_composition
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_h_flux(sf_grp)
!
      use m_surf_data_temp
      use set_sf_grad_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call count_num_surf_grad_scalar                                   &
     &   (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,           &
     &    h_flux_surf%num_bc, h_flux_surf%bc_name,                      &
     &    h_flux_surf%ibc_type, name_hf,                                &
     &    sf_bc1_grad_t%ngrp_sf_fix_fx, sf_bc1_grad_t%nitem_sf_fix_fx,  &
     &    sf_bc1_lead_gd_t%ngrp_sf_dat)
!
      end subroutine count_num_surf_h_flux
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_press_grad(sf_grp)
!
      use m_surf_data_press
      use set_sf_grad_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call count_num_surf_grad_scalar                                   &
     &    (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,          &
     &     wall_surf%num_bc, wall_surf%bc_name,                         &
     &     wall_surf%ibc_type, name_pg,                                 &
     &     sf_bc1_grad_p%ngrp_sf_fix_fx, sf_bc1_grad_p%nitem_sf_fix_fx, &
     &     sf_bc1_lead_gd_p%ngrp_sf_dat)
!
      end subroutine count_num_surf_press_grad
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_magp_grad(sf_grp)
!
      use m_surf_data_magne_p
      use set_sf_grad_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call count_num_surf_grad_scalar                                   &
     &   (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,           &
     &    e_potential_surf%num_bc, e_potential_surf%bc_name,            &
     &    e_potential_surf%ibc_type, name_mpg,                          &
     &    sf_bc1_grad_f%ngrp_sf_fix_fx, sf_bc1_grad_f%nitem_sf_fix_fx,  &
     &    sf_bc1_lead_gd_f%ngrp_sf_dat)
!
      end subroutine count_num_surf_magp_grad
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_composition_grad(sf_grp)
!
      use m_surf_data_composition
      use set_sf_grad_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call count_num_surf_grad_scalar                                   &
     &    (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,          &
     &     light_surf%num_bc, light_surf%bc_name,                       &
     &     light_surf%ibc_type, name_dsg,                               &
     &     sf_bc1_grad_c%ngrp_sf_fix_fx, sf_bc1_grad_c%nitem_sf_fix_fx, &
     &     sf_bc1_lead_gd_c%ngrp_sf_dat)
!
      end subroutine count_num_surf_composition_grad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_wall_press(sf_grp)
!
      use m_surf_data_press
      use set_wall_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call count_num_wall_surf(sf_grp%num_grp, sf_grp%grp_name,         &
     &    wall_surf%num_bc, wall_surf%bc_name, wall_surf%ibc_type,      &
     &    sf_bc1_wall_p%ngrp_sf_dat, sf_bc1_spin_p%ngrp_sf_dat,         &
     &    sf_bc1_spout_p%ngrp_sf_dat)
!
      end subroutine count_num_wall_press
!
!-----------------------------------------------------------------------
!
      subroutine count_num_wall_magne_p(sf_grp)
!
      use m_surf_data_magne_p
      use set_wall_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call count_num_wall_surf(sf_grp%num_grp, sf_grp%grp_name,         &
     &   e_potential_surf%num_bc, e_potential_surf%bc_name,             &
     &   e_potential_surf%ibc_type, sf_bc1_wall_f%ngrp_sf_dat,          &
     &   sf_bc1_spin_f%ngrp_sf_dat, sf_bc1_spout_f%ngrp_sf_dat)
!
      end subroutine count_num_wall_magne_p
!
!-----------------------------------------------------------------------
!
      end module m_count_num_surf_scalar
