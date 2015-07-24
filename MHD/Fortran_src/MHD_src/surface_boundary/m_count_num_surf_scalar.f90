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
     &    h_flux_surf%ibc_type, ngrp_sf_sgs_temp)
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
     &    ngrp_sf_sgs_p)
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
     &    e_potential_surf%ibc_type, ngrp_sf_sgs_magp)
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
     &    light_surf%ibc_type, ngrp_sf_sgs_cmg)
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
     &    ngrp_sf_fix_hf, nele_sf_fix_hf, ngrp_sf_lead_hf)
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
     &     wall_surf%num_bc, wall_surf%bc_name, wall_surf%ibc_type,     &
     &     name_pg, ngrp_sf_fix_pg, nele_sf_fix_pg, ngrp_sf_lead_p)
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
     &    e_potential_surf%ibc_type, name_mpg, ngrp_sf_fix_mpg,         &
     &    nele_sf_fix_mpg, ngrp_sf_lead_mp)
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
     &     ngrp_sf_fix_cmg, nele_sf_fix_cmg, ngrp_sf_lead_cmg)
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
     &    ngrp_sf_wall_p, ngrp_sf_spin_p, ngrp_sf_spout_p)
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
     &   e_potential_surf%ibc_type, ngrp_sf_wall_mp,                    &
     &   ngrp_sf_spin_mp, ngrp_sf_spout_mp)
!
      end subroutine count_num_wall_magne_p
!
!-----------------------------------------------------------------------
!
      end module m_count_num_surf_scalar
