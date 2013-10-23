!
!      module m_count_num_surf_scalar
!
!      Written by H. Matsui on Sep., 2005
!      Modified by H. Matsui on Feb., 2009
!
!      subroutine count_num_surf_temp
!      subroutine count_num_surf_press
!      subroutine count_num_surf_magne_p
!      subroutine count_num_surf_composition
!
!      subroutine count_num_surf_h_flux
!      subroutine count_num_surf_press_grad
!      subroutine count_num_surf_magp_grad
!      subroutine count_num_surf_composition_grad
!
!      subroutine count_num_wall_press
!      subroutine count_num_wall_magne_p
!
      module m_count_num_surf_scalar
!
      use m_precision
!
      use m_surface_group
      use m_surf_data_list
      use m_header_4_surface_bc
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_temp
!
      use m_surf_data_temp
      use set_surf_scalar_id
!
!
      call s_count_num_surf_scalar(num_surf, surf_name,                 &
     &    h_flux_surf%num_bc, h_flux_surf%bc_name,                      &
     &    h_flux_surf%ibc_type, ngrp_sf_sgs_temp)
!
      end subroutine count_num_surf_temp
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_press
!
      use m_surf_data_press
      use set_surf_scalar_id
!
!
      call s_count_num_surf_scalar(num_surf, surf_name,                 &
     &    wall_surf%num_bc, wall_surf%bc_name, wall_surf%ibc_type,      &
     &    ngrp_sf_sgs_p)
!
      end subroutine count_num_surf_press
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_magne_p
!
      use m_surf_data_magne_p
      use set_surf_scalar_id
!
!
      call s_count_num_surf_scalar(num_surf, surf_name,                 &
     &    e_potential_surf%num_bc, e_potential_surf%bc_name,            &
     &    e_potential_surf%ibc_type, ngrp_sf_sgs_magp)
!
      end subroutine count_num_surf_magne_p
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_composition
!
      use m_surf_data_composition
      use set_surf_scalar_id
!
!
      call s_count_num_surf_scalar                                      &
     &  (num_surf, surf_name, light_surf%num_bc, light_surf%bc_name,    &
     &   light_surf%ibc_type, ngrp_sf_sgs_cmg)
!
      end subroutine count_num_surf_composition
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_h_flux
!
      use m_surf_data_temp
      use set_sf_grad_scalar_id
!
!
      call count_num_surf_grad_scalar                                   &
     &   (num_surf, surf_istack, surf_name,                             &
     &    h_flux_surf%num_bc, h_flux_surf%bc_name,                      &
     &    h_flux_surf%ibc_type, name_hf,                                &
     &    ngrp_sf_fix_hf, nele_sf_fix_hf, ngrp_sf_lead_hf)
!
      end subroutine count_num_surf_h_flux
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_press_grad
!
      use m_surf_data_press
      use set_sf_grad_scalar_id
!
!
      call count_num_surf_grad_scalar                                   &
     &    (num_surf, surf_istack, surf_name,                            &
     &     wall_surf%num_bc, wall_surf%bc_name, wall_surf%ibc_type,     &
     &     name_pg, ngrp_sf_fix_pg, nele_sf_fix_pg, ngrp_sf_lead_p)
!
      end subroutine count_num_surf_press_grad
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_magp_grad
!
      use m_surf_data_magne_p
      use set_sf_grad_scalar_id
!
!
      call count_num_surf_grad_scalar                                   &
     &   (num_surf, surf_istack, surf_name, e_potential_surf%num_bc,    &
     &    e_potential_surf%bc_name, e_potential_surf%ibc_type,          &
     &    name_mpg, ngrp_sf_fix_mpg, nele_sf_fix_mpg, ngrp_sf_lead_mp)
!
      end subroutine count_num_surf_magp_grad
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_composition_grad
!
      use m_surf_data_composition
      use set_sf_grad_scalar_id
!
!
      call count_num_surf_grad_scalar                                   &
     &    (num_surf, surf_istack, surf_name,                            &
     &     light_surf%num_bc, light_surf%bc_name,                       &
     &     light_surf%ibc_type, name_dsg,                               &
     &     ngrp_sf_fix_cmg, nele_sf_fix_cmg, ngrp_sf_lead_cmg)
!
      end subroutine count_num_surf_composition_grad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_wall_press
!
      use m_surf_data_press
      use set_wall_scalar_id
!
!
      call count_num_wall_surf(num_surf, surf_name,                     &
     &    wall_surf%num_bc, wall_surf%bc_name, wall_surf%ibc_type,      &
     &    ngrp_sf_wall_p, ngrp_sf_spin_p, ngrp_sf_spout_p)
!
      end subroutine count_num_wall_press
!
!-----------------------------------------------------------------------
!
      subroutine count_num_wall_magne_p
!
      use m_surf_data_magne_p
      use set_wall_scalar_id
!
!
      call count_num_wall_surf(num_surf, surf_name,                     &
     &   e_potential_surf%num_bc, e_potential_surf%bc_name,             &
     &   e_potential_surf%ibc_type, ngrp_sf_wall_mp,                    &
     &   ngrp_sf_spin_mp, ngrp_sf_spout_mp)
!
      end subroutine count_num_wall_magne_p
!
!-----------------------------------------------------------------------
!
      end module m_count_num_surf_scalar
