!
!      module m_scalar_surf_id
!
!      Written by H. Matsui on Sep. 2005
!
!      subroutine set_surf_temp_id
!      subroutine set_surf_press_id
!      subroutine set_surf_magne_p_id
!      subroutine set_surf_fix_composition_id
!
!      subroutine set_surf_heat_flux_id
!      subroutine set_surf_grad_press_id
!      subroutine set_surf_grad_magne_p_id
!      subroutine set_surf_grad_composition_id
!
!      subroutine set_wall_press_id
!      subroutine set_wall_magne_p_id
!
      module m_scalar_surf_id
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
      subroutine set_surf_temp_id
!
      use m_surf_data_temp
      use set_surf_scalar_id
!
!
      call s_set_surf_scalar_id(num_surf, surf_name,                    &
     &   num_bc_h_flux, bc_h_flux_name, ibc_h_flux_type,                &
     &   ngrp_sf_sgs_temp, id_grp_sf_sgs_temp)
!
      end subroutine set_surf_temp_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_press_id
!
      use m_surf_data_press
      use set_surf_scalar_id
!
!
      call s_set_surf_scalar_id(num_surf, surf_name,                    &
     &   num_bc_wall, bc_wall_name, ibc_wall_type,                      &
     &   ngrp_sf_sgs_p, id_grp_sf_sgs_p)
!
      end subroutine set_surf_press_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_magne_p_id
!
      use m_surf_data_magne_p
      use set_surf_scalar_id
!
!
      call s_set_surf_scalar_id(num_surf, surf_name,                    &
     &   num_surf_magp, surf_magp_name, isurf_magp_type,                &
     &   ngrp_sf_sgs_magp, id_grp_sf_sgs_magp)
!
      end subroutine set_surf_magne_p_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_fix_composition_id
!
      use m_surf_data_composition
      use set_surf_scalar_id
!
!
      call s_set_surf_scalar_id(num_surf, surf_name,                    &
     &   num_surf_composition, surf_composit_name, isurf_composit_type, &
     &   ngrp_sf_sgs_cmg, id_grp_sf_sgs_cmg)
!
      end subroutine set_surf_fix_composition_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_surf_heat_flux_id
!
      use m_surf_data_temp
      use set_sf_grad_scalar_id
!
      call s_set_surf_grad_scalar_id(num_surf, surf_istack, surf_name,  &
     &   num_bc_h_flux, bc_h_flux_name,                                 &
     &   ibc_h_flux_type, bc_h_flux_magnitude, name_hf,                 &
     &   ngrp_sf_fix_hf, id_grp_sf_fix_hf,                              &
     &   nele_sf_fix_hf, ist_ele_sf_fix_hf, sf_apt_fix_hf,              &
     &   ngrp_sf_lead_hf, id_grp_sf_lead_hf)
!
      end subroutine set_surf_heat_flux_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_press_id
!
      use m_surf_data_press
      use set_sf_grad_scalar_id
!
      call s_set_surf_grad_scalar_id(num_surf, surf_istack, surf_name,  &
     &   num_bc_wall, bc_wall_name, ibc_wall_type, bc_wall_magnitude,   &
     &   name_pg, ngrp_sf_fix_pg, id_grp_sf_fix_pg,                     &
     &   nele_sf_fix_pg, ist_ele_sf_fix_pg, sf_apt_fix_pg,              &
     &   ngrp_sf_lead_p, id_grp_sf_lead_p)
!
      end subroutine set_surf_grad_press_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_magne_p_id
!
      use m_surf_data_magne_p
      use set_sf_grad_scalar_id
!
      call s_set_surf_grad_scalar_id(num_surf, surf_istack, surf_name,  &
     &   num_surf_magp, surf_magp_name,                                 &
     &   isurf_magp_type, surf_magp_magnitude, name_mpg,                &
     &   ngrp_sf_fix_mpg, id_grp_sf_fix_mpg,                            &
     &   nele_sf_fix_mpg, ist_ele_sf_fix_mpg, sf_apt_fix_mpg,           &
     &   ngrp_sf_lead_mp, id_grp_sf_lead_mp)
!
      end subroutine set_surf_grad_magne_p_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_composition_id
!
      use m_surf_data_composition
      use set_sf_grad_scalar_id
!
      call s_set_surf_grad_scalar_id(num_surf, surf_istack, surf_name,  &
     &   num_surf_composition, surf_composit_name,                      &
     &   isurf_composit_type, surf_composit_magnitude, name_dsg,        &
     &   ngrp_sf_fix_cmg, id_grp_sf_fix_cmg,                            &
     &   nele_sf_fix_cmg, ist_ele_sf_fix_cmg, sf_apt_fix_cmg,           &
     &   ngrp_sf_lead_cmg, id_grp_sf_lead_cmg)
!
      end subroutine set_surf_grad_composition_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_wall_press_id
!
      use m_surf_data_press
      use set_wall_scalar_id
!
!
      call s_set_wall_scalar_id(num_surf, surf_name,                    &
     &    num_bc_wall, bc_wall_name,                                    &
     &    ibc_wall_type, bc_wall_magnitude,                             &
     &    ngrp_sf_wall_p, id_grp_sf_wall_p,                             &
     &    ngrp_sf_spin_p, id_grp_sf_spin_p,                             &
     &    ngrp_sf_spout_p, id_grp_sf_spout_p)
!
      end subroutine set_wall_press_id
!
!-----------------------------------------------------------------------
!
      subroutine set_wall_magne_p_id
!
      use m_surf_data_magne_p
      use set_wall_scalar_id
!
!
      call s_set_wall_scalar_id(num_surf, surf_name,                    &
     &    num_surf_magp, surf_magp_name,                                &
     &    isurf_magp_type, surf_magp_magnitude,                         &
     &    ngrp_sf_wall_mp, id_grp_sf_wall_mp,                           &
     &    ngrp_sf_spin_mp, id_grp_sf_spin_mp,                           &
     &    ngrp_sf_spout_mp, id_grp_sf_spout_mp)
!
      end subroutine set_wall_magne_p_id
!
!-----------------------------------------------------------------------
!
      end module m_scalar_surf_id
