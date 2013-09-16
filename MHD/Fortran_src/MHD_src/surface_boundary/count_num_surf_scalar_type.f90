!count_num_surf_scalar_type.f90
!      module count_num_surf_scalar_type
!
!        (module m_count_num_surf_scalar)
!
!      Written by H. Matsui on Sep., 2005
!      Modified by H. Matsui on Feb., 2009
!
!      subroutine count_num_surf_temp_type(sf_grp, temp)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(scaler_surf_bc_type),  intent(inout) :: temp
!      subroutine count_num_surf_composit_type(sf_grp, comp_sf)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(scaler_surf_bc_type),  intent(inout) :: comp_sf
!      subroutine count_num_surf_press_type(sf_grp, press)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(potential_surf_bc_type),  intent(inout) :: press
!      subroutine count_num_surf_magne_p_type(sf_grp, magne_p)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(potential_surf_bc_type),  intent(inout) :: magne_p
!
!      subroutine count_num_surf_h_flux_type(sf_grp, temp)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(scaler_surf_bc_type),  intent(inout) :: temp
!      subroutine count_num_surf_cmpst_grad_type(sf_grp, comp_sf)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(scaler_surf_bc_type),  intent(inout) :: comp_sf
!      subroutine count_num_surf_press_grad_type(sf_grp, press)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(potential_surf_bc_type),  intent(inout) :: press
!      subroutine count_num_surf_magp_grad_type(sf_grp, magne_p)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(potential_surf_bc_type),  intent(inout) :: magne_p
!
!      subroutine count_num_wall_press_type(sf_grp, press)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(potential_surf_bc_type),  intent(inout) :: press
!      subroutine count_num_wall_magne_p_type(sf_grp, magne_p)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(potential_surf_bc_type),  intent(inout) :: magne_p
!
      module count_num_surf_scalar_type
!
      use m_precision
!
      use t_group_data
      use t_surface_bc_data
      use m_surf_data_list
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_temp_type(sf_grp, temp)
!
      use set_surf_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type),  intent(inout) :: temp
!
!
      call s_count_num_surf_scalar(sf_grp%num_grp, sf_grp%grp_name,     &
     &    num_bc_h_flux, bc_h_flux_name,                                &
     &    ibc_h_flux_type, temp%sgs%ngrp_sf_dat)
!
      end subroutine count_num_surf_temp_type
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_composit_type(sf_grp, comp_sf)
!
      use set_surf_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type),  intent(inout) :: comp_sf
!
!
      call s_count_num_surf_scalar(sf_grp%num_grp, sf_grp%grp_name,     &
     &    num_surf_composition, surf_composit_name,                     &
     &    isurf_composit_type, comp_sf%sgs%ngrp_sf_dat)
!
      end subroutine count_num_surf_composit_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_press_type(sf_grp, press)
!
      use set_surf_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(potential_surf_bc_type),  intent(inout) :: press
!
!
      call s_count_num_surf_scalar(sf_grp%num_grp, sf_grp%grp_name,     &
     &    num_bc_wall, bc_wall_name, ibc_wall_type,                     &
     &    press%sgs%ngrp_sf_dat)
!
      end subroutine count_num_surf_press_type
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_magne_p_type(sf_grp, magne_p)
!
      use set_surf_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(potential_surf_bc_type),  intent(inout) :: magne_p
!
!
      call s_count_num_surf_scalar(sf_grp%num_grp, sf_grp%grp_name,     &
     &    num_surf_magp, surf_magp_name,                                &
     &    isurf_magp_type, magne_p%sgs%ngrp_sf_dat)
!
      end subroutine count_num_surf_magne_p_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_h_flux_type(sf_grp, temp)
!
      use m_header_4_surface_bc
      use set_sf_grad_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type),  intent(inout) :: temp
!
!
      call count_num_surf_grad_scalar                                   &
     &    (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,          &
     &     num_bc_h_flux, bc_h_flux_name, ibc_h_flux_type, name_hf,     &
     &     temp%flux%ngrp_sf_fix_fx, temp%flux%nitem_sf_fix_fx,         &
     &     temp%flux_lead%ngrp_sf_dat)
!
      end subroutine count_num_surf_h_flux_type
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_cmpst_grad_type(sf_grp, comp_sf)
!
      use m_header_4_surface_bc
      use set_sf_grad_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type),  intent(inout) :: comp_sf
!
!
      call count_num_surf_grad_scalar                                   &
     &    (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,          &
     &     num_surf_composition, surf_composit_name,                    &
     &     isurf_composit_type, name_dsg,                               &
     &     comp_sf%flux%ngrp_sf_fix_fx, comp_sf%flux%nitem_sf_fix_fx,   &
     &     comp_sf%flux_lead%ngrp_sf_dat)
!
      end subroutine count_num_surf_cmpst_grad_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_press_grad_type(sf_grp, press)
!
      use m_header_4_surface_bc
      use set_sf_grad_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(potential_surf_bc_type),  intent(inout) :: press
!
!
      call count_num_surf_grad_scalar                                   &
     &    (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,          &
     &     num_bc_wall, bc_wall_name, ibc_wall_type,                    &
     &     name_pg, press%grad%ngrp_sf_fix_fx,                          &
     &     press%grad%nitem_sf_fix_fx, press%grad_lead%ngrp_sf_dat)
!
      end subroutine count_num_surf_press_grad_type
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_magp_grad_type(sf_grp, magne_p)
!
      use m_header_4_surface_bc
      use set_sf_grad_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(potential_surf_bc_type),  intent(inout) :: magne_p
!
!
      call count_num_surf_grad_scalar                                   &
     &    (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,          &
     &     num_surf_magp, surf_magp_name, isurf_magp_type, name_mpg,    &
     &     magne_p%grad%ngrp_sf_fix_fx, magne_p%grad%nitem_sf_fix_fx,   &
     &     magne_p%grad_lead%ngrp_sf_dat)
!
      end subroutine count_num_surf_magp_grad_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_wall_press_type(sf_grp, press)
!
      use set_wall_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(potential_surf_bc_type),  intent(inout) :: press
!
!
      call count_num_wall_surf(sf_grp%num_grp, sf_grp%grp_name,         &
     &   num_bc_wall, bc_wall_name, ibc_wall_type, bc_wall_magnitude,   &
     &   press%wall%ngrp_sf_dat, press%sph_in%ngrp_sf_dat,              &
     &   press%sph_out%ngrp_sf_dat)
!
      end subroutine count_num_wall_press_type
!
!-----------------------------------------------------------------------
!
      subroutine count_num_wall_magne_p_type(sf_grp, magne_p)
!
      use set_wall_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(potential_surf_bc_type),  intent(inout) :: magne_p
!
!
      call count_num_wall_surf(sf_grp%num_grp, sf_grp%grp_name,         &
     &   num_surf_magp, surf_magp_name,                                 &
     &   isurf_magp_type, surf_magp_magnitude,                          &
     &   magne_p%wall%ngrp_sf_dat, magne_p%sph_in%ngrp_sf_dat,          &
     &   magne_p%sph_out%ngrp_sf_dat)
!
      end subroutine count_num_wall_magne_p_type
!
!-----------------------------------------------------------------------
!
      end module count_num_surf_scalar_type
