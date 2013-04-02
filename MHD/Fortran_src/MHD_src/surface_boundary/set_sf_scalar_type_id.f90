!
!      module set_sf_scalar_type_id
!
!        (module m_scalar_surf_id)
!
!      Written by H. Matsui on Sep. 2005
!
!      subroutine set_surf_temp_type_id(sf_grp, temp)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(scaler_surf_bc_type),  intent(inout) :: temp
!      subroutine set_surf_composit_type_id(sf_grp, comp_sf)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(scaler_surf_bc_type),  intent(inout) :: comp_sf
!      subroutine set_surf_press_type_id(sf_grp, press)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(potential_surf_bc_type),  intent(inout) :: press
!      subroutine set_surf_magne_p_type_id(sf_grp, magne_p)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(potential_surf_bc_type),  intent(inout) :: magne_p
!
!      subroutine set_surf_heat_flux_type_id(sf_grp, temp)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(scaler_surf_bc_type),  intent(inout) :: temp
!      subroutine set_surf_grad_composit_type_id(sf_grp, comp_sf)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(scaler_surf_bc_type),  intent(inout) :: comp_sf
!      subroutine set_surf_grad_press_type_id(sf_grp, press)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(potential_surf_bc_type),  intent(inout) :: press
!      subroutine set_surf_grad_magne_p_type_id(sf_grp, magne_p)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(potential_surf_bc_type),  intent(inout) :: magne_p
!
!      subroutine set_wall_press_type_id(sf_grp, press)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(potential_surf_bc_type),  intent(inout) :: press
!      subroutine set_wall_magne_p_type_id(sf_grp, magne_p)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(potential_surf_bc_type),  intent(inout) :: magne_p
!
      module set_sf_scalar_type_id
!
      use m_precision
!
      use t_group_data
      use t_surface_bc_data
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
      subroutine set_surf_temp_type_id(sf_grp, temp)
!
      use set_surf_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type),  intent(inout) :: temp
!
!
      call s_set_surf_scalar_id(sf_grp%num_grp, sf_grp%grp_name,        &
     &   num_bc_h_flux, bc_h_flux_name, ibc_h_flux_type, name_st,       &
     &   temp%sgs%ngrp_sf_dat, temp%sgs%id_grp_sf_dat)
!
      end subroutine set_surf_temp_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_composit_type_id(sf_grp, comp_sf)
!
      use set_surf_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type),  intent(inout) :: comp_sf
!
!
      call s_set_surf_scalar_id(sf_grp%num_grp, sf_grp%grp_name,        &
     &   num_surf_composition, surf_composit_name, isurf_composit_type, &
     &   name_sds, comp_sf%sgs%ngrp_sf_dat, comp_sf%sgs%id_grp_sf_dat)
!
      end subroutine set_surf_composit_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_press_type_id(sf_grp, press)
!
      use set_surf_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(potential_surf_bc_type),  intent(inout) :: press
!
!
      call s_set_surf_scalar_id(sf_grp%num_grp, sf_grp%grp_name,        &
     &   num_bc_wall, bc_wall_name, ibc_wall_type, name_sp,             &
     &   press%sgs%ngrp_sf_dat, press%sgs%id_grp_sf_dat)
!
      end subroutine set_surf_press_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_magne_p_type_id(sf_grp, magne_p)
!
      use set_surf_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(potential_surf_bc_type),  intent(inout) :: magne_p
!
!
      call s_set_surf_scalar_id(sf_grp%num_grp, sf_grp%grp_name,        &
     &   num_surf_magp, surf_magp_name, isurf_magp_type, name_smp,      &
     &   magne_p%sgs%ngrp_sf_dat, magne_p%sgs%id_grp_sf_dat)
!
      end subroutine set_surf_magne_p_type_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_surf_heat_flux_type_id(sf_grp, temp)
!
      use set_surf_vector_id_type
!
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type),  intent(inout) :: temp
!
!
      call set_sf_grad_scalar_id_type                                   &
     &    (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,          &
     &   num_bc_h_flux, bc_h_flux_name, ibc_h_flux_type,                &
     &   temp%flux%ngrp_sf_fix_fx, temp%flux%id_grp_sf_fix_fx,          &
     &   temp%flux%ist_ele_sf_fix_fx, temp%flux_lead%ngrp_sf_dat,       &
     &   temp%flux_lead%id_grp_sf_dat)
!
      end subroutine set_surf_heat_flux_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_composit_type_id(sf_grp, comp_sf)
!
      use set_surf_vector_id_type
!
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type),  intent(inout) :: comp_sf
!
!
      call set_sf_grad_scalar_id_type                                   &
     &    (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,          &
     &   num_surf_composition, surf_composit_name, isurf_composit_type, &
     &   comp_sf%flux%ngrp_sf_fix_fx, comp_sf%flux%id_grp_sf_fix_fx,    &
     &   comp_sf%flux%ist_ele_sf_fix_fx, comp_sf%flux_lead%ngrp_sf_dat, &
     &   comp_sf%flux_lead%id_grp_sf_dat)
!
      end subroutine set_surf_grad_composit_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_press_type_id(sf_grp, press)
!
      use set_surf_vector_id_type
!
      type(surface_group_data), intent(in) :: sf_grp
      type(potential_surf_bc_type),  intent(inout) :: press
!
!
      call set_sf_grad_scalar_id_type                                   &
     &    (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,          &
     &   num_bc_wall, bc_wall_name, ibc_wall_type,                      &
     &   press%grad%ngrp_sf_fix_fx, press%grad%id_grp_sf_fix_fx,        &
     &   press%grad%ist_ele_sf_fix_fx, press%grad_lead%ngrp_sf_dat,     &
     &   press%grad_lead%id_grp_sf_dat)
!
      end subroutine set_surf_grad_press_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_magne_p_type_id(sf_grp, magne_p)
!
      use set_surf_vector_id_type
!
      type(surface_group_data), intent(in) :: sf_grp
      type(potential_surf_bc_type),  intent(inout) :: magne_p
!
!
      call set_sf_grad_scalar_id_type                                   &
     &    (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,          &
     &   num_surf_magp, surf_magp_name, isurf_magp_type,                &
     &   magne_p%grad%ngrp_sf_fix_fx, magne_p%grad%id_grp_sf_fix_fx,    &
     &   magne_p%grad%ist_ele_sf_fix_fx, magne_p%grad_lead%ngrp_sf_dat, &
     &   magne_p%grad_lead%id_grp_sf_dat)
!
      end subroutine set_surf_grad_magne_p_type_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_wall_press_type_id(sf_grp, press)
!
      use set_wall_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(potential_surf_bc_type),  intent(inout) :: press
!
!
      call s_set_wall_scalar_id(sf_grp%num_grp, sf_grp%grp_name,        &
     &    num_bc_wall, bc_wall_name,                                    &
     &    ibc_wall_type, bc_wall_magnitude,                             &
     &    press%wall%ngrp_sf_dat, press%wall%id_grp_sf_dat,             &
     &    press%sph_in%ngrp_sf_dat, press%sph_in%id_grp_sf_dat,         &
     &    press%sph_out%ngrp_sf_dat, press%sph_out%id_grp_sf_dat)
!
      end subroutine set_wall_press_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_wall_magne_p_type_id(sf_grp, magne_p)
!
      use set_wall_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(potential_surf_bc_type),  intent(inout) :: magne_p
!
!
      call s_set_wall_scalar_id(sf_grp%num_grp, sf_grp%grp_name,        &
     &    num_surf_magp, surf_magp_name,                                &
     &    isurf_magp_type, surf_magp_magnitude,                         &
     &    magne_p%wall%ngrp_sf_dat, magne_p%wall%id_grp_sf_dat,         &
     &    magne_p%sph_in%ngrp_sf_dat, magne_p%sph_in%id_grp_sf_dat,     &
     &    magne_p%sph_out%ngrp_sf_dat, magne_p%sph_out%id_grp_sf_dat)
!
      end subroutine set_wall_magne_p_type_id
!
!-----------------------------------------------------------------------
!
      end module set_sf_scalar_type_id
