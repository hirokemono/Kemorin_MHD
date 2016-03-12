!
!      module m_scalar_surf_id
!
!      Written by H. Matsui on Sep. 2005
!
!!      subroutine count_num_surf_gradient                              &
!!     &         (name_grad, sf_grp, scalar_surf, Ssf_bcs)
!!      subroutine count_num_wall_potential                             &
!!     &         (name_grad, sf_grp, potential_surf, Psf_bcs)
!!
!!      subroutine set_surf_grad_scalar_id(sf_grp, scalar_surf, Ssf_bcs)
!!      subroutine set_wall_potential_id(sf_grp, potential_surf, Psf_bcs)
!
      module m_scalar_surf_id
!
      use m_precision
!
      use m_surf_data_list
      use m_header_4_surface_bc
      use t_group_data
      use t_surface_bc_data
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_gradient                                &
     &         (name_grad, sf_grp, scalar_surf, Ssf_bcs)
!
      use set_surf_scalar_id
      use set_sf_grad_scalar_id
!
      character(len=kchara), intent(in) :: name_grad
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_bc_list_type), intent(in) :: scalar_surf
      type(scaler_surf_bc_type),  intent(inout) :: Ssf_bcs
!
!
      call s_count_num_surf_scalar(sf_grp%num_grp, sf_grp%grp_name,     &
     &    scalar_surf%num_bc, scalar_surf%bc_name,                      &
     &    scalar_surf%ibc_type, Ssf_bcs%sgs%ngrp_sf_dat)
      call count_num_surf_grad_scalar                                   &
     &    (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,          &
     &     scalar_surf%num_bc, scalar_surf%bc_name,                     &
     &     scalar_surf%ibc_type, name_grad,                             &
     &     Ssf_bcs%flux%ngrp_sf_fix_fx, Ssf_bcs%flux%nitem_sf_fix_fx,   &
     &     Ssf_bcs%flux_lead%ngrp_sf_dat)
!
      end subroutine count_num_surf_gradient
!
!-----------------------------------------------------------------------
!
      subroutine count_num_wall_potential                               &
     &         (name_grad, sf_grp, potential_surf, Psf_bcs)
!
      use set_surf_scalar_id
      use set_sf_grad_scalar_id
      use set_wall_scalar_id
!
      character(len=kchara), intent(in) :: name_grad
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_bc_list_type), intent(in) :: potential_surf
      type(potential_surf_bc_type),  intent(inout) :: Psf_bcs
!
!
      call s_count_num_surf_scalar(sf_grp%num_grp, sf_grp%grp_name,     &
     &    potential_surf%num_bc, potential_surf%bc_name,                &
     &    potential_surf%ibc_type, Psf_bcs%sgs%ngrp_sf_dat)
      call count_num_surf_grad_scalar                                   &
     &    (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,          &
     &     potential_surf%num_bc, potential_surf%bc_name,               &
     &     potential_surf%ibc_type, name_grad,                          &
     &     Psf_bcs%grad%ngrp_sf_fix_fx, Psf_bcs%grad%nitem_sf_fix_fx,   &
     &     Psf_bcs%grad_lead%ngrp_sf_dat)
!
      call count_num_wall_surf(sf_grp%num_grp, sf_grp%grp_name,         &
     &   potential_surf%num_bc, potential_surf%bc_name,                 &
     &   potential_surf%ibc_type, Psf_bcs%wall%ngrp_sf_dat,             &
     &   Psf_bcs%sph_in%ngrp_sf_dat, Psf_bcs%sph_out%ngrp_sf_dat)
!
      end subroutine count_num_wall_potential
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_scalar_id(sf_grp, scalar_surf, Ssf_bcs)
!
      use set_surf_scalar_id
      use set_sf_grad_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_bc_list_type), intent(in) :: scalar_surf
      type(scaler_surf_bc_type),  intent(inout) :: Ssf_bcs
!
!
      call s_set_surf_scalar_id(sf_grp%num_grp, sf_grp%grp_name,        &
     &   scalar_surf%num_bc, scalar_surf%bc_name, scalar_surf%ibc_type, &
     &   Ssf_bcs%sgs%ngrp_sf_dat, Ssf_bcs%sgs%id_grp_sf_dat)
!
      call s_set_surf_grad_scalar_id(sf_grp,                            &
     &   scalar_surf%num_bc, scalar_surf%bc_name,                       &
     &   scalar_surf%ibc_type, scalar_surf%bc_magnitude, name_dsg,      &
     &   Ssf_bcs%flux%ngrp_sf_fix_fx, Ssf_bcs%flux%id_grp_sf_fix_fx,    &
     &   Ssf_bcs%flux%nitem_sf_fix_fx,                                  &
     &   Ssf_bcs%flux%ist_ele_sf_fix_fx, Ssf_bcs%flux%sf_apt_fix_fx,    &
     &   Ssf_bcs%flux_lead%ngrp_sf_dat,                                 &
     &   Ssf_bcs%flux_lead%id_grp_sf_dat)
!
      end subroutine set_surf_grad_scalar_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_wall_potential_id(sf_grp, potential_surf, Psf_bcs)
!
      use set_surf_scalar_id
      use set_sf_grad_scalar_id
      use set_wall_scalar_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_bc_list_type), intent(in) :: potential_surf
      type(potential_surf_bc_type),  intent(inout) :: Psf_bcs
!
!
      call s_set_surf_scalar_id(sf_grp%num_grp, sf_grp%grp_name,        &
     &   potential_surf%num_bc, potential_surf%bc_name,                 &
     &   potential_surf%ibc_type, Psf_bcs%sgs%ngrp_sf_dat,              &
     &   Psf_bcs%sgs%id_grp_sf_dat)
!
      call s_set_surf_grad_scalar_id(sf_grp,                            &
     &    potential_surf%num_bc, potential_surf%bc_name,                &
     &    potential_surf%ibc_type, potential_surf%bc_magnitude,         &
     &    name_pg, Psf_bcs%grad%ngrp_sf_fix_fx,                         &
     &    Psf_bcs%grad%id_grp_sf_fix_fx, Psf_bcs%grad%nitem_sf_fix_fx,  &
     &    Psf_bcs%grad%ist_ele_sf_fix_fx, Psf_bcs%grad%sf_apt_fix_fx,   &
     &    Psf_bcs%grad_lead%ngrp_sf_dat,                                &
     &    Psf_bcs%grad_lead%id_grp_sf_dat)
!
      call s_set_wall_scalar_id                                        &
     &   (sf_grp%num_grp, sf_grp%grp_name, potential_surf%num_bc,      &
     &    potential_surf%bc_name, potential_surf%ibc_type,             &
     &    Psf_bcs%wall%ngrp_sf_dat, Psf_bcs%wall%id_grp_sf_dat,        &
     &    Psf_bcs%sph_in%ngrp_sf_dat, Psf_bcs%sph_in%id_grp_sf_dat,    &
     &    Psf_bcs%sph_out%ngrp_sf_dat, Psf_bcs%sph_out%id_grp_sf_dat)
!
      end subroutine set_wall_potential_id
!
!-----------------------------------------------------------------------
!
      end module m_scalar_surf_id
