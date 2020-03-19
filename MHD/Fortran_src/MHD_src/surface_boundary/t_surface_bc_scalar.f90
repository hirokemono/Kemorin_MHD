!>@file   t_surface_bc_scalar.f90
!!@brief  module t_surface_bc_scalar
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2005
!
!>    @brief flux boundary condition lists for scalar field
!!
!!@verbatim
!!      subroutine set_surf_grad_scalar_id                              &
!!     &         (IO_bc, sf_grp, scalar_surf, Ssf_bcs)
!!      subroutine dealloc_surf_data_scalar(Ssf_bcs)
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(boundary_condition_list), intent(in) :: scalar_surf
!!        type(scaler_surf_bc_type),  intent(inout) :: Ssf_bcs
!!      subroutine set_wall_potential_id                                &
!!     &         (name_grad, IO_bc, sf_grp, potential_surf, Psf_bcs)
!!      subroutine dealloc_surf_potential(Psf_bcs)
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(boundary_condition_list), intent(in) :: potential_surf
!!        type(potential_surf_bc_type),  intent(inout) :: Psf_bcs
!!@endverbatim
!
      module t_surface_bc_scalar
!
      use m_precision
!
      use m_header_4_surface_bc
      use t_group_data
      use t_surface_bc_data
      use t_boundary_field_IO
      use t_bc_data_list
!
      implicit  none
!
!
      type potential_surf_bc_type
        type(scaler_surf_bc_data_type) :: sgs
        type(scaler_surf_flux_bc_type) :: grad
        type(scaler_surf_bc_data_type) :: grad_lead
        type(scaler_surf_bc_data_type) :: wall
        type(scaler_surf_bc_data_type) :: sph_in
        type(scaler_surf_bc_data_type) :: sph_out
      end type potential_surf_bc_type
!
      type scaler_surf_bc_type
        type(scaler_surf_bc_data_type) :: sgs
        type(scaler_surf_flux_bc_type) :: flux
        type(scaler_surf_bc_data_type) :: flux_lead
      end type scaler_surf_bc_type
!
      private :: alloc_surf_data_scalar, alloc_surf_potential
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_scalar_id                                &
     &         (name_grad, IO_bc, sf_grp, scalar_surf, Ssf_bcs)
!
      use set_surf_scalar_id
      use set_sf_grad_scalar_id
!
      character(len=kchara), intent(in) :: name_grad
      type(IO_boundary), intent(in) :: IO_bc
      type(surface_group_data), intent(in) :: sf_grp
      type(boundary_condition_list), intent(in) :: scalar_surf
      type(scaler_surf_bc_type),  intent(inout) :: Ssf_bcs
!
!
      call s_count_num_surf_scalar(sf_grp%num_grp, sf_grp%grp_name,     &
     &    scalar_surf%num_bc, scalar_surf%bc_name,                      &
     &    scalar_surf%ibc_type, Ssf_bcs%sgs%ngrp_sf_dat)
      call count_num_surf_grad_scalar                                   &
     &    (IO_bc, sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,   &
     &     scalar_surf%num_bc, scalar_surf%bc_name,                     &
     &     scalar_surf%ibc_type, name_grad,                             &
     &     Ssf_bcs%flux%ngrp_sf_fix_fx, Ssf_bcs%flux%nitem_sf_fix_fx,   &
     &     Ssf_bcs%flux_lead%ngrp_sf_dat)
!
      call alloc_surf_data_scalar(Ssf_bcs)
!
      call s_set_surf_scalar_id(sf_grp%num_grp, sf_grp%grp_name,        &
     &   scalar_surf%num_bc, scalar_surf%bc_name, scalar_surf%ibc_type, &
     &   Ssf_bcs%sgs%ngrp_sf_dat, Ssf_bcs%sgs%id_grp_sf_dat)
!
      call s_set_surf_grad_scalar_id(IO_bc, sf_grp,                     &
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
!
      subroutine dealloc_surf_data_scalar(Ssf_bcs)
!
      type(scaler_surf_bc_type), intent(inout) :: Ssf_bcs
!
!
      call dealloc_surf_scaler_type(Ssf_bcs%flux)
      call dealloc_surf_scaler_dat_type(Ssf_bcs%sgs)
      call dealloc_surf_scaler_dat_type(Ssf_bcs%flux_lead)
!
      end subroutine dealloc_surf_data_scalar
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_wall_potential_id                                  &
     &         (name_grad, IO_bc, sf_grp, potential_surf, Psf_bcs)
!
      use set_surf_scalar_id
      use set_sf_grad_scalar_id
      use set_wall_scalar_id
!
      character(len=kchara), intent(in) :: name_grad
      type(IO_boundary), intent(in) :: IO_bc
      type(surface_group_data), intent(in) :: sf_grp
      type(boundary_condition_list), intent(in) :: potential_surf
      type(potential_surf_bc_type),  intent(inout) :: Psf_bcs
!
!
      call s_count_num_surf_scalar(sf_grp%num_grp, sf_grp%grp_name,     &
     &    potential_surf%num_bc, potential_surf%bc_name,                &
     &    potential_surf%ibc_type, Psf_bcs%sgs%ngrp_sf_dat)
      call count_num_surf_grad_scalar                                   &
     &    (IO_bc, sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,   &
     &     potential_surf%num_bc, potential_surf%bc_name,               &
     &     potential_surf%ibc_type, name_grad,                          &
     &     Psf_bcs%grad%ngrp_sf_fix_fx, Psf_bcs%grad%nitem_sf_fix_fx,   &
     &     Psf_bcs%grad_lead%ngrp_sf_dat)
!
      call count_num_wall_surf(sf_grp%num_grp, sf_grp%grp_name,         &
     &    potential_surf%num_bc, potential_surf%bc_name,                &
     &    potential_surf%ibc_type, Psf_bcs%wall%ngrp_sf_dat,            &
     &    Psf_bcs%sph_in%ngrp_sf_dat, Psf_bcs%sph_out%ngrp_sf_dat)
!
!
      call alloc_surf_potential(Psf_bcs)
!
      call s_set_surf_scalar_id(sf_grp%num_grp, sf_grp%grp_name,        &
     &   potential_surf%num_bc, potential_surf%bc_name,                 &
     &   potential_surf%ibc_type, Psf_bcs%sgs%ngrp_sf_dat,              &
     &   Psf_bcs%sgs%id_grp_sf_dat)
!
      call s_set_surf_grad_scalar_id(IO_bc, sf_grp,                     &
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
      subroutine dealloc_surf_potential(Psf_bcs)
!
      type(potential_surf_bc_type), intent(inout) :: Psf_bcs
!
!
      call dealloc_surf_scaler_dat_type(Psf_bcs%sgs)
      call dealloc_surf_scaler_dat_type(Psf_bcs%grad_lead)
!
      call dealloc_surf_scaler_type(Psf_bcs%grad)
      call dealloc_surf_scaler_dat_type(Psf_bcs%wall)
      call dealloc_surf_scaler_dat_type(Psf_bcs%sph_in)
      call dealloc_surf_scaler_dat_type(Psf_bcs%sph_out)
!
      end subroutine dealloc_surf_potential
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_data_scalar(Ssf_bcs)
!
      type(scaler_surf_bc_type), intent(inout) :: Ssf_bcs
!
!
      call alloc_surf_scaler_num(Ssf_bcs%flux)
      call alloc_surf_scaler_dat_type(Ssf_bcs%sgs)
      call alloc_surf_scaler_dat_type(Ssf_bcs%flux_lead)
!
      call alloc_surf_scaler_apt(Ssf_bcs%flux)
!
      end subroutine alloc_surf_data_scalar
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_potential(Psf_bcs)
!
      type(potential_surf_bc_type), intent(inout) :: Psf_bcs
!
!
      call alloc_surf_scaler_dat_type(Psf_bcs%sgs)
      call alloc_surf_scaler_dat_type(Psf_bcs%grad_lead)
!
      call alloc_surf_scaler_num(Psf_bcs%grad)
      call alloc_surf_scaler_dat_type(Psf_bcs%wall)
      call alloc_surf_scaler_dat_type(Psf_bcs%sph_in)
      call alloc_surf_scaler_dat_type(Psf_bcs%sph_out)
!
      call alloc_surf_scaler_apt(Psf_bcs%grad)
!
       end subroutine alloc_surf_potential
!
!-----------------------------------------------------------------------
!
      end module t_surface_bc_scalar
