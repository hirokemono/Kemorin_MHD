!>@file   t_surface_bc_velocity.f90
!!@brief  module t_surface_bc_velocity
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in Sep. 2005
!
!>    @brief flux boundary condition lists for vector
!!
!!@verbatim
!!      subroutine set_surf_grad_velo(name_norm, name_grad,           &
!!     &         IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,&
!!     &         vector_surf, Vsf_bcs)
!!      subroutine dealloc_surf_data_velo(Vsf_bcs)
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(surface_group_normals), intent(in) :: sf_grp_v
!!        type(boundary_condition_list), intent(in) :: vector_surf
!!        type(velocity_surf_bc_type), intent(inout) :: Vsf_bcs
!!@endverbatim
!
      module t_surface_bc_velocity
!
      use m_precision
!
      use m_header_4_surface_bc
      use t_surface_bc_data
      use t_surface_bc_vector
!
      implicit  none
!
      type velocity_surf_bc_type
        type(scaler_surf_bc_data_type), allocatable :: sgs(:)
        type(scaler_surf_flux_bc_type) :: normal
        type(scaler_surf_flux_bc_type), allocatable :: grad(:)
        type(scaler_surf_bc_data_type), allocatable :: torque_lead(:)
        type(scaler_surf_bc_data_type) :: free_sph_in
        type(scaler_surf_bc_data_type) :: free_sph_out
      end type velocity_surf_bc_type
!
      private :: alloc_surf_data_velo_num, alloc_surf_data_velo
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_velo(name_norm, name_grad,               &
     &          IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,   &
     &          vector_surf, Vsf_bcs)
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_surface_group_normals
      use t_boundary_field_IO
      use t_bc_data_list
!
      use set_surf_vector_id
      use set_stress_free_surf_id
      use set_sf_grad_vector_id
!
      character(len=kchara), intent(in) :: name_norm
      character(len=kchara), intent(in) :: name_grad(3)
      type(IO_boundary), intent(in) :: IO_bc
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(surface_group_normals), intent(in) :: sf_grp_v
      type(boundary_condition_list), intent(in) :: vector_surf
!
      type(velocity_surf_bc_type), intent(inout) :: Vsf_bcs
!
!
      call alloc_surf_data_velo_num(Vsf_bcs)
!
      call s_count_num_surf_vector                                      &
     &   (IO_bc, sf_grp, sf_grp_nod, vector_surf%num_bc,                &
     &    vector_surf%bc_name, vector_surf%ibc_type, name_norm,         &
     &    Vsf_bcs%sgs, Vsf_bcs%normal)
!
      call count_num_stress_free_surf                                   &
     &   (sf_grp%num_grp, sf_grp%grp_name, vector_surf%num_bc,          &
     &    vector_surf%bc_name, vector_surf%ibc_type,                    &
     &    iflag_surf_free_sph_in, iflag_surf_free_sph_out,              &
     &    Vsf_bcs%free_sph_in%ngrp_sf_dat,                              &
     &    Vsf_bcs%free_sph_out%ngrp_sf_dat)
!
      call count_num_sf_grad_vector                                     &
     &   (IO_bc, sf_grp, vector_surf%num_bc, vector_surf%bc_name,       &
     &    vector_surf%ibc_type, name_grad,                              &
     &    Vsf_bcs%grad, Vsf_bcs%torque_lead)
!
!
      call alloc_surf_data_velo(Vsf_bcs)
!
      call s_set_surf_vector_id                                         &
     &   (IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,         &
     &    vector_surf%num_bc, vector_surf%bc_name,                      &
     &    vector_surf%ibc_type, vector_surf%bc_magnitude, name_norm,    &
     &    Vsf_bcs%sgs, Vsf_bcs%normal)
!
      call s_stress_free_surf_id                                        &
     &   (sf_grp%num_grp, sf_grp%grp_name, vector_surf%num_bc,          &
     &    vector_surf%bc_name, vector_surf%ibc_type,                    &
     &    iflag_surf_free_sph_in, iflag_surf_free_sph_out,              &
     &    Vsf_bcs%free_sph_in%ngrp_sf_dat,                              &
     &    Vsf_bcs%free_sph_out%ngrp_sf_dat,                             &
     &    Vsf_bcs%free_sph_in%id_grp_sf_dat,                            &
     &    Vsf_bcs%free_sph_out%id_grp_sf_dat)
!
      call s_set_sf_grad_vector_id(IO_bc, sf_grp,                       &
     &    vector_surf%num_bc, vector_surf%bc_name,                      &
     &    vector_surf%ibc_type, vector_surf%bc_magnitude,               &
     &    name_grad, Vsf_bcs%grad, Vsf_bcs%torque_lead)
!
      end subroutine set_surf_grad_velo
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_data_velo(Vsf_bcs)
!
      type(velocity_surf_bc_type),  intent(inout) :: Vsf_bcs
!
!
      call dealloc_surf_vector_apt(Vsf_bcs%grad)
      call dealloc_surf_scaler_type(Vsf_bcs%normal)
      call dealloc_surf_vector_dat(Vsf_bcs%sgs)
      call dealloc_surf_vector_dat(Vsf_bcs%torque_lead)
      call dealloc_surf_scaler_dat_type(Vsf_bcs%free_sph_in)
      call dealloc_surf_scaler_dat_type(Vsf_bcs%free_sph_out)
!
      deallocate(Vsf_bcs%sgs, Vsf_bcs%grad, Vsf_bcs%torque_lead)
!
      end subroutine dealloc_surf_data_velo
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_data_velo_num(Vsf_bcs)
!
      type(velocity_surf_bc_type),  intent(inout) :: Vsf_bcs
!
!
      allocate(Vsf_bcs%sgs(3))
      allocate(Vsf_bcs%grad(3))
      allocate(Vsf_bcs%torque_lead(3))
!
      end subroutine alloc_surf_data_velo_num
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_data_velo(Vsf_bcs)
!
      type(velocity_surf_bc_type),  intent(inout) :: Vsf_bcs
!
!
      call alloc_surf_vector_num(Vsf_bcs%grad)
      call alloc_surf_scaler_num(Vsf_bcs%normal)
      call alloc_surf_vector_dat(Vsf_bcs%sgs)
      call alloc_surf_vector_dat(Vsf_bcs%torque_lead)
      call alloc_surf_scaler_dat_type(Vsf_bcs%free_sph_in)
      call alloc_surf_scaler_dat_type(Vsf_bcs%free_sph_out)
!
      call alloc_surf_vector_apt(Vsf_bcs%grad)
      call alloc_surf_scaler_apt(Vsf_bcs%normal)
!
      end subroutine alloc_surf_data_velo
!
!  ---------------------------------------------------------------------
!
      end module t_surface_bc_velocity
