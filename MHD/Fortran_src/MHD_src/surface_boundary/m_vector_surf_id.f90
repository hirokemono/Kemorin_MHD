!
!      module m_vector_surf_id
!
!      Written by H. Matsui on Sep. 2005
!
!!      subroutine count_num_surf_grad_velo(name_norm, name_grad,       &
!!     &          sf_grp, sf_nod, vector_surf, Vsf_bcs)
!!      subroutine count_num_surf_grad_vector(name_norm, name_grad,     &
!!     &          sf_grp, sf_nod, vector_surf, Bsf_bcs)
!!
!!      subroutine set_surf_grad_velo(name_norm, name_grad,             &
!!     &          node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,        &
!!     &          vector_surf, Vsf_bcs)
!!      subroutine set_surf_grad_vector(name_norm, name_grad,           &
!!     &          node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,        &
!!     &          vector_surf, Bsf_bcs)
!
      module m_vector_surf_id
!
      use m_precision
!
      use m_surf_data_list
      use m_header_4_surface_bc
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_surface_group_geometry
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
      subroutine count_num_surf_grad_velo(name_norm, name_grad,         &
     &          sf_grp, sf_nod, vector_surf, Vsf_bcs)
!
      use set_surf_vector_id
      use set_sf_grad_vector_id
      use set_stress_free_surf_id
!
      character(len=kchara), intent(in) :: name_norm
      character(len=kchara), intent(in) :: name_grad(3)
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(surface_bc_list_type), intent(in) :: vector_surf
      type(velocity_surf_bc_type), intent(inout) :: Vsf_bcs
!
!
      call s_count_num_surf_vector(sf_grp, sf_nod, vector_surf%num_bc,  &
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
     &    (sf_grp, vector_surf%num_bc, vector_surf%bc_name,             &
     &     vector_surf%ibc_type, name_grad,                             &
     &     Vsf_bcs%grad, Vsf_bcs%torque_lead)
!
      end subroutine count_num_surf_grad_velo
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_grad_vector(name_norm, name_grad,       &
     &          sf_grp, sf_nod, vector_surf, Bsf_bcs)
!
      use set_surf_vector_id
      use set_sf_grad_vector_id
!
      character(len=kchara), intent(in) :: name_norm
      character(len=kchara), intent(in) :: name_grad(3)
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(surface_bc_list_type), intent(in) :: vector_surf
      type(vector_surf_bc_type), intent(inout) :: Bsf_bcs
!
!
      call s_count_num_surf_vector(sf_grp, sf_nod,                      &
     &   vector_surf%num_bc, vector_surf%bc_name, vector_surf%ibc_type, &
     &   name_norm, Bsf_bcs%sgs, Bsf_bcs%normal)
      call count_num_sf_grad_vector                                     &
     &    (sf_grp, vector_surf%num_bc, vector_surf%bc_name,             &
     &     vector_surf%ibc_type, name_grad,                             &
     &     Bsf_bcs%grad, Bsf_bcs%torque_lead)
!
      end subroutine count_num_surf_grad_vector
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_velo(name_norm, name_grad,               &
     &          node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,          &
     &          vector_surf, Vsf_bcs)
!
      use set_surf_vector_id
      use set_stress_free_surf_id
      use set_sf_grad_vector_id
!
      character(len=kchara), intent(in) :: name_norm
      character(len=kchara), intent(in) :: name_grad(3)
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(surface_group_geometry), intent(in) :: sf_grp_v
      type(surface_bc_list_type), intent(in) :: vector_surf
!
      type(velocity_surf_bc_type), intent(inout) :: Vsf_bcs
!
!
      call s_set_surf_vector_id                                         &
     &   (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,                &
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
      call s_set_sf_grad_vector_id(sf_grp,                              &
     &    a_potential_surf%num_bc, a_potential_surf%bc_name,            &
     &    a_potential_surf%ibc_type, a_potential_surf%bc_magnitude,     &
     &    name_grad, Vsf_bcs%grad, Vsf_bcs%torque_lead)
!
      end subroutine set_surf_grad_velo
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_vector(name_norm, name_grad,             &
     &          node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,          &
     &          vector_surf, Bsf_bcs)
!
      use set_surf_vector_id
      use set_sf_grad_vector_id
!
      character(len=kchara), intent(in) :: name_norm
      character(len=kchara), intent(in) :: name_grad(3)
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(surface_group_geometry), intent(in) :: sf_grp_v
      type(surface_bc_list_type), intent(in) :: vector_surf
      type(vector_surf_bc_type), intent(inout) :: Bsf_bcs
!
!
      call s_set_surf_vector_id                                         &
     &   (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,                &
     &    vector_surf%num_bc, vector_surf%bc_name,                      &
     &    vector_surf%ibc_type, vector_surf%bc_magnitude, name_norm,    &
     &    Bsf_bcs%sgs, Bsf_bcs%normal)
!
      call s_set_sf_grad_vector_id(sf_grp,                              &
     &    vector_surf%num_bc, vector_surf%bc_name,                      &
     &    vector_surf%ibc_type, vector_surf%bc_magnitude,               &
     &    name_grad, Bsf_bcs%grad, Bsf_bcs%torque_lead)
!
      end subroutine set_surf_grad_vector
!
!-----------------------------------------------------------------------
!
      end module m_vector_surf_id
