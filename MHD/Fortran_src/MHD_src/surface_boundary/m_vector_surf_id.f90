!
!      module m_vector_surf_id
!
!      Written by H. Matsui on Sep. 2005
!
!!      subroutine set_surf_velo_id                                     &
!!     &         (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v)
!!      subroutine set_surf_vect_p_id                                   &
!!     &         (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v)
!!      subroutine set_surf_magne_id                                    &
!!     &         (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v)
!!      subroutine set_surf_current_id                                  &
!!     &         (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v)
!!
!!      subroutine set_surf_torque_id(sf_grp)
!!      subroutine set_surf_grad_vecp_id(sf_grp)
!!      subroutine set_surf_grad_b_id(sf_grp)
!!      subroutine set_surf_grad_j_id(sf_grp)
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
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_velo_id                                       &
     &         (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v)
!
      use m_surf_data_torque
      use m_boundary_condition_IDs
      use set_surf_vector_id
      use set_stress_free_surf_id
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(surface_group_geometry), intent(in) :: sf_grp_v
!
!
      write(*,*) 'torque_surf%ibc_type', torque_surf%ibc_type
      call s_set_surf_vector_id                                         &
     &   (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,                &
     &    torque_surf%num_bc, torque_surf%bc_name,                      &
     &    torque_surf%ibc_type, torque_surf%bc_magnitude, name_svn,     &
     &    sf_sgs1_grad_v, sf_bc1_norm_v)
!
      call s_stress_free_surf_id(sf_grp%num_grp, sf_grp%grp_name,       &
     &    torque_surf%num_bc, torque_surf%bc_name,                      &
     &    torque_surf%ibc_type,                                         &
     &    iflag_surf_free_sph_in, iflag_surf_free_sph_out,              &
     &    sf_bc1_free_sph_in%ngrp_sf_dat,                               &
     &    sf_bc1_free_sph_out%ngrp_sf_dat,                              &
     &    sf_bc1_free_sph_in%id_grp_sf_dat,                             &
     &    sf_bc1_free_sph_out%id_grp_sf_dat)
!
      end subroutine set_surf_velo_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_vect_p_id                                     &
     &         (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v)
!
      use m_surf_data_vector_p
      use m_boundary_condition_IDs
      use set_surf_vector_id
      use set_stress_free_surf_id
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(surface_group_geometry), intent(in) :: sf_grp_v
!
!
      call s_set_surf_vector_id                                         &
     &   (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,                &
     &    a_potential_surf%num_bc, a_potential_surf%bc_name,            &
     &    a_potential_surf%ibc_type, a_potential_surf%bc_magnitude,     &
     &    name_san, sf_sgs1_grad_a, sf_bc1_norm_a)
!
      call s_stress_free_surf_id(sf_grp%num_grp, sf_grp%grp_name,       &
     &    a_potential_surf%num_bc, a_potential_surf%bc_name,            &
     &    a_potential_surf%ibc_type,                                    &
     &    iflag_surf_qvc_sph_in, iflag_surf_qvc_sph_out,                &
     &    sf_bc1_pvc_in_a%ngrp_sf_dat, sf_bc1_pvc_out_a%ngrp_sf_dat,    &
     &    sf_bc1_pvc_in_a%id_grp_sf_dat,                                &
     &    sf_bc1_pvc_out_a%id_grp_sf_dat)
!
      end subroutine set_surf_vect_p_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_magne_id                                      &
     &         (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v)
!
      use m_surf_data_magne
      use set_surf_vector_id
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(surface_group_geometry), intent(in) :: sf_grp_v
!
!
      call s_set_surf_vector_id                                         &
     &   (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,                &
     &    magne_surf%num_bc, magne_surf%bc_name,                        &
     &    magne_surf%ibc_type, magne_surf%bc_magnitude, name_sbn,       &
     &    sf_sgs1_grad_b, sf_bc1_norm_b)
!
      end subroutine set_surf_magne_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_current_id                                    &
     &         (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v)
!
      use m_surf_data_current
      use set_surf_vector_id
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(surface_group_geometry), intent(in) :: sf_grp_v
!
!
      call s_set_surf_vector_id                                         &
     &   (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,                &
     &    current_surf%num_bc, current_surf%bc_name,                    &
     &    current_surf%ibc_type, current_surf%bc_magnitude, name_sjn,   &
     &    sf_sgs1_grad_j, sf_bc1_norm_j)
!
      end subroutine set_surf_current_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_surf_torque_id(sf_grp)
!
      use m_surf_data_torque
      use set_sf_grad_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_set_sf_grad_vector_id(sf_grp,                              &
     &    torque_surf%num_bc, torque_surf%bc_name,                      &
     &    torque_surf%ibc_type, torque_surf%bc_magnitude,               &
     &    name_vxg, name_vyg, name_vzg,                                 &
     &    sf_bc1_grad_v, sf_bc1_lead_tq)
!
      end subroutine set_surf_torque_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_vecp_id(sf_grp)
!
      use m_surf_data_vector_p
      use set_sf_grad_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_set_sf_grad_vector_id(sf_grp,                              &
     &    a_potential_surf%num_bc, a_potential_surf%bc_name,            &
     &    a_potential_surf%ibc_type, a_potential_surf%bc_magnitude,     &
     &    name_axg, name_ayg, name_azg,                                 &
     &    sf_bc1_grad_a, sf_bc1_lead_a)
!
      end subroutine set_surf_grad_vecp_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_b_id(sf_grp)
!
      use m_surf_data_magne
      use set_sf_grad_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_set_sf_grad_vector_id(sf_grp,                              &
     &    magne_surf%num_bc, magne_surf%bc_name,                        &
     &    magne_surf%ibc_type, magne_surf%bc_magnitude,                 &
     &    name_bxg, name_byg, name_bzg,                                 &
     &    sf_bc1_grad_b, sf_bc1_lead_b)
!
      end subroutine set_surf_grad_b_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_j_id(sf_grp)
!
      use m_surf_data_current
      use set_sf_grad_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call s_set_sf_grad_vector_id(sf_grp,                              &
     &    current_surf%num_bc, current_surf%bc_name,                    &
     &    current_surf%ibc_type, current_surf%bc_magnitude,             &
     &    name_sjx, name_sjy, name_sjz,                                 &
     &    sf_bc1_grad_j, sf_bc1_lead_j)
!
      end subroutine set_surf_grad_j_id
!
!-----------------------------------------------------------------------
!
      end module m_vector_surf_id
