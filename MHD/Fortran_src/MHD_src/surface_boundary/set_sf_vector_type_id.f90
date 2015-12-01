!
!      module set_sf_vector_type_id
!       (module m_vector_surf_id)
!
!      Written by H. Matsui on Sep. 2005
!
!      subroutine set_surf_velo_type_id(sf_grp, sf_nod, velo)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surface_node_grp_data), intent(in) :: sf_nod
!        type(velocity_surf_bc_type), intent(inout) :: velo
!      subroutine set_surf_vect_p_type_id(sf_grp, sf_nod, vector_p)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surface_node_grp_data), intent(in) :: sf_nod
!        type(velocity_surf_bc_type), intent(inout) :: vector_p
!      subroutine set_surf_magne_type_id(sf_grp, sf_nod, magne)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surface_node_grp_data), intent(in) :: sf_nod
!        type(vector_surf_bc_type), intent(inout) :: magne
!      subroutine set_surf_current_type_id(sf_grp, sf_nod, current)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surface_node_grp_data), intent(in) :: sf_nod
!        type(vector_surf_bc_type), intent(inout) :: current
!
!      subroutine set_surf_torque_type_id(sf_grp, velo)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(velocity_surf_bc_type), intent(inout) :: velo
!      subroutine set_surf_grad_vecp_type_id(sf_grp, vector_p)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(velocity_surf_bc_type), intent(inout) :: vector_p
!      subroutine set_surf_grad_b_type_id(sf_grp, magne)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(vector_surf_bc_type), intent(inout) :: magne
!      subroutine set_surf_grad_j_type_id(sf_grp, current)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(vector_surf_bc_type), intent(inout) :: current
!
      module set_sf_vector_type_id
!
      use m_precision
!
      use t_group_data
      use t_surface_group_connect
      use t_surface_bc_data
      use m_surf_data_list
      use m_header_4_surface_bc
      use set_surf_vector_id_type
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_velo_type_id(sf_grp, sf_nod, velo)
!
      use m_boundary_condition_IDs
      use set_stress_free_surf_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(velocity_surf_bc_type), intent(inout) :: velo
!
!
      call s_set_surf_vector_id_type                                    &
     &   (sf_grp, sf_nod%inod_stack_sf_grp,                             &
     &    torque_surf%num_bc, torque_surf%bc_name,                      &
     &    torque_surf%ibc_type, velo%sgs, velo%normal)
!
      call s_stress_free_surf_id(sf_grp%num_grp, sf_grp%grp_name,       &
     &    torque_surf%num_bc, torque_surf%bc_name,                      &
     &    torque_surf%ibc_type,                                         &
     &    iflag_surf_free_sph_in, iflag_surf_free_sph_out,              &
     &    velo%free_sph_in%ngrp_sf_dat, velo%free_sph_out%ngrp_sf_dat,  &
     &    velo%free_sph_in%id_grp_sf_dat,                               &
     &    velo%free_sph_out%id_grp_sf_dat)
!
      end subroutine set_surf_velo_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_vect_p_type_id(sf_grp, sf_nod, vector_p)
!
      use m_boundary_condition_IDs
      use set_stress_free_surf_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(velocity_surf_bc_type), intent(inout) :: vector_p
!
!
      call s_set_surf_vector_id_type                                    &
     &    (sf_grp, sf_nod%inod_stack_sf_grp,                            &
     &     a_potential_surf%num_bc, a_potential_surf%bc_name,           &
     &     a_potential_surf%ibc_type, vector_p%sgs, vector_p%normal)
!
      call s_stress_free_surf_id(sf_grp%num_grp, sf_grp%grp_name,       &
     &    torque_surf%num_bc, torque_surf%bc_name,                      &
     &    torque_surf%ibc_type,                                         &
     &    iflag_surf_qvc_sph_in, iflag_surf_qvc_sph_out,                &
     &    vector_p%free_sph_in%ngrp_sf_dat,                             &
     &    vector_p%free_sph_out%ngrp_sf_dat,                            &
     &    vector_p%free_sph_in%id_grp_sf_dat,                           &
     &    vector_p%free_sph_out%id_grp_sf_dat)
!
      end subroutine set_surf_vect_p_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_magne_type_id(sf_grp, sf_nod, magne)
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(vector_surf_bc_type), intent(inout) :: magne
!
!
      call s_set_surf_vector_id_type                                    &
     &    (sf_grp, sf_nod%inod_stack_sf_grp,                            &
     &     magne_surf%num_bc, magne_surf%bc_name, magne_surf%ibc_type,  &
     &     magne%sgs, magne%normal)
!
      end subroutine set_surf_magne_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_current_type_id(sf_grp, sf_nod, current)
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(vector_surf_bc_type), intent(inout) :: current
!
!
      call s_set_surf_vector_id_type                                    &
     &   (sf_grp, sf_nod%inod_stack_sf_grp,                             &
     &    current_surf%num_bc, current_surf%bc_name,                    &
     &    current_surf%ibc_type, current%sgs, current%normal)
!
      end subroutine set_surf_current_type_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_surf_torque_type_id(sf_grp, velo)
!
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(inout) :: velo
!
!
      call set_sf_grad_vector_id_type                                   &
     &   (sf_grp, torque_surf%num_bc, torque_surf%bc_name,              &
     &    torque_surf%ibc_type, velo%grad, velo%torque_lead)
!
      end subroutine set_surf_torque_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_vecp_type_id(sf_grp, vect_p)
!
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(inout) :: vect_p
!
!
      call set_sf_grad_vector_id_type                                   &
     &   (sf_grp, a_potential_surf%num_bc, a_potential_surf%bc_name,    &
     &    a_potential_surf%ibc_type, vect_p%grad, vect_p%torque_lead)
!
      end subroutine set_surf_grad_vecp_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_b_type_id(sf_grp, magne)
!
      type(surface_group_data), intent(in) :: sf_grp
      type(vector_surf_bc_type), intent(inout) :: magne
!
!
      call set_sf_grad_vector_id_type                                   &
     &    (sf_grp, magne_surf%num_bc, magne_surf%bc_name,               &
     &     magne_surf%ibc_type, magne%grad, magne%torque_lead)
!
      end subroutine set_surf_grad_b_type_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_j_type_id(sf_grp, current)
!
      type(surface_group_data), intent(in) :: sf_grp
      type(vector_surf_bc_type), intent(inout) :: current
!
!
      call set_sf_grad_vector_id_type                                   &
     &   (sf_grp, current_surf%num_bc, current_surf%bc_name,            &
     &    current_surf%ibc_type, current%grad, current%torque_lead)
!
      end subroutine set_surf_grad_j_type_id
!
!-----------------------------------------------------------------------
!
      end module set_sf_vector_type_id
