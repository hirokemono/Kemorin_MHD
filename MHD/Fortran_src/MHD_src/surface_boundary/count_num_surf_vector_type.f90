!count_num_surf_vector_type.f90
!      module count_num_surf_vector_type
!
!        (module m_count_num_surf_vector)
!
!      Written by H. Matsui on Feb., 2009
!
!      subroutine count_num_surf_velo_type(sf_grp, sf_nod, velo)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surface_node_grp_data), intent(in) :: sf_nod
!        type(velocity_surf_bc_type), intent(inout) :: velo
!      subroutine count_num_surf_vect_p_type(sf_grp, sf_nod, vector_p)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surface_node_grp_data), intent(in) :: sf_nod
!        type(velocity_surf_bc_type), intent(inout) :: vector_p
!      subroutine count_num_surf_magne_type(sf_grp, sf_nod, magne)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surface_node_grp_data), intent(in) :: sf_nod
!        type(vector_surf_bc_type), intent(inout) :: magne
!      subroutine count_num_surf_current_type(sf_grp, sf_nod, current)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surface_node_grp_data), intent(in) :: sf_nod
!        type(vector_surf_bc_type), intent(inout) :: current
!
!      subroutine count_num_surf_torque_type(sf_grp, velo)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(velocity_surf_bc_type), intent(inout) :: velo
!      subroutine count_num_surf_grad_vecp_type(sf_grp, vect_p)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(velocity_surf_bc_type), intent(inout) :: vect_p
!      subroutine count_num_surf_grad_b_type(sf_grp, magne)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(vector_surf_bc_type), intent(inout) :: magne
!      subroutine count_num_surf_grad_j_type(sf_grp, current)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(vector_surf_bc_type), intent(inout) :: current
!
      module count_num_surf_vector_type
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
      subroutine count_num_surf_velo_type(sf_grp, sf_nod, velo)
!
      use t_surface_group_connect
      use m_boundary_condition_IDs
      use set_surf_vector_id
      use set_stress_free_surf_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(velocity_surf_bc_type), intent(inout) :: velo
!
!
      call s_count_num_surf_vector(sf_grp, sf_nod,                      &
     &    torque_surf%num_bc, torque_surf%bc_name,                      &
     &    torque_surf%ibc_type, name_svn,                               &
     &    velo%sgs, velo%normal)
!
      call count_num_stress_free_surf(sf_grp%num_grp, sf_grp%grp_name,  &
     &    torque_surf%num_bc, torque_surf%bc_name,                      &
     &    torque_surf%ibc_type,                                         &
     &    iflag_surf_free_sph_in, iflag_surf_free_sph_out,              &
     &    velo%free_sph_in%ngrp_sf_dat, velo%free_sph_out%ngrp_sf_dat)
!
      end subroutine count_num_surf_velo_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_vect_p_type(sf_grp, sf_nod, vector_p)
!
      use t_surface_group_connect
      use m_boundary_condition_IDs
      use set_surf_vector_id
      use set_stress_free_surf_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(velocity_surf_bc_type), intent(inout) :: vector_p
!
!
      call s_count_num_surf_vector(sf_grp, sf_nod,                      &
     &    a_potential_surf%num_bc, a_potential_surf%bc_name,            &
     &    a_potential_surf%ibc_type, name_san,                          &
     &    vector_p%sgs, vector_p%normal)
!
      call count_num_stress_free_surf(sf_grp%num_grp, sf_grp%grp_name,  &
     &    torque_surf%num_bc, torque_surf%bc_name,                      &
     &    torque_surf%ibc_type,                                         &
     &    iflag_surf_qvc_sph_in, iflag_surf_qvc_sph_out,                &
     &    vector_p%free_sph_in%ngrp_sf_dat,                             &
     &    vector_p%free_sph_out%ngrp_sf_dat)
!
      end subroutine count_num_surf_vect_p_type
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_magne_type(sf_grp, sf_nod, magne)
!
      use t_surface_group_connect
      use set_surf_vector_id
      use set_stress_free_surf_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(vector_surf_bc_type), intent(inout) :: magne
!
!
      call s_count_num_surf_vector(sf_grp, sf_nod,                      &
     &     magne_surf%num_bc, magne_surf%bc_name, magne_surf%ibc_type,  &
     &     name_sbn, magne%sgs, magne%normal)
!
      end subroutine count_num_surf_magne_type
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_current_type(sf_grp, sf_nod, current)
!
      use t_surface_group_connect
      use set_surf_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(vector_surf_bc_type), intent(inout) :: current
!
!
      call s_count_num_surf_vector(sf_grp, sf_nod,                      &
     &     current_surf%num_bc, current_surf%bc_name,                   &
     &     current_surf%ibc_type, name_sjn,                             &
     &     current%sgs, current%normal)
!
      end subroutine count_num_surf_current_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_torque_type(sf_grp, velo)
!
      use set_sf_grad_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(inout) :: velo
!
!
      call count_num_sf_grad_vector                                     &
     &    (sf_grp, torque_surf%num_bc, torque_surf%bc_name,             &
     &     torque_surf%ibc_type, name_vxg, name_vyg, name_vzg,          &
     &     velo%grad, velo%torque_lead)
!
      end subroutine count_num_surf_torque_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_grad_vecp_type(sf_grp, vect_p)
!
      use set_sf_grad_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(inout) :: vect_p
!
!
      call count_num_sf_grad_vector                                     &
     &   (sf_grp, a_potential_surf%num_bc, a_potential_surf%bc_name,    &
     &    a_potential_surf%ibc_type, name_axg, name_ayg, name_azg,      &
     &    vect_p%grad, vect_p%torque_lead)
!
      end subroutine count_num_surf_grad_vecp_type
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_grad_b_type(sf_grp, magne)
!
      use set_sf_grad_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(vector_surf_bc_type), intent(inout) :: magne
!
!
      call count_num_sf_grad_vector                                     &
     &    (sf_grp, magne_surf%num_bc, magne_surf%bc_name,               &
     &     magne_surf%ibc_type, name_bxg, name_byg, name_bzg,           &
     &     magne%grad, magne%torque_lead)
!
      end subroutine count_num_surf_grad_b_type
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_grad_j_type(sf_grp, current)
!
      use set_sf_grad_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(vector_surf_bc_type), intent(inout) :: current
!
!
      call count_num_sf_grad_vector                                     &
     &   (sf_grp, current_surf%num_bc, current_surf%bc_name,            &
     &    current_surf%ibc_type, name_jxg, name_jyg, name_jzg,          &
     &    current%grad, current%torque_lead)
!
      end subroutine count_num_surf_grad_j_type
!
!-----------------------------------------------------------------------
!
      end module count_num_surf_vector_type
