!
!      module m_count_num_surf_vector
!
!      Written by H. Matsui on Sep. 2005
!      Modified by H. Matsui on Feb., 2009
!
!!      subroutine count_num_surf_velo(sf_grp, sf_grp_nod)
!!      subroutine count_num_surf_vect_p(sf_grp, sf_grp_nod)
!!      subroutine count_num_surf_magne(sf_grp, sf_grp_nod)
!!      subroutine count_num_surf_current(sf_grp, sf_grp_nod)
!!
!!      subroutine count_num_surf_torque(sf_grp)
!!      subroutine count_num_surf_grad_vecp(sf_grp)
!!      subroutine count_num_surf_grad_b(sf_grp)
!!      subroutine count_num_surf_grad_j(sf_grp)
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!
      module m_count_num_surf_vector
!
      use m_precision
!
      use m_surf_data_list
      use m_header_4_surface_bc
      use t_group_data
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_velo(sf_grp, sf_grp_nod)
!
      use t_surface_group_connect
      use m_surf_data_torque
      use m_boundary_condition_IDs
      use set_surf_vector_id
      use set_stress_free_surf_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
!
!
      call s_count_num_surf_vector(sf_grp, sf_grp_nod,                  &
     &    torque_surf%num_bc, torque_surf%bc_name,                      &
     &    torque_surf%ibc_type, name_svn,                               &
     &    sf_sgs1_grad_v, sf_bc1_norm_v)
!
      call count_num_stress_free_surf                                   &
     &   (sf_grp%num_grp, sf_grp%grp_name,                              &
     &    torque_surf%num_bc, torque_surf%bc_name,                      &
     &    torque_surf%ibc_type,                                         &
     &    iflag_surf_free_sph_in, iflag_surf_free_sph_out,              &
     &    sf_bc1_free_sph_in%ngrp_sf_dat,                               &
     &    sf_bc1_free_sph_out%ngrp_sf_dat)
!
      end subroutine count_num_surf_velo
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_vect_p(sf_grp, sf_grp_nod)
!
      use t_surface_group_connect
      use m_surf_data_vector_p
      use m_boundary_condition_IDs
      use set_surf_vector_id
      use set_stress_free_surf_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
!
!
      call s_count_num_surf_vector(sf_grp, sf_grp_nod,                  &
     &     a_potential_surf%num_bc,                                     &
     &     a_potential_surf%bc_name, a_potential_surf%ibc_type,         &
     &     name_san, sf_sgs1_grad_a, sf_bc1_norm_a)
!
      call count_num_stress_free_surf                                   &
     &   (sf_grp%num_grp, sf_grp%grp_name,                              &
     &    torque_surf%num_bc, torque_surf%bc_name,                      &
     &    torque_surf%ibc_type,                                         &
     &    iflag_surf_qvc_sph_in, iflag_surf_qvc_sph_out,                &
     &    sf_bc1_pvc_in_a%ngrp_sf_dat, sf_bc1_pvc_out_a%ngrp_sf_dat)
!
      end subroutine count_num_surf_vect_p
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_magne(sf_grp, sf_grp_nod)
!
      use t_surface_group_connect
      use m_surf_data_magne
      use set_surf_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
!
!
      call s_count_num_surf_vector(sf_grp, sf_grp_nod,                  &
     &    magne_surf%num_bc, magne_surf%bc_name,                        &
     &    magne_surf%ibc_type, name_sbn, sf_sgs1_grad_b, sf_bc1_norm_b)
!
      end subroutine count_num_surf_magne
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_current(sf_grp, sf_grp_nod)
!
      use t_surface_group_connect
      use m_surf_data_current
      use set_surf_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
!
!
      call s_count_num_surf_vector(sf_grp, sf_grp_nod,                  &
     &    current_surf%num_bc, current_surf%bc_name,                    &
     &    current_surf%ibc_type, name_sjn,                              &
     &    sf_sgs1_grad_j, sf_bc1_norm_j)
!
      end subroutine count_num_surf_current
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_torque(sf_grp)
!
      use m_surf_data_torque
      use set_sf_grad_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call count_num_sf_grad_vector                                     &
     &   (sf_grp, torque_surf%num_bc, torque_surf%bc_name,              &
     &    torque_surf%ibc_type, name_vxg, name_vyg, name_vzg,           &
     &    sf_bc1_grad_v, sf_bc1_lead_tq)
!
      end subroutine count_num_surf_torque
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_grad_vecp(sf_grp)
!
      use m_surf_data_vector_p
      use set_sf_grad_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call count_num_sf_grad_vector                                     &
     &   (sf_grp, a_potential_surf%num_bc, a_potential_surf%bc_name,    &
     &    a_potential_surf%ibc_type, name_axg, name_ayg, name_azg,      &
     &    sf_bc1_grad_a, sf_bc1_lead_a)
!
      end subroutine count_num_surf_grad_vecp
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_grad_b(sf_grp)
!
      use m_surf_data_magne
      use set_sf_grad_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call count_num_sf_grad_vector                                     &
     &    (sf_grp, magne_surf%num_bc, magne_surf%bc_name,               &
     &     magne_surf%ibc_type, name_bxg, name_byg, name_bzg,           &
     &     sf_bc1_grad_b, sf_bc1_lead_b)
!
      end subroutine count_num_surf_grad_b
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_grad_j(sf_grp)
!
      use m_surf_data_current
      use set_sf_grad_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call count_num_sf_grad_vector                                     &
     &    (sf_grp, current_surf%num_bc, current_surf%bc_name,           &
     &     current_surf%ibc_type, name_jxg, name_jyg, name_jzg,         &
     &     sf_bc1_grad_j, sf_bc1_lead_j)
!
      end subroutine count_num_surf_grad_j
!
!-----------------------------------------------------------------------
!
      end module m_count_num_surf_vector
