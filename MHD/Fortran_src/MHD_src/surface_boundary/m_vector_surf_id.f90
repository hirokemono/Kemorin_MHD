!
!      module m_vector_surf_id
!
!      Written by H. Matsui on Sep. 2005
!
!      subroutine set_surf_velo_id
!      subroutine set_surf_vect_p_id
!      subroutine set_surf_magne_id
!      subroutine set_surf_current_id
!
!      subroutine set_surf_torque_id
!      subroutine set_surf_grad_vecp_id
!      subroutine set_surf_grad_b_id
!      subroutine set_surf_grad_j_id
!
      module m_vector_surf_id
!
      use m_precision
!
      use m_group_data
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
      subroutine set_surf_velo_id
!
      use m_surface_group_connect
      use m_surf_data_torque
      use m_boundary_condition_IDs
      use set_surf_vector_id
      use set_stress_free_surf_id
!
!
      call s_set_surf_vector_id(sf_grp1%num_grp, sf_grp1%grp_name,      &
     &    sf_grp_nod1%inod_stack_sf_grp,                                &
     &    torque_surf%num_bc, torque_surf%bc_name,                      &
     &    torque_surf%ibc_type, torque_surf%bc_magnitude,               &
     &    name_svn, nmax_sf_sgs_velo, id_grp_sf_sgs_velo,               &
     &    ngrp_sf_fix_vn, id_grp_sf_fix_vn,                             &
     &    nnod_sf_fix_vn, ist_nod_sf_fix_vn, sf_fix_vn_apt)
!
      call s_stress_free_surf_id(sf_grp1%num_grp, sf_grp1%grp_name,     &
     &    torque_surf%num_bc, torque_surf%bc_name,                      &
     &    torque_surf%ibc_type,                                         &
     &    iflag_surf_free_sph_in, iflag_surf_free_sph_out,              &
     &    ngrp_sf_fr_in, ngrp_sf_fr_out,                                &
     &    id_grp_sf_fr_in, id_grp_sf_fr_out)
!
      end subroutine set_surf_velo_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_vect_p_id
!
      use m_surface_group_connect
      use m_surf_data_vector_p
      use m_boundary_condition_IDs
      use set_surf_vector_id
      use set_stress_free_surf_id
!
!
      call s_set_surf_vector_id(sf_grp1%num_grp, sf_grp1%grp_name,      &
     &    sf_grp_nod1%inod_stack_sf_grp,                                &
     &    a_potential_surf%num_bc, a_potential_surf%bc_name,            &
     &    a_potential_surf%ibc_type, a_potential_surf%bc_magnitude,     &
     &    name_san, nmax_sf_sgs_vect_p, id_grp_sf_sgs_vect_p,           &
     &    ngrp_sf_fix_vpn, id_grp_sf_fix_vpn,                           &
     &    nnod_sf_fix_vpn, ist_nod_sf_fix_vpn, sf_apt_fix_vpn)
!
      call s_stress_free_surf_id(sf_grp1%num_grp, sf_grp1%grp_name,     &
     &    a_potential_surf%num_bc, a_potential_surf%bc_name,            &
     &    a_potential_surf%ibc_type,                                    &
     &    iflag_surf_qvc_sph_in, iflag_surf_qvc_sph_out,                &
     &    ngrp_sf_qvc_in, ngrp_sf_qvc_out,                              &
     &    id_grp_sf_qvc_in, id_grp_sf_qvc_out)
!
      end subroutine set_surf_vect_p_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_magne_id
!
      use m_surface_group_connect
      use m_surf_data_magne
      use set_surf_vector_id
!
!
      call s_set_surf_vector_id(sf_grp1%num_grp, sf_grp1%grp_name,      &
     &    sf_grp_nod1%inod_stack_sf_grp,                                &
     &    magne_surf%num_bc, magne_surf%bc_name,                        &
     &    magne_surf%ibc_type, magne_surf%bc_magnitude,                 &
     &    name_sbn, nmax_sf_sgs_magne, id_grp_sf_sgs_magne,             &
     &    ngrp_sf_fix_bn, id_grp_sf_fix_bn,                             &
     &    nnod_sf_fix_bn, ist_nod_sf_fix_bn, sf_apt_fix_bn)
!
      end subroutine set_surf_magne_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_current_id
!
      use m_surface_group_connect
      use m_surf_data_current
      use set_surf_vector_id
!
!
      call s_set_surf_vector_id(sf_grp1%num_grp, sf_grp1%grp_name,      &
     &    sf_grp_nod1%inod_stack_sf_grp,                                &
     &    current_surf%num_bc, current_surf%bc_name,                    &
     &    current_surf%ibc_type, current_surf%bc_magnitude,             &
     &    name_sjn, nmax_sf_sgs_current, id_grp_sf_sgs_current,         &
     &    ngrp_sf_fix_jn, id_grp_sf_fix_jn,                             &
     &    nnod_sf_fix_jn, ist_nod_sf_fix_jn, sf_apt_fix_jn)
!
      end subroutine set_surf_current_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_surf_torque_id
!
      use m_surf_data_torque
      use set_sf_grad_vector_id
!
!
      call s_set_sf_grad_vector_id                                      &
     &    (sf_grp1%num_grp, sf_grp1%istack_grp, sf_grp1%grp_name,       &
     &     torque_surf%num_bc, torque_surf%bc_name,                     &
     &     torque_surf%ibc_type, torque_surf%bc_magnitude,              &
     &     name_vxg, name_vyg, name_vzg,                                &
     &     nmax_sf_fix_tq, id_grp_sf_fix_tq,                            &
     &     nmax_ele_sf_fix_tq, ist_ele_sf_fix_tq, sf_apt_fix_tq,        &
     &     nmax_sf_lead_tq, id_grp_sf_lead_tq)
!
      end subroutine set_surf_torque_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_vecp_id
!
      use m_surf_data_vector_p
      use set_sf_grad_vector_id
!
!
      call s_set_sf_grad_vector_id                                      &
     &    (sf_grp1%num_grp, sf_grp1%istack_grp, sf_grp1%grp_name,       &
     &     a_potential_surf%num_bc, a_potential_surf%bc_name,           &
     &     a_potential_surf%ibc_type, a_potential_surf%bc_magnitude,    &
     &     name_axg, name_ayg, name_azg,                                &
     &     nmax_sf_fix_grad_a, id_grp_sf_fix_grad_a,                    &
     &     nmax_ele_sf_fix_grad_a, ist_ele_sf_fix_grad_a,               &
     &     sf_apt_fix_grad_a, nmax_sf_lead_vect_p,                      &
     &     id_grp_sf_lead_vect_p)
!
      end subroutine set_surf_grad_vecp_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_b_id
!
      use m_surf_data_magne
      use set_sf_grad_vector_id
!
!
      call s_set_sf_grad_vector_id                                      &
     &   (sf_grp1%num_grp, sf_grp1%istack_grp, sf_grp1%grp_name,        &
     &    magne_surf%num_bc, magne_surf%bc_name,                        &
     &    magne_surf%ibc_type, magne_surf%bc_magnitude,                 &
     &    name_bxg, name_byg, name_bzg,                                 &
     &    nmax_sf_fix_grad_b, id_grp_sf_fix_grad_b,                     &
     &    nmax_ele_sf_fix_grad_b, ist_ele_sf_fix_grad_b,                &
     &    sf_apt_fix_grad_b, nmax_sf_lead_b, id_grp_sf_lead_b)
!
      end subroutine set_surf_grad_b_id
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_grad_j_id
!
      use m_surf_data_current
      use set_sf_grad_vector_id
!
!
      call s_set_sf_grad_vector_id                                      &
     &    (sf_grp1%num_grp, sf_grp1%istack_grp, sf_grp1%grp_name,       &
     &     current_surf%num_bc, current_surf%bc_name,                   &
     &     current_surf%ibc_type, current_surf%bc_magnitude,            &
     &     name_sjx, name_sjy, name_sjz,                                &
     &     nmax_sf_fix_grad_j, id_grp_sf_fix_grad_j,                    &
     &     nmax_ele_sf_fix_grad_j, ist_ele_sf_fix_grad_j,               &
     &     sf_apt_fix_grad_j, nmax_sf_lead_j, id_grp_sf_lead_j)
!
      end subroutine set_surf_grad_j_id
!
!-----------------------------------------------------------------------
!
      end module m_vector_surf_id
