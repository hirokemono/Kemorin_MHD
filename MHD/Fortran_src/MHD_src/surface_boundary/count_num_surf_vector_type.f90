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
!        type(vector_surf_bc_type), intent(inout) :: vector_p
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
!        type(vector_surf_bc_type), intent(inout) :: vect_p
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
      use set_surf_vector_id
      use set_stress_free_surf_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(velocity_surf_bc_type), intent(inout) :: velo
!
!
      call s_count_num_surf_vector                                      &
     &    (sf_grp%num_grp, sf_nod%inod_stack_sf_grp, sf_grp%grp_name,   &
     &     num_bc_tq, bc_tq_name, ibc_tq_type,                          &
     &     name_svn, velo%sgs%nmax_sf_dat, velo%sgs%ngrp_sf_dat,        &
     &     velo%normal%ngrp_sf_fix_fx, velo%normal%nitem_sf_fix_fx)
!
      call count_num_stress_free_surf(sf_grp%num_grp, sf_grp%grp_name,  &
     &    num_bc_tq, bc_tq_name, ibc_tq_type,                           &
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
      use set_surf_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(vector_surf_bc_type), intent(inout) :: vector_p
!
!
      call s_count_num_surf_vector                                      &
     &    (sf_grp%num_grp, sf_nod%inod_stack_sf_grp, sf_grp%grp_name,   &
     &     num_bc_vps, bc_vps_name, ibc_vps_type,                       &
     &     name_san, vector_p%sgs%nmax_sf_dat,                          &
     &     vector_p%sgs%ngrp_sf_dat, vector_p%normal%ngrp_sf_fix_fx,    &
     &     vector_p%normal%nitem_sf_fix_fx)
!
      end subroutine count_num_surf_vect_p_type
!
!-----------------------------------------------------------------------
!
      subroutine count_num_surf_magne_type(sf_grp, sf_nod, magne)
!
      use t_surface_group_connect
      use set_surf_vector_id
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_nod
      type(vector_surf_bc_type), intent(inout) :: magne
!
!
      call s_count_num_surf_vector                                      &
     &    (sf_grp%num_grp, sf_nod%inod_stack_sf_grp, sf_grp%grp_name,   &
     &     num_bc_bs, bc_bs_name, ibc_bs_type,                          &
     &     name_sbn, magne%sgs%nmax_sf_dat, magne%sgs%ngrp_sf_dat,      &
     &     magne%normal%ngrp_sf_fix_fx, magne%normal%nitem_sf_fix_fx)
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
      call s_count_num_surf_vector                                      &
     &    (sf_grp%num_grp, sf_nod%inod_stack_sf_grp, sf_grp%grp_name,   &
     &     num_bc_js, bc_js_name, ibc_js_type,                          &
     &     name_sjn, current%sgs%nmax_sf_dat, current%sgs%ngrp_sf_dat,  &
     &     current%normal%ngrp_sf_fix_fx,                               &
     &     current%normal%nitem_sf_fix_fx)
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
     &    (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,          &
     &     num_bc_tq, bc_tq_name, ibc_tq_type,                          &
     &     name_vxg, name_vyg, name_vzg, velo%torque%nmax_sf_fix_fx,    &
     &     velo%torque%nmax_ele_sf_fix_fx,                              &
     &     velo%torque_lead%nmax_sf_dat, velo%torque%ngrp_sf_fix_fx,    &
     &     velo%torque%nitem_sf_fix_fx, velo%torque_lead%ngrp_sf_dat)
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
      type(vector_surf_bc_type), intent(inout) :: vect_p
!
!
      call count_num_sf_grad_vector                                     &
     &    (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,          &
     &     num_bc_vps, bc_vps_name, ibc_vps_type,                       &
     &     name_axg, name_ayg, name_azg, vect_p%grad%nmax_sf_fix_fx,    &
     &     vect_p%grad%nmax_ele_sf_fix_fx,                              &
     &     vect_p%torque_lead%nmax_sf_dat, vect_p%grad%ngrp_sf_fix_fx,  &
     &     vect_p%grad%nitem_sf_fix_fx, vect_p%torque_lead%ngrp_sf_dat)
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
     &    (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,          &
     &     num_bc_bs, bc_bs_name, ibc_bs_type,                          &
     &     name_bxg, name_byg, name_bzg, magne%grad%nmax_sf_fix_fx,     &
     &     magne%grad%nmax_ele_sf_fix_fx,                               &
     &     magne%torque_lead%nmax_sf_dat, magne%grad%ngrp_sf_fix_fx,    &
     &     magne%grad%nitem_sf_fix_fx, magne%torque_lead%ngrp_sf_dat)
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
     &    (sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name,          &
     &     num_bc_js, bc_js_name, ibc_js_type,                          &
     &     name_jxg, name_jyg, name_jzg, current%grad%nmax_sf_fix_fx,   &
     &     current%grad%nmax_ele_sf_fix_fx,                             &
     &     current%torque_lead%nmax_sf_dat,                             &
     &     current%grad%ngrp_sf_fix_fx, current%grad%nitem_sf_fix_fx,   &
     &     current%torque_lead%ngrp_sf_dat)
!
      end subroutine count_num_surf_grad_j_type
!
!-----------------------------------------------------------------------
!
      end module count_num_surf_vector_type
