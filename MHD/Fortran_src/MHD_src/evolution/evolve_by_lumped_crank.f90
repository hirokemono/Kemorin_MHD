!
!      module evolve_by_lumped_crank
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!!      subroutine cal_velo_pre_lumped_crank
!!      subroutine cal_vect_p_pre_lumped_crank
!!      subroutine cal_magne_pre_lumped_crank(iak_diff_b)
!!
!!      subroutine cal_temp_pre_lumped_crank
!!      subroutine cal_per_temp_lumped_crank
!!      subroutine cal_composit_pre_lumped_crank
!!
!!      subroutine cal_velo_co_lumped_crank
!!      subroutine cal_vector_p_co_lumped_crank
!!      subroutine cal_magne_co_lumped_crank
!
      module evolve_by_lumped_crank
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_t_int_parameter
      use m_t_step_parameter
      use m_phys_constants
      use m_nod_comm_table
      use m_geometry_data
      use m_geometry_data_MHD
      use m_group_data
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
      use m_filter_elength
      use m_SGS_address
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_velo_pre_lumped_crank
!
      use m_iccg_parameter
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_multi_pass
      use cal_sol_vector_pre_crank
      use set_nodal_bc_id_data
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use int_vol_coriolis_term
!
!
      if (coef_imp_v.gt.0.0d0) then
        call int_sk_4_fixed_velo(iphys%i_velo, iak_diff_v, node1, ele1, &
     &      nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen, fem1_wk, f1_l)
!        if (iflag_initial_step.eq.1) coef_imp_v = 1.0d0 / coef_imp_v
      end if
!
      call cal_t_evo_4_vector(iflag_velo_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'int_coriolis_nod_exp'
      call int_coriolis_nod_exp(node1, mhd_fem1_wk,                     &
     &    iphys%i_velo, nod_fld1, f1_l, f1_nl)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_buoyancy_nod_exp'
      call int_buoyancy_nod_exp                                         &
     &   (node1, mhd_fem1_wk, iphys, nod_fld1, f1_nl)
!
      call set_boundary_velo_4_rhs(node1, f1_l, f1_nl)
!
      call cal_sol_velo_pre_linear                                      &
     &   (node1, iphys, mhd_fem1_wk, f1_nl, f1_l, nod_fld1)
!
      call solver_crank_vector                                          &
     &   (node1, DJDS_comm_fl, DJDS_fluid, Vmat_DJDS, num_MG_level,     &
     &    MG_itp, MG_comm_fl, MG_djds_tbl_fl, MG_mat_velo,              &
     &    method_4_velo, precond_4_crank, eps_4_velo_crank, itr,        &
     &    iphys%i_velo, MG_vector, f1_l, b_vec, x_vec, nod_fld1)
!
      end subroutine cal_velo_pre_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_vect_p_pre_lumped_crank
!
      use m_iccg_parameter
      use m_bc_data_magne
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_multi_pass
      use cal_sol_vector_pre_crank
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use set_boundary_scalars
      use copy_nodal_fields
!
!
      if (coef_imp_b.gt.0.0d0) then
        call int_sk_4_fixed_vector_p(iphys%i_vecp, iak_diff_b,          &
     &      node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      fem1_wk, f1_l)
!        if (iflag_initial_step.eq.1) coef_imp_b = 1.0d0 / coef_imp_b
      end if
!
      call cal_t_evo_4_vector_cd(iflag_mag_supg,                        &
     &    conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,            &
     &    nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,        &
     &    rhs_tbl1, mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      call delete_vector_ffs_on_bc(node1, nod_bc1_a, f1_l, f1_nl)
!
      call cal_sol_vect_p_pre_linear                                    &
     &   (node1, iphys, mhd_fem1_wk, f1_nl, f1_l, nod_fld1)
!
      call solver_crank_vector                                          &
     &   (node1, DJDS_comm_etr, DJDS_entire, Bmat_DJDS, num_MG_level,   &
     &    MG_itp, MG_comm, MG_djds_tbl, MG_mat_magne,                   &
     &    method_4_velo, precond_4_crank, eps_4_magne_crank, itr,       &
     &    iphys%i_vecp, MG_vector, f1_l, b_vec, x_vec, nod_fld1)
!
      call clear_nodal_data(node1, nod_fld1, n_scalar, iphys%i_m_phi)
!
      end subroutine cal_vect_p_pre_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_magne_pre_lumped_crank(iak_diff_b)
!
      use m_iccg_parameter
      use m_bc_data_magne
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_multi_pass
      use cal_sol_vector_pre_crank
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use set_boundary_scalars
!
      integer(kind = kint), intent(in) :: iak_diff_b
!
!
      if (coef_imp_b.gt.0.0d0) then
        call int_sk_4_fixed_magne(iphys%i_magne, iak_diff_b,            &
     &      node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      fem1_wk, f1_l)
!        if (iflag_initial_step.eq.1) coef_imp_b = 1.0d0 / coef_imp_b
      end if
!
      call cal_t_evo_4_vector_cd(iflag_mag_supg,                        &
     &    conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,            &
     &    nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,        &
     &    rhs_tbl1, mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      if (iflag_debug .eq. 0 ) write(*,*) 'bc_4_magne_rhs'
      call delete_vector_ffs_on_bc(node1, nod_bc1_b, f1_l, f1_nl)
!
      call cal_sol_magne_pre_linear                                     &
     &   (node1, iphys, mhd_fem1_wk, f1_nl, f1_l, nod_fld1)
!
      if (iflag_debug .eq. 0 ) write(*,*) 'time_evolution'
      call solver_crank_vector                                          &
     &   (node1, DJDS_comm_etr, DJDS_entire, Bmat_DJDS, num_MG_level,   &
     &    MG_itp, MG_comm, MG_djds_tbl, MG_mat_magne,                   &
     &    method_4_velo, precond_4_crank, eps_4_magne_crank, itr,       &
     &    iphys%i_magne, MG_vector, f1_l, b_vec, x_vec, nod_fld1)
!
       end subroutine cal_magne_pre_lumped_crank
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_temp_pre_lumped_crank
!
      use m_iccg_parameter
      use m_bc_data_ene
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_sol_vector_pre_crank
      use cal_multi_pass
      use set_boundary_scalars
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
!
!
      if (coef_imp_t .gt. 0.0d0) then
        call int_sk_4_fixed_temp(iphys%i_temp, iak_diff_t, node1, ele1, &
     &      nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen, fem1_wk, f1_l)
!        if (iflag_initial_step.eq.1) coef_imp_t = 1.0d0 / coef_imp_t
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'multi_pass temp'
      call cal_t_evo_4_scalar(iflag_temp_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      call set_boundary_rhs_scalar(node1, nod_bc1_t, f1_l, f1_nl)
!
      call cal_sol_temp_linear                                          &
     &   (node1, iphys, mhd_fem1_wk, f1_nl, f1_l, nod_fld1)
!
      call solver_crank_scalar                                          &
     &   (node1, DJDS_comm_fl, DJDS_fluid, Tmat_DJDS, num_MG_level,     &
     &    MG_itp, MG_comm_fl, MG_djds_tbl_fl, MG_mat_temp,              &
     &    method_4_solver, precond_4_solver, eps_4_temp_crank, itr,     &
     &    iphys%i_temp, MG_vector, f1_l, b_vec, x_vec, nod_fld1)
!
      end subroutine cal_temp_pre_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_per_temp_lumped_crank
!
      use m_iccg_parameter
      use m_bc_data_ene
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_sol_vector_pre_crank
      use cal_multi_pass
      use set_boundary_scalars
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
!
!
      if (coef_imp_t .gt. 0.0d0) then
        call int_sk_4_fixed_part_temp(iphys%i_par_temp, iak_diff_t,     &
     &      node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      fem1_wk, f1_l)
        if (iflag_initial_step.eq.1) coef_imp_t = 1.0d0 / coef_imp_t
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'multi_pass temp'
      call cal_t_evo_4_scalar(iflag_temp_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      call set_boundary_rhs_scalar(node1, nod_bc1_t, f1_l, f1_nl)
!
      call cal_sol_par_temp_linear                                      &
     &   (node1, iphys, mhd_fem1_wk, f1_nl, f1_l, nod_fld1)
!
      call solver_crank_scalar                                          &
     &   (node1, DJDS_comm_fl, DJDS_fluid, Tmat_DJDS, num_MG_level,     &
     &    MG_itp, MG_comm_fl, MG_djds_tbl_fl, MG_mat_temp,              &
     &    method_4_solver, precond_4_solver, eps_4_temp_crank, itr,     &
     &    iphys%i_par_temp, MG_vector, f1_l, b_vec, x_vec, nod_fld1)
!
      end subroutine cal_per_temp_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_composit_pre_lumped_crank
!
      use m_iccg_parameter
      use m_bc_data_ene
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_multi_pass
      use set_boundary_scalars
      use cal_sol_vector_pre_crank
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
!
!
      if (coef_imp_c.gt.0.0d0) then
        call int_sk_4_fixed_composition(iphys%i_light, iak_diff_c,      &
     &      node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      fem1_wk, f1_l)
!         if (iflag_initial_step.eq.1) coef_imp_c = 1.0d0 / coef_imp_c
      end if
!
      call cal_t_evo_4_scalar(iflag_comp_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      call set_boundary_rhs_scalar(node1, nod_bc1_c, f1_l, f1_nl)
      call cal_sol_d_scalar_linear                                      &
     &   (node1, iphys, mhd_fem1_wk, f1_nl, f1_l, nod_fld1)
!
      call solver_crank_scalar                                          &
     &   (node1, DJDS_comm_fl, DJDS_fluid, Cmat_DJDS, num_MG_level,     &
     &    MG_itp, MG_comm_fl, MG_djds_tbl_fl, MG_mat_d_scalar,          &
     &    method_4_solver, precond_4_solver, eps_4_comp_crank, itr,     &
     &    iphys%i_light, MG_vector, f1_l, b_vec, x_vec, nod_fld1)
!
      end subroutine cal_composit_pre_lumped_crank
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_velo_co_lumped_crank
!
      use int_vol_coriolis_term
      use cal_multi_pass
      use set_nodal_bc_id_data
      use cal_sol_vector_co_crank
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_t_evo_4_vector_fl'
      call cal_t_evo_4_vector(iflag_velo_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'int_coriolis_nod_exp'
      call int_coriolis_nod_exp(node1, mhd_fem1_wk,                     &
     &    iphys%i_velo, nod_fld1, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_velo_4_rhs'
      call set_boundary_velo_4_rhs(node1, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_velo_co_crank'
      call cal_sol_velo_co_crank(node1%istack_internal_smp)
!
      end subroutine cal_velo_co_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_vector_p_co_lumped_crank
!
      use m_bc_data_magne
!
      use cal_multi_pass
      use cal_sol_vector_co_crank
      use set_boundary_scalars
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_t_evo_4_vector'
      call cal_t_evo_4_vector                                           &
     &   (iflag_mag_supg, ele1%istack_ele_smp, m1_lump, nod_comm,       &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_vect_p_4_rhs'
      call delete_vector_ffs_on_bc(node1, nod_bc1_a, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_vect_p_co_crank'
      call cal_sol_vect_p_co_crank(node1%istack_internal_smp)
!
      end subroutine cal_vector_p_co_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_magne_co_lumped_crank
!
      use m_bc_data_magne
!
      use cal_multi_pass
      use cal_sol_vector_co_crank
      use set_boundary_scalars
!
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_t_evo_4_vector'
      call cal_t_evo_4_vector                                           &
     &   (iflag_mag_supg, ele1%istack_ele_smp, m1_lump, nod_comm,       &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      if (iflag_debug.eq.1)   write(*,*) 'set_boundary_magne_4_rhs'
      call delete_vector_ffs_on_bc(node1, nod_bc1_b, f1_l, f1_nl)
!
      if (iflag_debug.eq.1)   write(*,*) 'cal_sol_magne_co_crank'
      call cal_sol_magne_co_crank(node1%istack_internal_smp)
!
      end subroutine cal_magne_co_lumped_crank
!
! -----------------------------------------------------------------------
!
      end module evolve_by_lumped_crank
