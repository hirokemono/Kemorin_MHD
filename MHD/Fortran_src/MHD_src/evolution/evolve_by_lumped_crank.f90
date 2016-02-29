!
!      module evolve_by_lumped_crank
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!!      subroutine cal_velo_pre_lumped_crank                            &
!!     &         (iak_diff_v, nod_comm, node, ele, fluid,               &
!!     &          iphys, iphys_ele, ele_fld, jac_3d, rhs_tbl,           &
!!     &          FEM_elens, Vmat_MG_DJDS, mhd_fem_wk, fem_wk,          &
!!     &          f_l, f_nl, nod_fld)
!!      subroutine cal_vect_p_pre_lumped_crank                          &
!!     &         (i_vecp, i_pre_uxb, iak_diff_b, nod_bc_a,              &
!!     &          nod_comm, node, ele, conduct, iphys_ele, ele_fld,     &
!!     &          jac_3d, rhs_tbl, FEM_elens, Bmat_MG_DJDS,             &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_magne_pre_lumped_crank                           &
!!     &         (i_magne, i_pre_uxb, iak_diff_b, nod_bc_b,             &
!!     &          nod_comm, node, ele, conduct, iphys_ele, ele_fld,     &
!!     &          jac_3d, rhs_tbl, FEM_elens, Bmat_MG_DJDS,             &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!
!!      subroutine cal_temp_pre_lumped_crank                            &
!!     &         (i_temp, i_pre_heat, iak_diff_t, nod_bc_t,             &
!!     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,       &
!!     &          jac_3d, rhs_tbl, FEM_elens, Tmat_MG_DJDS,             &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_per_temp_lumped_crank                            &
!!     &         (i_par_temp, i_pre_heat, iak_diff_t, nod_bc_t,         &
!!     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,       &
!!     &          jac_3d, rhs_tbl, FEM_elens, Tmat_MG_DJDS,             &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_composit_pre_lumped_crank                        &
!!     &         (i_light, i_pre_composit, iak_diff_c, nod_bc_c,        &
!!     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,       &
!!     &          jac_3d, rhs_tbl, FEM_elens, Cmat_MG_DJDS,             &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_a
!!        type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_b
!!        type(scaler_fixed_nod_bc_type), intent(in) :: nod_bc_t
!!        type(scaler_fixed_nod_bc_type), intent(in) :: nod_bc_c
!!        type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!!        type(DJDS_MATRIX), intent(in) :: Vmat_MG_DJDS(0:num_MG_level)
!!        type(DJDS_MATRIX), intent(in) :: Bmat_MG_DJDS(0:num_MG_level)
!!        type(DJDS_MATRIX), intent(in) :: Tmat_MG_DJDS(0:num_MG_level)
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
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
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_nodal_bc_data
      use t_solver_djds
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_velo_pre_lumped_crank                              &
     &         (iak_diff_v, nod_comm, node, ele, fluid,                 &
     &          iphys, iphys_ele, ele_fld, jac_3d, rhs_tbl,             &
     &          FEM_elens, Vmat_MG_DJDS, mhd_fem_wk, fem_wk,            &
     &          f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_multi_pass
      use set_nodal_bc_id_data
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use int_vol_coriolis_term
      use cal_sol_field_explicit
!
      integer(kind = kint), intent(in) :: iak_diff_v
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!
      type(DJDS_MATRIX), intent(in) :: Vmat_MG_DJDS(0:num_MG_level)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (coef_imp_v.gt.0.0d0) then
        call int_sk_4_fixed_velo(iphys%i_velo, iak_diff_v, node, ele, &
     &      nod_fld, jac_3d, rhs_tbl, FEM_elens, fem_wk, f_l)
!        if (iflag_initial_step.eq.1) coef_imp_v = 1.0d0 / coef_imp_v
      end if
!
      call cal_t_evo_4_vector(iflag_velo_supg,                          &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl, nod_comm,      &
     &    node, ele, iphys_ele, ele_fld, jac_3d, rhs_tbl,               &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'int_coriolis_nod_exp'
      call int_coriolis_nod_exp(node, mhd_fem_wk,                       &
     &    iphys%i_velo, nod_fld, f_l, f_nl)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_buoyancy_nod_exp'
      call int_buoyancy_nod_exp                                         &
     &   (node, mhd_fem_wk, iphys, nod_fld, f_nl)
!
      call set_boundary_velo_4_rhs(node, f_l, f_nl)
!
      call cal_sol_vec_fluid_linear(node%numnod, node%istack_nod_smp,   &
     &    mhd_fem_wk%mlump_fl%ml_o, f_nl%ff, nod_fld%ntot_phys,         &
     &    n_vector, iphys%i_velo, iphys%i_pre_mom, nod_fld%d_fld,       &
     &    f_l%ff)
!
      call solver_crank_vector(node,                                    &
     &    DJDS_comm_fl, DJDS_fluid, num_MG_level,                       &
     &    MG_itp, MG_comm_fl, MG_djds_tbl_fl, Vmat_MG_DJDS,             &
     &    method_4_velo, precond_4_crank, eps_4_velo_crank, itr,        &
     &    iphys%i_velo, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_velo_pre_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_vect_p_pre_lumped_crank                            &
     &         (i_vecp, i_pre_uxb, iak_diff_b, nod_bc_a,                &
     &          nod_comm, node, ele, conduct, iphys_ele, ele_fld,       &
     &          jac_3d, rhs_tbl, FEM_elens, Bmat_MG_DJDS,               &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
      use m_ele_material_property
!
      use cal_multi_pass
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use set_boundary_scalars
      use copy_nodal_fields
      use cal_sol_field_explicit
!
      integer(kind = kint), intent(in) :: i_vecp, i_pre_uxb
      integer(kind = kint), intent(in) :: iak_diff_b
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_a
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!
      type(DJDS_MATRIX), intent(in) :: Bmat_MG_DJDS(0:num_MG_level)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (coef_imp_b.gt.0.0d0) then
        call int_sk_4_fixed_vector(iflag_commute_magne,                 &
     &      i_vecp, node, ele, nod_fld, jac_3d, rhs_tbl,                &
     &      FEM_elens, nod_bc_a, ak_d_magne, coef_imp_b, iak_diff_b,    &
     &      fem_wk, f_l)
!        if (iflag_initial_step.eq.1) coef_imp_b = 1.0d0 / coef_imp_b
      end if
!
      call cal_t_evo_4_vector_cd(iflag_mag_supg,                        &
     &    conduct%istack_ele_fld_smp, mhd_fem_wk%mlump_cd,              &
     &    nod_comm, node, ele, iphys_ele, ele_fld, jac_3d,              &
     &    rhs_tbl, mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      call delete_vector_ffs_on_bc(node, nod_bc_a, f_l, f_nl)
!
      call cal_sol_vec_conduct_linear(node%numnod,                      &
     &    node%istack_internal_smp, conduct%istack_inter_fld_smp,       &
     &    conduct%numnod_fld, conduct%inod_fld,                         &
     &    mhd_fem_wk%mlump_cd%ml_o, f_nl%ff,                            &
     &    nod_fld%ntot_phys, n_vector, i_vecp, i_pre_uxb,               &
     &    nod_fld%d_fld, f_l%ff)
!
      call solver_crank_vector                                          &
     &   (node, DJDS_comm_etr, DJDS_entire,                             &
     &    num_MG_level, MG_itp, MG_comm, MG_djds_tbl, Bmat_MG_DJDS,     &
     &    method_4_velo, precond_4_crank, eps_4_magne_crank, itr,       &
     &    i_vecp, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_vect_p_pre_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_magne_pre_lumped_crank                             &
     &         (i_magne, i_pre_uxb, iak_diff_b, nod_bc_b,               &
     &          nod_comm, node, ele, conduct, iphys_ele, ele_fld,       &
     &          jac_3d, rhs_tbl, FEM_elens, Bmat_MG_DJDS,               &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
      use m_ele_material_property
!
      use cal_multi_pass
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use cal_sol_field_explicit
      use set_boundary_scalars
!
      integer(kind = kint), intent(in) :: i_magne, i_pre_uxb
      integer(kind = kint), intent(in) :: iak_diff_b
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_b
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!
      type(DJDS_MATRIX), intent(in) :: Bmat_MG_DJDS(0:num_MG_level)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (coef_imp_b.gt.0.0d0) then
        call int_sk_4_fixed_vector(iflag_commute_magne,                 &
     &      i_magne, node, ele, nod_fld, jac_3d, rhs_tbl,               &
     &      FEM_elens, nod_bc_b, ak_d_magne, coef_imp_b, iak_diff_b,    &
     &      fem_wk, f_l)
!        if (iflag_initial_step.eq.1) coef_imp_b = 1.0d0 / coef_imp_b
      end if
!
      call cal_t_evo_4_vector_cd(iflag_mag_supg,                        &
     &    conduct%istack_ele_fld_smp, mhd_fem_wk%mlump_cd,              &
     &    nod_comm, node, ele, iphys_ele, ele_fld, jac_3d,              &
     &    rhs_tbl, mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      if (iflag_debug .eq. 0 ) write(*,*) 'bc_4_magne_rhs'
      call delete_vector_ffs_on_bc(node, nod_bc_b, f_l, f_nl)
!
      call cal_sol_vec_conduct_linear(node%numnod,                      &
     &    node%istack_internal_smp, conduct%istack_inter_fld_smp,       &
     &    conduct%numnod_fld, conduct%inod_fld,                         &
     &    mhd_fem_wk%mlump_cd%ml_o, f_nl%ff,                            &
     &    nod_fld%ntot_phys, n_vector, i_magne, i_pre_uxb,              &
     &    nod_fld%d_fld, f_l%ff)
!
      if (iflag_debug .eq. 0 ) write(*,*) 'time_evolution'
      call solver_crank_vector                                          &
     &   (node, DJDS_comm_etr, DJDS_entire,                             &
     &    num_MG_level, MG_itp, MG_comm, MG_djds_tbl, Bmat_MG_DJDS,     &
     &    method_4_velo, precond_4_crank, eps_4_magne_crank, itr,       &
     &    i_magne, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
       end subroutine cal_magne_pre_lumped_crank
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_temp_pre_lumped_crank                              &
     &         (i_temp, i_pre_heat, iak_diff_t, nod_bc_t,               &
     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,         &
     &          jac_3d, rhs_tbl, FEM_elens, Tmat_MG_DJDS,               &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_multi_pass
      use set_boundary_scalars
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use cal_sol_field_explicit
!
      integer(kind = kint), intent(in) :: i_temp, i_pre_heat
      integer(kind = kint), intent(in) :: iak_diff_t
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(scaler_fixed_nod_bc_type), intent(in) :: nod_bc_t
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!
      type(DJDS_MATRIX), intent(in) :: Tmat_MG_DJDS(0:num_MG_level)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (coef_imp_t .gt. 0.0d0) then
        call int_sk_4_fixed_temp(i_temp, iak_diff_t, node, ele,         &
     &      nod_fld, jac_3d, rhs_tbl, FEM_elens, fem_wk, f_l)
!        if (iflag_initial_step.eq.1) coef_imp_t = 1.0d0 / coef_imp_t
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'multi_pass temp'
      call cal_t_evo_4_scalar(iflag_temp_supg,                          &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl, nod_comm,      &
     &    node, ele, iphys_ele, ele_fld, jac_3d, rhs_tbl,               &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      call set_boundary_rhs_scalar(node, nod_bc_t, f_l, f_nl)
!
      call cal_sol_vec_fluid_linear(node%numnod, node%istack_nod_smp,   &
     &    mhd_fem_wk%mlump_fl%ml_o, f_nl%ff, nod_fld%ntot_phys,         &
     &    n_scalar, i_temp, i_pre_heat, nod_fld%d_fld, f_l%ff)
!
      call solver_crank_scalar(node,                                    &
     &   DJDS_comm_fl, DJDS_fluid, num_MG_level,                        &
     &    MG_itp, MG_comm_fl, MG_djds_tbl_fl, Tmat_MG_DJDS,             &
     &    method_4_solver, precond_4_solver, eps_4_temp_crank, itr,     &
     &    i_temp, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_temp_pre_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_per_temp_lumped_crank                              &
     &         (i_par_temp, i_pre_heat, iak_diff_t, nod_bc_t,           &
     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,         &
     &          jac_3d, rhs_tbl, FEM_elens, Tmat_MG_DJDS,               &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_multi_pass
      use set_boundary_scalars
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use cal_sol_field_explicit
!
      integer(kind = kint), intent(in) :: i_par_temp, i_pre_heat
      integer(kind = kint), intent(in) :: iak_diff_t
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(scaler_fixed_nod_bc_type), intent(in) :: nod_bc_t
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!
      type(DJDS_MATRIX), intent(in) :: Tmat_MG_DJDS(0:num_MG_level)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (coef_imp_t .gt. 0.0d0) then
        call int_sk_4_fixed_part_temp(i_par_temp, iak_diff_t,           &
     &      node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,             &
     &      fem_wk, f_l)
        if (iflag_initial_step.eq.1) coef_imp_t = 1.0d0 / coef_imp_t
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'multi_pass temp'
      call cal_t_evo_4_scalar(iflag_temp_supg,                          &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl, nod_comm,      &
     &    node, ele, iphys_ele, ele_fld, jac_3d, rhs_tbl,               &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      call set_boundary_rhs_scalar(node, nod_bc_t, f_l, f_nl)
!
      call cal_sol_vec_fluid_linear(node%numnod, node%istack_nod_smp,   &
     &    mhd_fem_wk%mlump_fl%ml_o, f_nl%ff, nod_fld%ntot_phys,         &
     &    n_scalar, i_par_temp, i_pre_heat, nod_fld%d_fld, f_l%ff)
!
      call solver_crank_scalar(node,                                    &
     &    DJDS_comm_fl, DJDS_fluid, num_MG_level,                       &
     &    MG_itp, MG_comm_fl, MG_djds_tbl_fl, Tmat_MG_DJDS,             &
     &    method_4_solver, precond_4_solver, eps_4_temp_crank, itr,     &
     &    i_par_temp, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_per_temp_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_composit_pre_lumped_crank                          &
     &         (i_light, i_pre_composit, iak_diff_c, nod_bc_c,          &
     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,         &
     &          jac_3d, rhs_tbl, FEM_elens, Cmat_MG_DJDS,               &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_multi_pass
      use set_boundary_scalars
      use cal_sol_field_explicit
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
!
      integer(kind = kint), intent(in) :: i_light, i_pre_composit
      integer(kind = kint), intent(in) :: iak_diff_c
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(scaler_fixed_nod_bc_type), intent(in) :: nod_bc_c
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!
      type(DJDS_MATRIX), intent(in) :: Cmat_MG_DJDS(0:num_MG_level)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (coef_imp_c.gt.0.0d0) then
        call int_sk_4_fixed_composition(i_light, iak_diff_c,            &
     &      node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,             &
     &      fem_wk, f_l)
!         if (iflag_initial_step.eq.1) coef_imp_c = 1.0d0 / coef_imp_c
      end if
!
      call cal_t_evo_4_scalar(iflag_comp_supg,                          &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl, nod_comm,      &
     &    node, ele, iphys_ele, ele_fld, jac_3d, rhs_tbl,               &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      call set_boundary_rhs_scalar(node, nod_bc_c, f_l, f_nl)
      call cal_sol_vec_fluid_linear(node%numnod, node%istack_nod_smp,   &
     &    mhd_fem_wk%mlump_fl%ml_o, f_nl%ff, nod_fld%ntot_phys,         &
     &    n_scalar, i_light, i_pre_composit,  nod_fld%d_fld, f_l%ff)
!
      call solver_crank_scalar(node,                                    &
     &    DJDS_comm_fl, DJDS_fluid, num_MG_level,                       &
     &    MG_itp, MG_comm_fl, MG_djds_tbl_fl, Cmat_MG_DJDS,             &
     &    method_4_solver, precond_4_solver, eps_4_comp_crank, itr,     &
     &    i_light, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_composit_pre_lumped_crank
!
! ----------------------------------------------------------------------
!
      end module evolve_by_lumped_crank
