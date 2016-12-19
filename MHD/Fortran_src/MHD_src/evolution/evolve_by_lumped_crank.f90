!
!      module evolve_by_lumped_crank
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!!      subroutine cal_velo_pre_lumped_crank(iak_diff_v, ak_d_velo,     &
!!     &          nod_comm, node, ele, fluid, Vnod_bcs,                 &
!!     &          iphys, iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,&
!!     &          diff_coefs, Vmatrix, MG_vector, mhd_fem_wk, fem_wk,   &
!!     &          f_l, f_nl, nod_fld)
!!      subroutine cal_vect_p_pre_lumped_crank                          &
!!     &         (i_vecp, i_pre_uxb, iak_diff_b, ak_d_magne, nod_bc_a,  &
!!     &          nod_comm, node, ele, conduct, iphys_ele, ele_fld,     &
!!     &          jac_3d, rhs_tbl, FEM_elens, diff_coefs,               &
!!     &          Bmatrix, MG_vector, mhd_fem_wk, fem_wk,               &
!!     &          f_l, f_nl, nod_fld)
!!      subroutine cal_magne_pre_lumped_crank                           &
!!     &         (i_magne, i_pre_uxb, iak_diff_b, ak_d_magne, nod_bc_b, &
!!     &          nod_comm, node, ele, conduct, iphys_ele, ele_fld,     &
!!     &          jac_3d, rhs_tbl, FEM_elens, diff_coefs,               &
!!     &          Bmatrix, MG_vector, mhd_fem_wk, fem_wk,               &
!!     &          f_l, f_nl, nod_fld)
!!
!!      subroutine cal_temp_pre_lumped_crank                            &
!!     &         (i_temp, i_pre_heat, iak_diff_t, ak_d_temp,            &
!!     &          nod_comm, node, ele, fluid, Tnod_bcs, iphys_ele,      &
!!     &          ele_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs,      &
!!     &          Tmatrix, MG_vector, mhd_fem_wk, fem_wk,               &
!!     &          f_l, f_nl, nod_fld)
!!      subroutine cal_composit_pre_lumped_crank                        &
!!     &         (i_light, i_pre_composit, iak_diff_c, ak_d_composit,   &
!!     &          nod_comm, node, ele, fluid, Cnod_bcs, iphys_ele,      &
!!     &          ele_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs,      &
!!     &          Cmatrix, MG_vector, mhd_fem_wk, fem_wk,               &
!!     &          f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(nodal_bcs_4_scalar_type), intent(in) :: Tnod_bcs
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_a
!!        type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_b
!!        type(scaler_fixed_nod_bc_type), intent(in) :: nod_bc_c
!!        type(MHD_MG_matrix), intent(in) :: Vmatrix
!!        type(MHD_MG_matrix), intent(in) :: Bmatrix
!!        type(MHD_MG_matrix), intent(in) :: Tmatrix
!!        type(MHD_MG_matrix), intent(in) :: Cmatrix
!!        type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
!!        type(vectors_4_solver), intent(inout)                         &
!!       &           :: MG_vector(0:num_MG_level)
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
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
      use t_material_property
      use t_nodal_bc_data
      use t_solver_djds
      use t_solver_djds_MHD
      use t_interpolate_table
      use t_vector_for_solver
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_velo_pre_lumped_crank(iak_diff_v, ak_d_velo,       &
     &          nod_comm, node, ele, fluid, Vnod_bcs,                   &
     &          iphys, iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,  &
     &          diff_coefs, Vmatrix, MG_vector, mhd_fem_wk, fem_wk,     &
     &          f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_array_for_send_recv
!
      use t_bc_data_velo
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
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(MHD_MG_matrix), intent(in) :: Vmatrix
!
      real(kind = kreal), intent(in) :: ak_d_velo(ele%numele)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Vmatrix%nlevel_MG)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (coef_imp_v.gt.0.0d0) then
        call int_sk_4_fixed_velo(iphys%i_velo, iak_diff_v, node, ele,   &
     &      nod_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs,            &
     &      Vnod_bcs%nod_bc_v, Vnod_bcs%nod_bc_rot, ak_d_velo,          &
     &      fem_wk, f_l)
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
      call set_boundary_velo_4_rhs(node, Vnod_bcs, f_l, f_nl)
!
      call cal_sol_vec_fluid_linear(node%numnod, node%istack_nod_smp,   &
     &    mhd_fem_wk%mlump_fl%ml_o, f_nl%ff, nod_fld%ntot_phys,         &
     &    n_vector, iphys%i_velo, iphys%i_pre_mom, nod_fld%d_fld,       &
     &    f_l%ff)
!
      call solver_crank_vector(node, Vmatrix%nlevel_MG,                 &
     &    Vmatrix%MG_interpolate, Vmatrix%MG_comm_table,                &
     &    Vmatrix%MG_DJDS_table, Vmatrix%mat_MG_DJDS,                   &
     &    method_4_velo, precond_4_crank, eps_4_velo_crank, itr,        &
     &    iphys%i_velo, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_velo_pre_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_vect_p_pre_lumped_crank                            &
     &         (i_vecp, i_pre_uxb, iak_diff_b, ak_d_magne, nod_bc_a,    &
     &          nod_comm, node, ele, conduct, iphys_ele, ele_fld,       &
     &          jac_3d, rhs_tbl, FEM_elens, diff_coefs,                 &
     &          Bmatrix, MG_vector, mhd_fem_wk, fem_wk,                 &
     &          f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_array_for_send_recv
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
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_a
      type(MHD_MG_matrix), intent(in) :: Bmatrix
!
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Bmatrix%nlevel_MG)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (evo_vect_p%coef_imp .gt. 0.0d0) then
        call int_sk_4_fixed_vector(iflag_commute_magne,                 &
     &      i_vecp, node, ele, nod_fld, jac_3d, rhs_tbl,                &
     &      FEM_elens, diff_coefs, nod_bc_a, ak_d_magne,                &
     &      evo_vect_p%coef_imp, iak_diff_b, fem_wk, f_l)
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
      call solver_crank_vector(node, Bmatrix%nlevel_MG,                 &
     &    Bmatrix%MG_interpolate, Bmatrix%MG_comm_table,                &
     &    Bmatrix%MG_DJDS_table, Bmatrix%mat_MG_DJDS,                   &
     &    method_4_velo, precond_4_crank, eps_4_magne_crank, itr,       &
     &    i_vecp, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_vect_p_pre_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_magne_pre_lumped_crank                             &
     &         (i_magne, i_pre_uxb, iak_diff_b, ak_d_magne, nod_bc_b,   &
     &          nod_comm, node, ele, conduct, iphys_ele, ele_fld,       &
     &          jac_3d, rhs_tbl, FEM_elens, diff_coefs,                 &
     &          Bmatrix, MG_vector, mhd_fem_wk, fem_wk,                 &
     &          f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_array_for_send_recv
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
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_b
      type(MHD_MG_matrix), intent(in) :: Bmatrix
!
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Bmatrix%nlevel_MG)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (evo_magne%coef_imp .gt. 0.0d0) then
        call int_sk_4_fixed_vector(iflag_commute_magne,                 &
     &      i_magne, node, ele, nod_fld, jac_3d, rhs_tbl,               &
     &      FEM_elens, diff_coefs, nod_bc_b, ak_d_magne,                &
     &      evo_magne%coef_imp, iak_diff_b, fem_wk, f_l)
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
      call solver_crank_vector(node, Bmatrix%nlevel_MG,                 &
     &    Bmatrix%MG_interpolate, Bmatrix%MG_comm_table,                &
     &    Bmatrix%MG_DJDS_table, Bmatrix%mat_MG_DJDS,                   &
     &    method_4_velo, precond_4_crank, eps_4_magne_crank, itr,       &
     &    i_magne, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
       end subroutine cal_magne_pre_lumped_crank
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_temp_pre_lumped_crank                              &
     &         (i_temp, i_pre_heat, iak_diff_t, ak_d_temp,              &
     &          nod_comm, node, ele, fluid, Tnod_bcs, iphys_ele,        &
     &          ele_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs,        &
     &          Tmatrix, MG_vector, mhd_fem_wk, fem_wk,                 &
     &          f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_array_for_send_recv
!
      use t_bc_data_temp
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
      type(nodal_bcs_4_scalar_type), intent(in) :: Tnod_bcs
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(MHD_MG_matrix), intent(in) :: Tmatrix
!
      real(kind = kreal), intent(in) :: ak_d_temp(ele%numele)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Tmatrix%nlevel_MG)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (coef_imp_t .gt. 0.0d0) then
        call int_sk_fixed_temp(iflag_commute_temp, i_temp, iak_diff_t,  &
     &      node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs, &
     &      Tnod_bcs%nod_bc_s, ak_d_temp, coef_imp_t, fem_wk, f_l)
        if (iflag_initial_step.eq.1) coef_imp_t = 1.0d0 / coef_imp_t
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'multi_pass temp'
      call cal_t_evo_4_scalar(iflag_temp_supg,                          &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl, nod_comm,      &
     &    node, ele, iphys_ele, ele_fld, jac_3d, rhs_tbl,               &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      call set_boundary_rhs_scalar(node, Tnod_bcs%nod_bc_s, f_l, f_nl)
!
      call cal_sol_vec_fluid_linear(node%numnod, node%istack_nod_smp,   &
     &    mhd_fem_wk%mlump_fl%ml_o, f_nl%ff, nod_fld%ntot_phys,         &
     &    n_scalar, i_temp, i_pre_heat, nod_fld%d_fld, f_l%ff)
!
      call solver_crank_scalar(node, Tmatrix%nlevel_MG,                 &
     &    Tmatrix%MG_interpolate, Tmatrix%MG_comm_table,                &
     &    Tmatrix%MG_DJDS_table, Tmatrix%mat_MG_DJDS,                   &
     &    method_4_solver, precond_4_solver, eps_4_temp_crank, itr,     &
     &    i_temp, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_temp_pre_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_composit_pre_lumped_crank                          &
     &         (i_light, i_pre_composit, iak_diff_c, ak_d_composit,     &
     &          nod_comm, node, ele, fluid, Cnod_bcs, iphys_ele,        &
     &          ele_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs,        &
     &          Cmatrix, MG_vector, mhd_fem_wk, fem_wk,                 &
     &          f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_array_for_send_recv
!
      use t_bc_data_temp
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
      type(nodal_bcs_4_scalar_type), intent(in) :: Cnod_bcs
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(MHD_MG_matrix), intent(in) :: Cmatrix
!
      real(kind = kreal), intent(in) :: ak_d_composit(ele%numele)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Cmatrix%nlevel_MG)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (evo_comp%coef_imp .gt. zero) then
        call int_sk_fixed_temp                                          &
     &     (iflag_commute_composit, i_light, iak_diff_c,                &
     &      node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs, &
     &      Cnod_bcs%nod_bc_s, ak_d_composit, evo_comp%coef_imp,        &
     &      fem_wk, f_l)
      end if
!
      call cal_t_evo_4_scalar(iflag_comp_supg,                          &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl, nod_comm,      &
     &    node, ele, iphys_ele, ele_fld, jac_3d, rhs_tbl,               &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      call set_boundary_rhs_scalar(node, Cnod_bcs%nod_bc_s, f_l, f_nl)
      call cal_sol_vec_fluid_linear(node%numnod, node%istack_nod_smp,   &
     &    mhd_fem_wk%mlump_fl%ml_o, f_nl%ff, nod_fld%ntot_phys,         &
     &    n_scalar, i_light, i_pre_composit,  nod_fld%d_fld, f_l%ff)
!
      call solver_crank_scalar(node, Cmatrix%nlevel_MG,                 &
     &    Cmatrix%MG_interpolate, Cmatrix%MG_comm_table,                &
     &    Cmatrix%MG_DJDS_table, Cmatrix%mat_MG_DJDS,                   &
     &    method_4_solver, precond_4_solver, eps_4_comp_crank, itr,     &
     &    i_light, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_composit_pre_lumped_crank
!
! ----------------------------------------------------------------------
!
      end module evolve_by_lumped_crank
