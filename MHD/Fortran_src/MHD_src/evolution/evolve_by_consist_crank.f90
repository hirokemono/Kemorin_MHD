!
!      module evolve_by_consist_crank
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!!      subroutine cal_velo_pre_consist_crank                           &
!!     &         (i_velo, i_pre_mom, iak_diff_v,                        &
!!     &          node, ele, fluid, Vnod_bcs, jac_3d, rhs_tbl,          &
!!     &          FEM_elens, diff_coefs, num_MG_level, MG_interpolate,  &
!!     &          MG_comm_fluid, MG_DJDS_fluid, Vmat_MG_DJDS, MG_vector,&
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_vect_p_pre_consist_crank                         &
!!     &         (i_vecp, i_pre_uxb, iak_diff_b, nod_bc_a,              &
!!     &          node, ele, conduct, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          diff_coefs, num_MG_level, MG_interpolate,             &
!!     &          MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS, MG_vector,&
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!       subroutine cal_magne_pre_consist_crank                         &
!!     &         (i_magne, i_pre_uxb, iak_diff_b, nod_bc_b,             &
!!     &          node, ele, conduct, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          diff_coefs, num_MG_level, MG_interpolate,             &
!!     &          MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS, MG_vector,&
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!
!!      subroutine cal_temp_pre_consist_crank                           &
!!     &         (i_temp, i_pre_heat, iak_diff_t, node, ele, fluid,     &
!!     &          Tnod_bcs, jac_3d, rhs_tbl, FEM_elens, diff_coefs,     &
!!     &          num_MG_level, MG_interpolate, MG_comm_fluid,          &
!!     &          MG_DJDS_fluid, Tmat_MG_DJDS, MG_vector,               &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_per_temp_consist_crank                           &
!!     &         (i_par_temp, i_pre_heat, iak_diff_t, node, ele,        &
!!     &          fluid, Tnod_bcs, jac_3d, rhs_tbl, FEM_elens,          &
!!     &          diff_coefs, num_MG_level, MG_interpolate,             &
!!     &          MG_comm_fluid, MG_DJDS_fluid, Tmat_MG_DJDS, MG_vector,&
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_composit_pre_consist_crank                       &
!!     &         (i_light, i_pre_composit, iak_diff_c, node, ele,       &
!!     &          fluid, Cnod_bcs, jac_3d, rhs_tbl, FEM_elens,          &
!!     &          diff_coefs, num_MG_level, MG_interpolate,             &
!!     &          MG_comm_fluid, MG_DJDS_fluid, Cmat_MG_DJDS, MG_vector,&
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(nodal_bcs_4_scalar_type), intent(in) :: Tnod_bcs
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(MHD_coefficients_type), intent(in) :: diff_coefs
!!        type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_a
!!        type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_b
!!        type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
!!        type(communication_table), intent(in)                         &
!!       &           :: MG_comm_table(0:num_MG_level)
!!        type(communication_table), intent(in)                         &
!!       &           :: MG_comm_fluid(0:num_MG_level)
!!        type(DJDS_ordering_table), intent(in)                         &
!!       &           :: MG_DJDS_table(0:num_MG_level)
!!        type(DJDS_ordering_table), intent(in)                         &
!!       &           :: MG_DJDS_fluid(0:num_MG_level)
!!        type(DJDS_MATRIX), intent(in) :: Vmat_MG_DJDS(0:num_MG_level)
!!        type(DJDS_MATRIX), intent(in) :: Bmat_MG_DJDS(0:num_MG_level)
!!        type(DJDS_MATRIX), intent(in) :: Tmat_MG_DJDS(0:num_MG_level)
!!        type(DJDS_MATRIX), intent(in) :: Cmat_MG_DJDS(0:num_MG_level)
!!        type(vectors_4_solver), intent(inout)                         &
!!       &           :: MG_vector(0:num_MG_level)
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module evolve_by_consist_crank
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_control_parameter
      use m_physical_property
      use m_t_int_parameter
      use m_t_step_parameter
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_nodal_bc_data
      use t_solver_djds
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
      subroutine cal_velo_pre_consist_crank                             &
     &         (i_velo, i_pre_mom, iak_diff_v,                          &
     &          node, ele, fluid, Vnod_bcs, jac_3d, rhs_tbl,            &
     &          FEM_elens, diff_coefs, num_MG_level, MG_interpolate,    &
     &          MG_comm_fluid, MG_DJDS_fluid, Vmat_MG_DJDS, MG_vector,  &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_phys_constants
      use m_iccg_parameter
      use m_solver_djds_MHD
      use m_array_for_send_recv
!
      use t_bc_data_velo
!
      use cal_sol_vector_pre_crank
      use set_nodal_bc_id_data
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
!
      integer(kind = kint), intent(in) :: i_velo, i_pre_mom
      integer(kind = kint), intent(in) :: iak_diff_v
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_coefficients_type), intent(in) :: diff_coefs
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
      type(communication_table), intent(in)                             &
     &           :: MG_comm_fluid(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &           :: MG_DJDS_fluid(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: Vmat_MG_DJDS(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:num_MG_level)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (coef_imp_v.gt.0.0d0) then
        call int_sk_4_fixed_velo(i_velo, iak_diff_v, node, ele,         &
     &      nod_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs,            &
     &      Vnod_bcs%nod_bc_v, Vnod_bcs%nod_bc_rot, fem_wk, f_l)
!        if (iflag_initial_step.eq.1) coef_imp_v = 1.0d0 / coef_imp_v
      end if
!
      call reset_ff_t_smp(node%max_nod_smp, mhd_fem_wk)
!
      call int_vol_initial_vector                                       &
     &   (fluid%istack_ele_fld_smp, i_velo, coef_velo,                  &
     &    node, ele, nod_fld, jac_3d, rhs_tbl, fem_wk, mhd_fem_wk)
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
!
      call set_boundary_velo_4_rhs(node, Vnod_bcs, f_l, f_nl)
!
      call cal_vector_pre_consist(node, coef_velo,                      &
     &    n_vector, i_pre_mom, nod_fld, rhs_tbl, mhd_fem_wk, f_nl, f_l)
!
      call solver_crank_vector(node, num_MG_level, MG_interpolate,      &
     &    MG_comm_fluid, MG_DJDS_fluid, Vmat_MG_DJDS,                   &
     &    method_4_velo, precond_4_crank, eps_4_velo_crank, itr,        &
     &    i_velo, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_velo_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_vect_p_pre_consist_crank                           &
     &         (i_vecp, i_pre_uxb, iak_diff_b, nod_bc_a,                &
     &          node, ele, conduct, jac_3d, rhs_tbl, FEM_elens,         &
     &          diff_coefs, num_MG_level, MG_interpolate,               &
     &          MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS, MG_vector,  &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_phys_constants
      use m_iccg_parameter
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_ele_material_property
!
      use cal_sol_vector_pre_crank
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
      use set_boundary_scalars
      use copy_nodal_fields
!
      integer(kind = kint), intent(in) :: i_vecp, i_pre_uxb
      integer(kind = kint), intent(in) :: iak_diff_b
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_coefficients_type), intent(in) :: diff_coefs
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_a
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
      type(communication_table), intent(in)                             &
     &           :: MG_comm_table(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &           :: MG_DJDS_table(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: Bmat_MG_DJDS(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:num_MG_level)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (coef_imp_b.gt.0.0d0) then
        call int_sk_4_fixed_vector(iflag_commute_magne,                 &
     &      i_vecp, node, ele, nod_fld, jac_3d, rhs_tbl,                &
     &      FEM_elens, diff_coefs, nod_bc_a, ak_d_magne,                &
     &      coef_imp_b, iak_diff_b, fem_wk, f_l)
!        if (iflag_initial_step.eq.1) coef_imp_b = 1.0d0 / coef_imp_b
      end if
!
      call reset_ff_t_smp(node%max_nod_smp, mhd_fem_wk)
!
      call int_vol_initial_vector                                       &
     &   (conduct%istack_ele_fld_smp, i_vecp, coef_magne,               &
     &    node, ele, nod_fld, jac_3d, rhs_tbl, fem_wk,  mhd_fem_wk)
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
!
      call delete_vector_ffs_on_bc(node, nod_bc_a, f_l, f_nl)
!
      call cal_vector_pre_consist(node, coef_magne,                     &
     &    n_vector, i_pre_uxb, nod_fld, rhs_tbl,                        &
     &    mhd_fem_wk, f_nl, f_l)
!
      if (iflag_debug.eq.1) write(*,*) 'time_evolution'
      call solver_crank_vector(node, num_MG_level, MG_interpolate,      &
     &    MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS,                   &
     &    method_4_velo, precond_4_crank, eps_4_magne_crank, itr,       &
     &    i_vecp, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_vect_p_pre_consist_crank
!
! ----------------------------------------------------------------------
!
       subroutine cal_magne_pre_consist_crank                           &
     &         (i_magne, i_pre_uxb, iak_diff_b, nod_bc_b,               &
     &          node, ele, conduct, jac_3d, rhs_tbl, FEM_elens,         &
     &          diff_coefs, num_MG_level, MG_interpolate,               &
     &          MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS, MG_vector,  &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_phys_constants
      use m_iccg_parameter
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_ele_material_property
!
      use cal_sol_vector_pre_crank
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
      use set_boundary_scalars
!
      integer(kind = kint), intent(in) :: i_magne, i_pre_uxb
      integer(kind = kint), intent(in) :: iak_diff_b
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_coefficients_type), intent(in) :: diff_coefs
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_b
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
      type(communication_table), intent(in)                             &
     &           :: MG_comm_table(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &           :: MG_DJDS_table(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: Bmat_MG_DJDS(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:num_MG_level)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (coef_imp_b.gt.0.0d0) then
        call int_sk_4_fixed_vector(iflag_commute_magne,                 &
     &      i_magne, node, ele, nod_fld, jac_3d, rhs_tbl,               &
     &      FEM_elens, diff_coefs, nod_bc_b, ak_d_magne,                &
     &      coef_imp_b, iak_diff_b, fem_wk, f_l)
!         if (iflag_initial_step.eq.1) coef_imp_b = 1.0d0 / coef_imp_b
      end if
!
      call reset_ff_t_smp(node%max_nod_smp, mhd_fem_wk)
      call int_vol_initial_vector                                       &
     &   (conduct%istack_ele_fld_smp, i_magne, coef_magne,              &
     &    node, ele, nod_fld, jac_3d, rhs_tbl, fem_wk, mhd_fem_wk)
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'bc_4_magne_rhs'
      call delete_vector_ffs_on_bc(node, nod_bc_b, f_l, f_nl)
!
      call cal_vector_pre_consist(node, coef_magne,                     &
     &    n_vector, i_pre_uxb, nod_fld, rhs_tbl,                        &
     &    mhd_fem_wk, f_nl, f_l)
!
      if (iflag_debug.eq.1)                                             &
     &        write(*,*) 'time_evolution for magnetic field'
      call solver_crank_vector(node, num_MG_level, MG_interpolate,      &
     &    MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS,                   &
     &    method_4_velo, precond_4_crank, eps_4_magne_crank, itr,       &
     &    i_magne, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
       end subroutine cal_magne_pre_consist_crank
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_temp_pre_consist_crank                             &
     &         (i_temp, i_pre_heat, iak_diff_t, node, ele, fluid,       &
     &          Tnod_bcs, jac_3d, rhs_tbl, FEM_elens, diff_coefs,       &
     &          num_MG_level, MG_interpolate, MG_comm_fluid,            &
     &          MG_DJDS_fluid, Tmat_MG_DJDS, MG_vector,                 &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_phys_constants
!
      use m_iccg_parameter
      use m_solver_djds_MHD
      use m_array_for_send_recv
!
      use t_bc_data_temp
!
      use cal_sol_vector_pre_crank
      use set_boundary_scalars
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
!
      integer(kind = kint), intent(in) :: i_temp, i_pre_heat
      integer(kind = kint), intent(in) :: iak_diff_t
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(nodal_bcs_4_scalar_type), intent(in) :: Tnod_bcs
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_coefficients_type), intent(in) :: diff_coefs
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
      type(communication_table), intent(in)                             &
     &           :: MG_comm_fluid(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &           :: MG_DJDS_fluid(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: Tmat_MG_DJDS(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:num_MG_level)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (coef_imp_t .gt. 0.0d0) then
        call int_sk_4_fixed_temp(i_temp, iak_diff_t, node, ele,         &
     &      nod_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs,            &
     &      Tnod_bcs%nod_bc_s, fem_wk, f_l)
!        if (iflag_initial_step.eq.1) coef_imp_t = 1.0d0 / coef_imp_t
      end if
!
      call reset_ff_t_smp(node%max_nod_smp, mhd_fem_wk)
!
      call int_vol_initial_scalar                                       &
     &   (fluid%istack_ele_fld_smp, i_temp, coef_temp,                  &
     &    node, ele, nod_fld, jac_3d, rhs_tbl, fem_wk, mhd_fem_wk)
      call set_ff_nl_smp_2_ff(n_scalar, node, rhs_tbl, f_l, f_nl)
!
      call set_boundary_rhs_scalar(node, Tnod_bcs%nod_bc_s, f_l, f_nl)
!
      call cal_vector_pre_consist(node, coef_temp,                      &
     &    n_scalar, i_pre_heat, nod_fld, rhs_tbl,                       &
     &    mhd_fem_wk, f_nl, f_l)
!
      call solver_crank_scalar(node, num_MG_level, MG_interpolate,      &
     &    MG_comm_fluid, MG_DJDS_fluid, Tmat_MG_DJDS,                   &
     &    method_4_solver, precond_4_solver, eps_4_temp_crank, itr,     &
     &    i_temp, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_temp_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_per_temp_consist_crank                             &
     &         (i_par_temp, i_pre_heat, iak_diff_t, node, ele,          &
     &          fluid, Tnod_bcs, jac_3d, rhs_tbl, FEM_elens,            &
     &          diff_coefs, num_MG_level, MG_interpolate,               &
     &          MG_comm_fluid, MG_DJDS_fluid, Tmat_MG_DJDS, MG_vector,  &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_phys_constants
!
      use m_iccg_parameter
      use m_solver_djds_MHD
      use m_array_for_send_recv
!
      use t_bc_data_temp
!
      use cal_sol_vector_pre_crank
      use set_boundary_scalars
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
!
      integer(kind = kint), intent(in) :: i_par_temp, i_pre_heat
      integer(kind = kint), intent(in) :: iak_diff_t
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(nodal_bcs_4_scalar_type), intent(in) :: Tnod_bcs
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_coefficients_type), intent(in) :: diff_coefs
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
      type(communication_table), intent(in)                             &
     &           :: MG_comm_fluid(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &           :: MG_DJDS_fluid(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: Tmat_MG_DJDS(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:num_MG_level)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (coef_imp_t .gt. 0.0d0) then
        call int_sk_4_fixed_part_temp(i_par_temp, iak_diff_t,           &
     &      node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs, &
     &      Tnod_bcs%nod_bc_s, fem_wk, f_l)
        if (iflag_initial_step.eq.1) coef_imp_t = 1.0d0 / coef_imp_t
      end if
!
      call reset_ff_t_smp(node%max_nod_smp, mhd_fem_wk)
!
      call int_vol_initial_scalar                                       &
     &   (fluid%istack_ele_fld_smp, i_par_temp, coef_temp,              &
     &    node, ele, nod_fld, jac_3d, rhs_tbl, fem_wk, mhd_fem_wk)
      call set_ff_nl_smp_2_ff(n_scalar, node, rhs_tbl, f_l, f_nl)
!
      call set_boundary_rhs_scalar(node, Tnod_bcs%nod_bc_s, f_l, f_nl)
!
      call cal_vector_pre_consist(node, coef_temp,                      &
     &    n_scalar, i_pre_heat, nod_fld, rhs_tbl,                       &
     &    mhd_fem_wk, f_nl, f_l)
!
      call solver_crank_scalar(node, num_MG_level, MG_interpolate,      &
     &    MG_comm_fluid, MG_DJDS_fluid, Tmat_MG_DJDS,                   &
     &    method_4_solver, precond_4_solver, eps_4_temp_crank, itr,     &
     &    i_par_temp, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_per_temp_consist_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_composit_pre_consist_crank                         &
     &         (i_light, i_pre_composit, iak_diff_c, node, ele,         &
     &          fluid, Cnod_bcs, jac_3d, rhs_tbl, FEM_elens,            &
     &          diff_coefs, num_MG_level, MG_interpolate,               &
     &          MG_comm_fluid, MG_DJDS_fluid, Cmat_MG_DJDS, MG_vector,  &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_solver_djds_MHD
      use m_array_for_send_recv
!
      use t_bc_data_temp
!
      use cal_sol_vector_pre_crank
      use set_boundary_scalars
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
!
      integer(kind = kint), intent(in) :: i_light, i_pre_composit
      integer(kind = kint), intent(in) :: iak_diff_c
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(nodal_bcs_4_scalar_type), intent(in) :: Cnod_bcs
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_coefficients_type), intent(in) :: diff_coefs
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
      type(communication_table), intent(in)                             &
     &           :: MG_comm_fluid(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &           :: MG_DJDS_fluid(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: Cmat_MG_DJDS(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:num_MG_level)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
       if (coef_imp_c.gt.0.0d0) then
         call int_sk_4_fixed_composition(i_light, iak_diff_c,           &
     &       node, ele, nod_fld, jac_3d, rhs_tbl,                       &
     &       FEM_elens, diff_coefs, Cnod_bcs%nod_bc_s, fem_wk, f_l)
!         if (iflag_initial_step.eq.1) coef_imp_c = 1.0d0 / coef_imp_c
       end if
!
      call reset_ff_t_smp(node%max_nod_smp, mhd_fem_wk)
!
      call int_vol_initial_scalar                                       &
     &   (fluid%istack_ele_fld_smp, i_light, coef_light,                &
     &    node, ele, nod_fld, jac_3d, rhs_tbl, fem_wk, mhd_fem_wk)
      call set_ff_nl_smp_2_ff(n_scalar, node, rhs_tbl, f_l, f_nl)
!
      call set_boundary_rhs_scalar(node, Cnod_bcs%nod_bc_s, f_l, f_nl)
!
      call cal_vector_pre_consist(node, coef_light,                     &
     &    n_scalar, i_pre_composit, nod_fld, rhs_tbl,                   &
     &    mhd_fem_wk, f_nl, f_l)
!
      call solver_crank_scalar(node, num_MG_level, MG_interpolate,      &
     &    MG_comm_fluid, MG_DJDS_fluid, Cmat_MG_DJDS,                   &
     &    method_4_solver, precond_4_solver, eps_4_comp_crank, itr,     &
     &    i_light, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
       end subroutine cal_composit_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      end module evolve_by_consist_crank
