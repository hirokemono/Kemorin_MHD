!
!      module evolve_by_lumped_crank
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!!      subroutine cal_velo_pre_lumped_crank(iflag_commute_velo,        &
!!     &          ifilter_final, iak_diff_v, ak_d_velo, dt, FEM_prm,    &
!!     &          nod_comm, node, ele, fluid, fl_prop, Vnod_bcs,        &
!!     &          iphys, iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,&
!!     &          diff_coefs, mlump_fl, Vmatrix, MG_vector,             &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_vect_p_pre_lumped_crank                          &
!!     &         (iflag_commute_magne, ifilter_final,                   &
!!     &          i_vecp, i_pre_uxb, iak_diff_b, ak_d_magne, nod_bc_a,  &
!!     &          dt, FEM_prm, nod_comm, node, ele, conduct, cd_prop,   &
!!     &          iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          diff_coefs, mlump_cd, Bmatrix, MG_vector,             &
!!     &          mhd_fem_wk, fem_wk,  f_l, f_nl, nod_fld)
!!      subroutine cal_magne_pre_lumped_crank                           &
!!     &         (iflag_commute_magne, ifilter_final,                   &
!!     &          i_magne, i_pre_uxb, iak_diff_b, ak_d_magne, nod_bc_b, &
!!     &          dt, FEM_prm, nod_comm, node, ele, conduct, cd_prop,   &
!!     &          iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          diff_coefs, mlump_cd, Bmatrix, MG_vector,             &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!
!!      subroutine cal_temp_pre_lumped_crank(iflag_supg,                &
!!     &          iflag_commute_field, ifilter_final, i_field,          &
!!     &          i_pre_advect, iak_diff, ak_diffuese, eps_4_crank, dt, &
!!     &          FEM_prm, nod_comm, node, ele, fluid, property,        &
!!     &          Snod_bcs, iphys_ele, ele_fld, jac_3d, rhs_tbl,        &
!!     &          FEM_elens, diff_coefs, mlump_fl, matrix, MG_vector,   &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: property
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(nodal_bcs_4_scalar_type), intent(in) :: Tnod_bcs
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(lumped_mass_matrices), intent(in) :: mlump_cd
!!        type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_a
!!        type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_b
!!        type(scaler_fixed_nod_bc_type), intent(in) :: nod_bc_c
!!        type(MHD_MG_matrix), intent(in) :: Vmatrix
!!        type(MHD_MG_matrix), intent(in) :: Bmatrix
!!        type(MHD_MG_matrix), intent(in) :: Tmatrix
!!        type(MHD_MG_matrix), intent(in) :: matrix
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
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_physical_property
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use m_fem_gauss_int_coefs
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
      subroutine cal_velo_pre_lumped_crank(iflag_commute_velo,          &
     &          ifilter_final, iak_diff_v, ak_d_velo, dt, FEM_prm,      &
     &          nod_comm, node, ele, fluid, fl_prop, Vnod_bcs,          &
     &          iphys, iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,  &
     &          diff_coefs, mlump_fl, Vmatrix, MG_vector,               &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
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
      integer(kind = kint), intent(in) :: iflag_commute_velo
      integer(kind = kint), intent(in) :: ifilter_final
      integer(kind = kint), intent(in) :: iak_diff_v
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_fl
      type(MHD_MG_matrix), intent(in) :: Vmatrix
!
      real(kind = kreal), intent(in) :: dt
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
      if (fl_prop%coef_imp .gt. zero) then
        call int_sk_4_fixed_velo                                        &
     &    (iflag_commute_velo, ifilter_final, FEM_prm%npoint_t_evo_int, &
     &     iphys%i_velo, iak_diff_v, node, ele, nod_fld, fl_prop,       &
     &     jac_3d, rhs_tbl, FEM_elens, diff_coefs,                      &
     &     Vnod_bcs%nod_bc_v, Vnod_bcs%nod_bc_rot, ak_d_velo,           &
     &     fem_wk, f_l)
      end if
!
      call cal_t_evo_4_vector                                           &
     &   (FEM_prm%iflag_velo_supg, fluid%istack_ele_fld_smp, dt,        &
     &    FEM_prm, mlump_fl, nod_comm, node, ele, iphys_ele, ele_fld,   &
     &    g_FEM1, jac_3d, rhs_tbl, mhd_fem_wk%ff_m_smp,                 &
     &    fem_wk, f_l, f_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'int_coriolis_nod_exp'
      call int_coriolis_nod_exp(node, fl_prop, mlump_fl,                &
     &    iphys%i_velo, nod_fld, f_l, f_nl)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_buoyancy_nod_exp'
      call int_buoyancy_nod_exp                                         &
     &   (node, fl_prop, mlump_fl, iphys, nod_fld, f_nl)
!
      call set_boundary_velo_4_rhs(node, Vnod_bcs, f_l, f_nl)
!
      call cal_sol_vec_fluid_linear                                     &
     &   (dt, node%numnod, node%istack_nod_smp,                         &
     &    mlump_fl%ml_o, f_nl%ff, nod_fld%ntot_phys, n_vector,          &
     &    iphys%i_velo, iphys%i_pre_mom, nod_fld%d_fld, f_l%ff)
!
      call solver_crank_vector                                          &
     &   (node, FEM_prm%MG_param, Vmatrix%nlevel_MG,                    &
     &    Vmatrix%MG_interpolate, Vmatrix%MG_comm_table,                &
     &    Vmatrix%MG_DJDS_table, Vmatrix%mat_MG_DJDS,                   &
     &    FEM_PRM%method_33, FEM_PRM%precond_33,                        &
     &    FEM_prm%eps_4_velo_crank, FEM_prm%CG11_param%MAXIT,           &
     &    iphys%i_velo, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_velo_pre_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_vect_p_pre_lumped_crank                            &
     &         (iflag_commute_magne, ifilter_final,                     &
     &          i_vecp, i_pre_uxb, iak_diff_b, ak_d_magne, nod_bc_a,    &
     &          dt, FEM_prm, nod_comm, node, ele, conduct, cd_prop,     &
     &          iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          diff_coefs, mlump_cd, Bmatrix, MG_vector,               &
     &          mhd_fem_wk, fem_wk,  f_l, f_nl, nod_fld)
!
      use m_array_for_send_recv
!
      use cal_multi_pass
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use set_boundary_scalars
      use copy_nodal_fields
      use cal_sol_field_explicit
!
      integer(kind = kint), intent(in) :: iflag_commute_magne
      integer(kind = kint), intent(in) :: ifilter_final
      integer(kind = kint), intent(in) :: i_vecp, i_pre_uxb
      integer(kind = kint), intent(in) :: iak_diff_b
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_cd
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_a
      type(MHD_MG_matrix), intent(in) :: Bmatrix
!
      real(kind = kreal), intent(in) :: dt
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
      if (cd_prop%coef_imp .gt. 0.0d0) then
        call int_sk_4_fixed_vector                                      &
     &   (iflag_commute_magne, ifilter_final, FEM_prm%npoint_t_evo_int, &
     &    i_vecp, node, ele, nod_fld, jac_3d, rhs_tbl,                  &
     &    FEM_elens, diff_coefs, nod_bc_a, ak_d_magne,                  &
     &    cd_prop%coef_imp, iak_diff_b, fem_wk, f_l)
      end if
!
      call cal_t_evo_4_vector_cd                                        &
     &   (FEM_prm%iflag_magne_supg, conduct%istack_ele_fld_smp, dt,     &
     &    FEM_prm, mlump_cd, nod_comm, node, ele, iphys_ele, ele_fld,   &
     &    g_FEM1, jac_3d, rhs_tbl, mhd_fem_wk%ff_m_smp,                 &
     &    fem_wk, f_l, f_nl)
!
      call delete_vector_ffs_on_bc(node, nod_bc_a, f_l, f_nl)
!
      call cal_sol_vec_conduct_linear(dt, node%numnod,                  &
     &    node%istack_internal_smp, conduct%istack_inter_fld_smp,       &
     &    conduct%numnod_fld, conduct%inod_fld, mlump_cd%ml_o, f_nl%ff, &
     &    nod_fld%ntot_phys, n_vector, i_vecp, i_pre_uxb,               &
     &    nod_fld%d_fld, f_l%ff)
!
      call solver_crank_vector                                          &
     &   (node, FEM_prm%MG_param, Bmatrix%nlevel_MG,                    &
     &    Bmatrix%MG_interpolate, Bmatrix%MG_comm_table,                &
     &    Bmatrix%MG_DJDS_table, Bmatrix%mat_MG_DJDS,                   &
     &    FEM_PRM%method_33, FEM_PRM%precond_33,                        &
     &    FEM_prm%eps_4_magne_crank, FEM_prm%CG11_param%MAXIT,          &
     &    i_vecp, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_vect_p_pre_lumped_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_magne_pre_lumped_crank                             &
     &         (iflag_commute_magne, ifilter_final,                     &
     &          i_magne, i_pre_uxb, iak_diff_b, ak_d_magne, nod_bc_b,   &
     &          dt, FEM_prm, nod_comm, node, ele, conduct, cd_prop,     &
     &          iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          diff_coefs, mlump_cd, Bmatrix, MG_vector,               &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_array_for_send_recv
!
      use cal_multi_pass
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use cal_sol_field_explicit
      use set_boundary_scalars
!
      integer(kind = kint), intent(in) :: iflag_commute_magne
      integer(kind = kint), intent(in) :: ifilter_final
      integer(kind = kint), intent(in) :: i_magne, i_pre_uxb
      integer(kind = kint), intent(in) :: iak_diff_b
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_cd
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_b
      type(MHD_MG_matrix), intent(in) :: Bmatrix
!
      real(kind = kreal), intent(in) :: dt
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
      if (cd_prop%coef_imp .gt. 0.0d0) then
        call int_sk_4_fixed_vector                                      &
     &   (iflag_commute_magne, ifilter_final, FEM_prm%npoint_t_evo_int, &
     &    i_magne, node, ele, nod_fld, jac_3d, rhs_tbl,                 &
     &    FEM_elens, diff_coefs, nod_bc_b, ak_d_magne,                  &
     &    cd_prop%coef_imp, iak_diff_b, fem_wk, f_l)
      end if
!
      call cal_t_evo_4_vector_cd                                        &
     &   (FEM_prm%iflag_magne_supg, conduct%istack_ele_fld_smp, dt,     &
     &    FEM_prm, mlump_cd, nod_comm, node, ele, iphys_ele, ele_fld,   &
     &    g_FEM1, jac_3d, rhs_tbl, mhd_fem_wk%ff_m_smp,                 &
     &    fem_wk, f_l, f_nl)
!
      if (iflag_debug .eq. 0 ) write(*,*) 'bc_4_magne_rhs'
      call delete_vector_ffs_on_bc(node, nod_bc_b, f_l, f_nl)
!
      call cal_sol_vec_conduct_linear(dt, node%numnod,                  &
     &    node%istack_internal_smp, conduct%istack_inter_fld_smp,       &
     &    conduct%numnod_fld, conduct%inod_fld, mlump_cd%ml_o, f_nl%ff, &
     &    nod_fld%ntot_phys, n_vector, i_magne, i_pre_uxb,              &
     &    nod_fld%d_fld, f_l%ff)
!
      if (iflag_debug .eq. 0 ) write(*,*) 'time_evolution'
      call solver_crank_vector                                          &
     &   (node, FEM_prm%MG_param, Bmatrix%nlevel_MG,                    &
     &    Bmatrix%MG_interpolate, Bmatrix%MG_comm_table,                &
     &    Bmatrix%MG_DJDS_table, Bmatrix%mat_MG_DJDS,                   &
     &    FEM_PRM%method_33, FEM_PRM%precond_33,                        &
     &    FEM_prm%eps_4_magne_crank, FEM_prm%CG11_param%MAXIT,          &
     &    i_magne, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
       end subroutine cal_magne_pre_lumped_crank
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_temp_pre_lumped_crank(iflag_supg,                  &
     &          iflag_commute_field, ifilter_final, i_field,            &
     &          i_pre_advect, iak_diff, ak_diffuese, eps_4_crank,       &
     &          dt, FEM_prm, nod_comm, node, ele, fluid, property,      &
     &          Snod_bcs, iphys_ele, ele_fld, jac_3d, rhs_tbl,          &
     &          FEM_elens, diff_coefs, mlump_fl, matrix, MG_vector,     &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
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
      integer(kind = kint), intent(in) :: iflag_supg
      integer(kind = kint), intent(in) :: iflag_commute_field
      integer(kind = kint), intent(in) :: ifilter_final
      integer(kind = kint), intent(in) :: i_field, i_pre_advect
      integer(kind = kint), intent(in) :: iak_diff
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(scalar_property), intent(in) :: property
      type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_fl
      type(MHD_MG_matrix), intent(in) :: matrix
!
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: eps_4_crank
      real(kind = kreal), intent(in) :: ak_diffuese(ele%numele)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:matrix%nlevel_MG)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (property%coef_imp .gt. zero) then
        call int_sk_fixed_temp(iflag_commute_field,                     &
     &      ifilter_final, FEM_prm%npoint_t_evo_int, i_field, iak_diff, &
     &      node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs, &
     &      Snod_bcs%nod_bc_s, ak_diffuese, property%coef_imp,          &
     &      fem_wk, f_l)
!        if (MHD_step1%iflag_initial_step.eq.1) then
!          property%coef_imp = 1.0d0 / property%coef_imp
!        end if
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'multi_pass temp'
      call cal_t_evo_4_scalar(iflag_supg, fluid%istack_ele_fld_smp, dt, &
     &    FEM_prm, mlump_fl, nod_comm, node, ele, iphys_ele, ele_fld,   &
     &    g_FEM1, jac_3d, rhs_tbl, mhd_fem_wk%ff_m_smp,                 &
     &    fem_wk, f_l, f_nl)
!
      call set_boundary_rhs_scalar(node, Snod_bcs%nod_bc_s, f_l, f_nl)
!
      call cal_sol_vec_fluid_linear                                     &
     &   (dt, node%numnod, node%istack_nod_smp, mlump_fl%ml_o,          &
     &    f_nl%ff, nod_fld%ntot_phys, n_scalar, i_field, i_pre_advect,  &
     &    nod_fld%d_fld, f_l%ff)
!
      call solver_crank_scalar                                          &
     &   (node, FEM_prm%MG_param, matrix%nlevel_MG,                     &
     &    matrix%MG_interpolate, matrix%MG_comm_table,                  &
     &    matrix%MG_DJDS_table, matrix%mat_MG_DJDS,                     &
     &    FEM_PRM%CG11_param%METHOD, FEM_PRM%CG11_param%PRECOND,        &
     &    eps_4_crank, FEM_prm%CG11_param%MAXIT,                        &
     &    i_field, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_temp_pre_lumped_crank
!
! ----------------------------------------------------------------------
!
      end module evolve_by_lumped_crank
