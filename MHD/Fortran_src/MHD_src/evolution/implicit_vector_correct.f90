!implicit_vector_correct.f90
!      module implicit_vector_correct
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on March, 2006
!
!!      subroutine cal_velocity_co_exp(i_velo, i_p_phi, FEM_prm,        &
!!     &          nod_comm, node, ele, fluid, g_FEM, jac_3d, rhs_tbl,   &
!!     &          mlump_fl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_vector_p_co_exp                                  &
!!     &         (i_vecp, FEM_prm, nod_comm, node, ele,                 &
!!     &          g_FEM, jac_3d, rhs_tbl, m_lump, mhd_fem_wk, fem_wk,   &
!!     &          f_l, f_nl, nod_fld)
!!      subroutine cal_magnetic_co_exp                                  &
!!     &         (i_magne, FEM_prm, nod_comm, node, ele,                &
!!     &          g_FEM, jac_3d, rhs_tbl, m_lump, mhd_fem_wk, fem_wk,   &
!!     &          f_l, f_nl, nod_fld)
!!
!!      subroutine cal_velocity_co_imp(i_velo, iak_diff_v, ak_d_velo,   &
!!     &          dt, FEM_prm, SGS_param, cmt_param,                    &
!!     &          nod_comm, node, ele, fluid, fl_prop, Vnod_bcs,        &
!!     &          iphys_ele, ele_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,&
!!     &          diff_coefs, mlump_fl, Vmatrix, MG_vector,             &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_vector_p_co_imp(i_vecp, iak_diff_b, ak_d_magne,  &
!!     &          dt, FEM_prm, SGS_param, cmt_param,                    &
!!     &          nod_comm, node, ele, conduct, cd_prop, Bnod_bcs,      &
!!     &          iphys_ele, ele_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,&
!!     &          diff_coefs, m_lump, Bmatrix, MG_vector,               &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_magnetic_co_imp(i_magne, iak_diff_b, ak_d_magne, &
!!     &          dt, FEM_prm, SGS_param, cmt_param,                    &
!!     &          nod_comm, node, ele, conduct, cd_prop, Bnod_bcs,      &
!!     &          iphys_ele, ele_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,&
!!     &          diff_coefs,m_lump,  Bmatrix, MG_vector,               &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type (lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(MHD_MG_matrix), intent(in) :: Vmatrix
!!        type(MHD_MG_matrix), intent(in) :: Bmatrix
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module implicit_vector_correct
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_solver_djds
      use t_solver_djds_MHD
      use t_bc_data_velo
      use t_bc_data_magne
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_velocity_co_exp(i_velo, i_p_phi, FEM_prm,          &
     &          nod_comm, node, ele, fluid, g_FEM, jac_3d, rhs_tbl,     &
     &          mlump_fl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use cal_multi_pass
      use cal_sol_vector_correct
!
      integer(kind=kint), intent(in) :: i_velo, i_p_phi
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type (lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_multi_pass_4_vector_fl'
      call cal_multi_pass_4_vector_ff(fluid%istack_ele_fld_smp,         &
     &    FEM_prm, mlump_fl, nod_comm, node, ele, g_FEM, jac_3d,        &
     &    rhs_tbl, mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_velo_co'
      call cal_sol_velocity_co                                          &
     &   (nod_fld%n_point, node%istack_internal_smp, mlump_fl%ml,       &
     &    f_l%ff, nod_fld%ntot_phys, i_velo, i_p_phi, nod_fld%d_fld)
!
      end subroutine cal_velocity_co_exp
!
! ----------------------------------------------------------------------
!
      subroutine cal_vector_p_co_exp                                    &
     &         (i_vecp, FEM_prm, nod_comm, node, ele,                   &
     &          g_FEM, jac_3d, rhs_tbl, m_lump, mhd_fem_wk, fem_wk,     &
     &          f_l, f_nl, nod_fld)
!
      use cal_multi_pass
      use cal_sol_vector_correct
!
      integer(kind=kint), intent(in) :: i_vecp
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_multi_pass_4_vector_ff'
      call cal_multi_pass_4_vector_ff                                   &
     &   (ele%istack_ele_smp, FEM_prm, m_lump,                          &
     &    nod_comm, node, ele, g_FEM, jac_3d, rhs_tbl,                  &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_vect_p_co'
      call cal_sol_vector_co                                            &
     &   (nod_fld%n_point, node%istack_internal_smp, m_lump%ml,         &
     &    f_l%ff, nod_fld%ntot_phys, i_vecp, nod_fld%d_fld)
!
      end subroutine cal_vector_p_co_exp
!
! ----------------------------------------------------------------------
!
      subroutine cal_magnetic_co_exp                                    &
     &         (i_magne, FEM_prm, nod_comm, node, ele,                  &
     &          g_FEM, jac_3d, rhs_tbl, m_lump, mhd_fem_wk, fem_wk,     &
     &          f_l, f_nl, nod_fld)
!
      use cal_multi_pass
      use cal_sol_vector_correct
!
      integer(kind=kint), intent(in) :: i_magne
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_multi_pass_4_vector_ff'
      call cal_multi_pass_4_vector_ff                                   &
     &   (ele%istack_ele_smp, FEM_prm, m_lump,                          &
     &    nod_comm, node, ele, g_FEM, jac_3d, rhs_tbl,                  &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_sol_magne_co'
      call cal_sol_vector_co                                            &
     &   (nod_fld%n_point, node%istack_internal_smp, m_lump%ml,         &
     &    f_l%ff, nod_fld%ntot_phys, i_magne, nod_fld%d_fld)
!
      end subroutine cal_magnetic_co_exp
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_velocity_co_imp(i_velo, iak_diff_v, ak_d_velo,     &
     &          dt, FEM_prm, SGS_param, cmt_param,                      &
     &          nod_comm, node, ele, fluid, fl_prop, Vnod_bcs,          &
     &          iphys_ele, ele_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,  &
     &          diff_coefs, mlump_fl, Vmatrix, MG_vector,               &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_array_for_send_recv
!
      use int_vol_diffusion_ele
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use int_vol_coriolis_term
      use evolve_by_lumped_crank
      use evolve_by_consist_crank
      use set_nodal_bc_id_data
      use cal_sol_vector_co_crank
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_fl
      type(MHD_MG_matrix), intent(in) :: Vmatrix
!
      integer(kind=kint), intent(in) :: i_velo, iak_diff_v
      real(kind = kreal), intent(in) :: ak_d_velo(ele%numele)
      real(kind = kreal), intent(in) :: dt
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Vmatrix%nlevel_MG)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'int_vol_viscosity_co'
      if (fl_prop%coef_imp .gt. zero) then
        call int_vol_vector_diffuse_ele(SGS_param%ifilter_final,        &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,      &
     &      diff_coefs, iak_diff_v, fl_prop%coef_imp, ak_d_velo,        &
     &      i_velo, fem_wk, f_l)
      end if
!
      if (fl_prop%coef_imp .gt. zero) then
        if (iflag_debug.eq.1) write(*,*) 'int_sk_4_fixed_velo'
        call int_sk_4_fixed_velo                                        &
     &     (cmt_param%iflag_c_velo, SGS_param%ifilter_final,            &
     &      FEM_prm%npoint_t_evo_int, i_velo, iak_diff_v,               &
     &      node, ele, nod_fld, fl_prop,                                &
     &      g_FEM, jac_3d, rhs_tbl, FEM_elens, diff_coefs,              &
     &      Vnod_bcs%nod_bc_v, Vnod_bcs%nod_bc_rot, ak_d_velo,          &
     &      fem_wk, f_l)
      end if
!
      if (fl_prop%iflag_4_coriolis .eq. id_Coriolis_ele_imp) then
        if (iflag_debug.eq.1) write(*,*) 'int_vol_coriolis_crank_ele'
        call int_vol_coriolis_crank_ele(FEM_prm%npoint_t_evo_int,       &
     &      node, ele, fluid, fl_prop, g_FEM, jac_3d, rhs_tbl,          &
     &      i_velo, nod_fld, fem_wk, f_l)
      end if
!
!
      if (     FEM_prm%iflag_imp_correct .eq. id_Crank_nicolson) then
        call cal_velo_co_lumped_crank                                   &
     &     (i_velo, dt, FEM_prm, nod_comm, node, ele, fluid, fl_prop,   &
     &      Vnod_bcs, nod_fld, iphys_ele, ele_fld, g_FEM, jac_3d,       &
     &      rhs_tbl, mlump_fl, mhd_fem_wk, fem_wk, f_l, f_nl)
      else if(FEM_prm%iflag_imp_correct .eq. id_Crank_nicolson_cmass)   &
     & then
        call cal_velo_co_consist_crank(i_velo, fl_prop%coef_velo, dt,   &
     &      FEM_prm, node, ele, fluid, Vnod_bcs, nod_fld,               &
     &      g_FEM, jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl)
      end if
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_velo_pre_crank'
      call solver_crank_vector                                          &
     &   (node, FEM_prm%MG_param, Vmatrix%nlevel_MG,                    &
     &    Vmatrix%MG_interpolate, Vmatrix%MG_comm_table,                &
     &    Vmatrix%MG_DJDS_table, Vmatrix%mat_MG_DJDS,                   &
     &    FEM_PRM%method_33, FEM_PRM%precond_33,                        &
     &    FEM_prm%eps_4_velo_crank, FEM_prm%CG11_param%MAXIT,           &
     &    i_velo, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_velocity_co_imp
!
! ----------------------------------------------------------------------
!
      subroutine cal_vector_p_co_imp(i_vecp, iak_diff_b, ak_d_magne,    &
     &          dt, FEM_prm, SGS_param, cmt_param,                      &
     &          nod_comm, node, ele, conduct, cd_prop, Bnod_bcs,        &
     &          iphys_ele, ele_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,  &
     &          diff_coefs, m_lump, Bmatrix, MG_vector,                 &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_array_for_send_recv
!
      use int_vol_diffusion_ele
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use evolve_by_lumped_crank
      use evolve_by_consist_crank
      use copy_nodal_fields
      use set_nodal_bc_id_data
      use cal_sol_vector_co_crank
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(MHD_MG_matrix), intent(in) :: Bmatrix
!
      integer(kind=kint), intent(in) :: i_vecp, iak_diff_b
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
      real(kind = kreal), intent(in) :: dt
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Bmatrix%nlevel_MG)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'int_vol_vecp_diffuse_co'
      if (cd_prop%coef_imp .gt. zero) then
        call int_vol_vector_diffuse_ele(SGS_param%ifilter_final,        &
     &      ele%istack_ele_smp, FEM_prm%npoint_t_evo_int,               &
     &      node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,      &
     &      diff_coefs, iak_diff_b, cd_prop%coef_imp, ak_d_magne,       &
     &      i_vecp, fem_wk, f_l)
      end if
!
      if (cd_prop%coef_imp .gt. 0.0d0) then
        if (iflag_debug.eq.1) write(*,*) 'int_sk_4_fixed_vector_p'
        call int_sk_4_fixed_vector(cmt_param%iflag_c_magne,             &
     &      SGS_param%ifilter_final, FEM_prm%npoint_t_evo_int,          &
     &      i_vecp, node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl,         &
     &      FEM_elens, diff_coefs, Bnod_bcs%nod_bc_a, ak_d_magne,       &
     &      cd_prop%coef_imp, iak_diff_b, fem_wk, f_l)
      end if
!
!
      if (     FEM_prm%iflag_imp_correct .eq. id_Crank_nicolson) then
        call cal_magne_co_lumped_crank                                  &
     &     (i_vecp, dt, FEM_prm, nod_comm, node, ele, nod_fld,          &
     &      iphys_ele, ele_fld, Bnod_bcs%nod_bc_a, g_FEM, jac_3d,       &
     &      rhs_tbl, m_lump, mhd_fem_wk, fem_wk, f_l, f_nl)
      else if(FEM_prm%iflag_imp_correct .eq. id_Crank_nicolson_cmass)   &
     & then
        call cal_magne_co_consist_crank(i_vecp, cd_prop%coef_magne, dt, &
     &      FEM_prm, node, ele, conduct, nod_fld, Bnod_bcs%nod_bc_a,    &
     &      g_FEM, jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_vect_p_pre_crank'
      call solver_crank_vector                                          &
     &   (node, FEM_prm%MG_param, Bmatrix%nlevel_MG,                    &
     &    Bmatrix%MG_interpolate, Bmatrix%MG_comm_table,                &
     &    Bmatrix%MG_DJDS_table, Bmatrix%mat_MG_DJDS,                   &
     &    FEM_PRM%method_33, FEM_PRM%precond_33,                        &
     &    FEM_prm%eps_4_magne_crank, FEM_prm%CG11_param%MAXIT,          &
     &    i_vecp, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_vector_p_co_imp
!
! ----------------------------------------------------------------------
!
      subroutine cal_magnetic_co_imp(i_magne, iak_diff_b, ak_d_magne,   &
     &          dt, FEM_prm, SGS_param, cmt_param,                      &
     &          nod_comm, node, ele, conduct, cd_prop, Bnod_bcs,        &
     &          iphys_ele, ele_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,  &
     &          diff_coefs,m_lump,  Bmatrix, MG_vector,                 &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_array_for_send_recv
!
      use int_vol_diffusion_ele
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use evolve_by_lumped_crank
      use evolve_by_consist_crank
      use set_nodal_bc_id_data
      use cal_sol_vector_co_crank
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(MHD_MG_matrix), intent(in) :: Bmatrix
!
      integer(kind=kint), intent(in) :: i_magne, iak_diff_b
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
      real(kind = kreal), intent(in) :: dt
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Bmatrix%nlevel_MG)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_debug.eq.1)  write(*,*) 'int_vol_magne_diffuse_co'
      if (cd_prop%coef_imp .gt. zero) then
        call int_vol_vector_diffuse_ele(SGS_param%ifilter_final,        &
     &      conduct%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &      node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,      &
     &      diff_coefs, iak_diff_b, cd_prop%coef_imp, ak_d_magne,       &
     &      i_magne, fem_wk, f_l)
      end if
!
      if (cd_prop%coef_imp .gt. zero) then
        if (iflag_debug.eq.1)  write(*,*) 'int_sk_4_fixed_magne'
        call int_sk_4_fixed_vector(cmt_param%iflag_c_magne,             &
     &      SGS_param%ifilter_final, FEM_prm%npoint_t_evo_int,          &
     &      i_magne, node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl,        &
     &      FEM_elens, diff_coefs, Bnod_bcs%nod_bc_b, ak_d_magne,       &
     &      cd_prop%coef_imp, iak_diff_b, fem_wk, f_l)
      end if
!
!
      if     (FEM_prm%iflag_imp_correct .eq. id_Crank_nicolson) then
        call cal_magne_co_lumped_crank                                  &
     &     (i_magne, dt, FEM_prm, nod_comm, node, ele, nod_fld,         &
     &      iphys_ele, ele_fld, Bnod_bcs%nod_bc_b, g_FEM, jac_3d,       &
     &      rhs_tbl, m_lump, mhd_fem_wk, fem_wk, f_l, f_nl)
      else if(FEM_prm%iflag_imp_correct .eq. id_Crank_nicolson_cmass)   &
     & then
        call cal_magne_co_consist_crank                                 &
     &     (i_magne, cd_prop%coef_magne, dt,                            &
     &      FEM_prm, node, ele, conduct, nod_fld, Bnod_bcs%nod_bc_b,    &
     &      g_FEM, jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl)
      end if
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_sol_magne_pre_crank'
      call solver_crank_vector                                          &
     &   (node, FEM_prm%MG_param, Bmatrix%nlevel_MG,                    &
     &    Bmatrix%MG_interpolate, Bmatrix%MG_comm_table,                &
     &    Bmatrix%MG_DJDS_table, Bmatrix%mat_MG_DJDS,                   &
     &    FEM_PRM%method_33, FEM_PRM%precond_33,                        &
     &    FEM_prm%eps_4_magne_crank, FEM_prm%CG11_param%MAXIT,          &
     &    i_magne, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_magnetic_co_imp
!
! -----------------------------------------------------------------------
!
      end module implicit_vector_correct
