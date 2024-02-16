!>@file   implicit_vector_correct.f90
!!@brief  module implicit_vector_correct
!!
!!@author  H.Matsui and H.Okuda
!!@date   Programmed in July, 2000 (ver 1.1)
!!@n      Modified in Sep., 2005
!
!>@brief Correction by poisson equation
!!
!!@verbatim
!!      subroutine cal_velocity_co_exp(i_velo, i_p_phi, FEM_prm,        &
!!     &          nod_comm, node, ele, fluid, g_FEM, jac_3d, rhs_tbl,   &
!!     &          mlump_fl, mhd_fem_wk, fem_wk, f_l, f_nl,              &
!!     &          nod_fld, v_sol, SR_sig, SR_r)
!!      subroutine cal_vector_p_co_exp                                  &
!!     &         (i_vecp, FEM_prm, nod_comm, node, ele,                 &
!!     &          g_FEM, jac_3d, rhs_tbl, m_lump, mhd_fem_wk, fem_wk,   &
!!     &          f_l, f_nl, nod_fld, v_sol, SR_sig, SR_r)
!!      subroutine cal_magnetic_co_exp                                  &
!!     &         (i_magne, FEM_prm, nod_comm, node, ele,                &
!!     &          g_FEM, jac_3d, rhs_tbl, m_lump, mhd_fem_wk, fem_wk,   &
!!     &          f_l, f_nl, nod_fld, v_sol, SR_sig, SR_r)
!!
!!      subroutine cal_velocity_co_imp                                  &
!!     &         (i_velo, ak_d_velo, dt, FEM_prm, SGS_param,            &
!!     &          nod_comm, node, ele, fluid, fl_prop, Vnod_bcs,        &
!!     &          iphys_ele_base, ele_fld, g_FEM, jac_3d, rhs_tbl,      &
!!     &          FEM_elens, Cdiff_velo, mlump_fl, Vmatrix, MG_vector,  &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,               &
!!     &          v_sol, SR_sig, SR_r)
!!      subroutine cal_vector_p_co_imp(i_vecp, ak_d_magne,              &
!!     &          dt, FEM_prm, SGS_param, cmt_param,                    &
!!     &          nod_comm, node, ele, conduct, cd_prop, Bnod_bcs,      &
!!     &          iphys_ele_base, ele_fld, g_FEM, jac_3d, rhs_tbl,      &
!!     &          FEM_elens, Cdiff_magne, m_lump, Bmatrix, MG_vector,   &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,               &
!!     &          v_sol, SR_sig, SR_r)
!!      subroutine cal_magnetic_co_imp(i_magne, ak_d_magne,             &
!!     &          dt, FEM_prm, SGS_param, cmt_param,                    &
!!     &          nod_comm, node, ele, conduct, cd_prop, Bnod_bcs,      &
!!     &          iphys_ele_base, ele_fld, g_FEM, jac_3d, rhs_tbl,      &
!!     &          FEM_elens, Cdiff_magne, m_lump, Bmatrix, MG_vector,   &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,               &
!!     &          v_sol, SR_sig, SR_r)
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
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_model_coefficient), intent(in) :: Cdiff_velo
!!        type(SGS_model_coefficient), intent(in) :: Cdiff_magne
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(MHD_MG_matrix), intent(in) :: Vmatrix
!!        type(MHD_MG_matrix), intent(in) :: Bmatrix
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module implicit_vector_correct
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
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
      use t_jacobians
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_vector_for_solver
      use t_solver_djds
      use t_solver_djds_MHD
      use t_bc_data_velo
      use t_bc_data_magne
      use t_vector_for_solver
      use t_solver_SR
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
     &          mlump_fl, mhd_fem_wk, fem_wk, f_l, f_nl,                &
     &          nod_fld, v_sol, SR_sig, SR_r)
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
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_multi_pass_4_vector_fl'
      call cal_multi_pass_4_vector_ff(fluid%istack_ele_fld_smp,         &
     &    FEM_prm, mlump_fl, nod_comm, node, ele, g_FEM, jac_3d,        &
     &    rhs_tbl, mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl,              &
     &    v_sol, SR_sig, SR_r)
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
     &          f_l, f_nl, nod_fld, v_sol, SR_sig, SR_r)
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
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_multi_pass_4_vector_ff'
      call cal_multi_pass_4_vector_ff                                   &
     &   (ele%istack_ele_smp, FEM_prm, m_lump,                          &
     &    nod_comm, node, ele, g_FEM, jac_3d, rhs_tbl,                  &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl,                       &
     &    v_sol, SR_sig, SR_r)
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
     &          f_l, f_nl, nod_fld, v_sol, SR_sig, SR_r)
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
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_multi_pass_4_vector_ff'
      call cal_multi_pass_4_vector_ff                                   &
     &   (ele%istack_ele_smp, FEM_prm, m_lump,                          &
     &    nod_comm, node, ele, g_FEM, jac_3d, rhs_tbl,                  &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl,                       &
     &    v_sol, SR_sig, SR_r)
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
      subroutine cal_velocity_co_imp                                    &
     &         (i_velo, ak_d_velo, dt, FEM_prm, SGS_param,              &
     &          nod_comm, node, ele, fluid, fl_prop, Vnod_bcs,          &
     &          iphys_ele_base, ele_fld, g_FEM, jac_3d, rhs_tbl,        &
     &          FEM_elens, Cdiff_velo, mlump_fl, Vmatrix, MG_vector,    &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,                 &
     &          v_sol, SR_sig, SR_r)
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
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_model_coefficient), intent(in) :: Cdiff_velo
      type(lumped_mass_matrices), intent(in) :: mlump_fl
      type(MHD_MG_matrix), intent(in) :: Vmatrix
!
      integer(kind=kint), intent(in) :: i_velo
      real(kind = kreal), intent(in) :: ak_d_velo(ele%numele)
      real(kind = kreal), intent(in) :: dt
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Vmatrix%nlevel_MG)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if (iflag_debug.eq.1) write(*,*) 'int_vol_viscosity_co'
      if (fl_prop%coef_imp .gt. zero) then
        call int_vol_vector_diffuse_ele(SGS_param%ifilter_final,        &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,      &
     &      Cdiff_velo, fl_prop%coef_imp, ak_d_velo, i_velo,            &
     &      fem_wk, f_l)
      end if
!
      if (fl_prop%coef_imp .gt. zero) then
        if (iflag_debug.eq.1) write(*,*) 'int_sk_4_fixed_velo'
        call int_sk_4_fixed_velo                                        &
     &     (SGS_param%SGS_momentum%iflag_commute_field,                 &
     &      SGS_param%ifilter_final, FEM_prm%npoint_t_evo_int,          &
     &      i_velo, node, ele, nod_fld, fl_prop, g_FEM, jac_3d,         &
     &      rhs_tbl, FEM_elens, Vnod_bcs%nod_bc_v, Vnod_bcs%nod_bc_rot, &
     &      ak_d_velo, Cdiff_velo%coef(1,1), fem_wk, f_l)
      end if
!
      if(fl_prop%iflag_4_coriolis .and. fl_prop%iflag_coriolis_implicit &
     &    .and. fl_prop%iflag_FEM_coriolis .eq. id_FORCE_ele_int) then
        if (iflag_debug.eq.1) write(*,*) 'int_vol_coriolis_ele'
        call int_vol_coriolis_ele(FEM_prm%npoint_t_evo_int,             &
     &      node, ele, fluid, fl_prop, g_FEM, jac_3d, rhs_tbl,          &
     &      i_velo, nod_fld, fem_wk, f_l)
      end if
!
!
      if (     FEM_prm%iflag_imp_correct .eq. id_Crank_nicolson) then
        call cal_velo_co_lumped_crank                                   &
     &     (i_velo, dt, FEM_prm, nod_comm, node, ele, fluid, fl_prop,   &
     &      Vnod_bcs, nod_fld, iphys_ele_base, ele_fld, g_FEM, jac_3d,  &
     &      rhs_tbl, mlump_fl, mhd_fem_wk, fem_wk, f_l, f_nl,           &
     &      v_sol, SR_sig, SR_r)
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
     &    i_velo, MG_vector, f_l, v_sol%b_vec, nod_fld,                 &
     &    v_sol%x_vec, SR_sig, SR_r)
!
      end subroutine cal_velocity_co_imp
!
! ----------------------------------------------------------------------
!
      subroutine cal_vector_p_co_imp(i_vecp, ak_d_magne,                &
     &          dt, FEM_prm, SGS_param, cmt_param,                      &
     &          nod_comm, node, ele, conduct, cd_prop, Bnod_bcs,        &
     &          iphys_ele_base, ele_fld, g_FEM, jac_3d, rhs_tbl,        &
     &          FEM_elens, Cdiff_magne, m_lump, Bmatrix, MG_vector,     &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,                 &
     &          v_sol, SR_sig, SR_r)
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
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_model_coefficient), intent(in) :: Cdiff_magne
      type(MHD_MG_matrix), intent(in) :: Bmatrix
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind=kint), intent(in) :: i_vecp
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
      real(kind = kreal), intent(in) :: dt
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Bmatrix%nlevel_MG)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      if (iflag_debug.eq.1) write(*,*) 'int_vol_vecp_diffuse_co'
      if (cd_prop%coef_imp .gt. zero) then
        call int_vol_vector_diffuse_ele(SGS_param%ifilter_final,        &
     &      ele%istack_ele_smp, FEM_prm%npoint_t_evo_int,               &
     &      node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,      &
     &      Cdiff_magne, cd_prop%coef_imp, ak_d_magne, i_vecp,          &
     &      fem_wk, f_l)
      end if
!
      if (cd_prop%coef_imp .gt. 0.0d0) then
        if (iflag_debug.eq.1) write(*,*) 'int_sk_4_fixed_vector_p'
        call int_sk_4_fixed_vector(cmt_param%iflag_c_magne,             &
     &      SGS_param%ifilter_final, FEM_prm%npoint_t_evo_int,          &
     &      i_vecp, node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl,         &
     &      FEM_elens, Bnod_bcs%nod_bc_a, ak_d_magne,                   &
     &      cd_prop%coef_imp, Cdiff_magne%coef(1,1), fem_wk, f_l)
      end if
!
!
      if (     FEM_prm%iflag_imp_correct .eq. id_Crank_nicolson) then
        call cal_magne_co_lumped_crank                                  &
     &     (i_vecp, dt, FEM_prm, nod_comm, node, ele, nod_fld,          &
     &      iphys_ele_base, ele_fld, Bnod_bcs%nod_bc_a, g_FEM, jac_3d,  &
     &      rhs_tbl, m_lump, mhd_fem_wk, fem_wk, f_l, f_nl,             &
     &      v_sol, SR_sig, SR_r)
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
     &    i_vecp, MG_vector, f_l, v_sol%b_vec, nod_fld,                 &
     &    v_sol%x_vec, SR_sig, SR_r)
!
      end subroutine cal_vector_p_co_imp
!
! ----------------------------------------------------------------------
!
      subroutine cal_magnetic_co_imp(i_magne, ak_d_magne,               &
     &          dt, FEM_prm, SGS_param, cmt_param,                      &
     &          nod_comm, node, ele, conduct, cd_prop, Bnod_bcs,        &
     &          iphys_ele_base, ele_fld, g_FEM, jac_3d, rhs_tbl,        &
     &          FEM_elens, Cdiff_magne, m_lump, Bmatrix, MG_vector,     &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,                 &
     &          v_sol, SR_sig, SR_r)
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
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_model_coefficient), intent(in) :: Cdiff_magne
      type(MHD_MG_matrix), intent(in) :: Bmatrix
!
      integer(kind=kint), intent(in) :: i_magne
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
      real(kind = kreal), intent(in) :: dt
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Bmatrix%nlevel_MG)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if (iflag_debug.eq.1)  write(*,*) 'int_vol_magne_diffuse_co'
      if (cd_prop%coef_imp .gt. zero) then
        call int_vol_vector_diffuse_ele(SGS_param%ifilter_final,        &
     &      conduct%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &      node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,      &
     &      Cdiff_magne, cd_prop%coef_imp, ak_d_magne, i_magne,         &
     &      fem_wk, f_l)
      end if
!
      if (cd_prop%coef_imp .gt. zero) then
        if (iflag_debug.eq.1)  write(*,*) 'int_sk_4_fixed_magne'
        call int_sk_4_fixed_vector(cmt_param%iflag_c_magne,             &
     &      SGS_param%ifilter_final, FEM_prm%npoint_t_evo_int,          &
     &      i_magne, node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl,        &
     &      FEM_elens, Bnod_bcs%nod_bc_b, ak_d_magne,                   &
     &      cd_prop%coef_imp, Cdiff_magne%coef(1,1), fem_wk, f_l)
      end if
!
!
      if     (FEM_prm%iflag_imp_correct .eq. id_Crank_nicolson) then
        call cal_magne_co_lumped_crank                                  &
     &     (i_magne, dt, FEM_prm, nod_comm, node, ele, nod_fld,         &
     &      iphys_ele_base, ele_fld, Bnod_bcs%nod_bc_b, g_FEM, jac_3d,  &
     &      rhs_tbl, m_lump, mhd_fem_wk, fem_wk, f_l, f_nl,             &
     &      v_sol, SR_sig, SR_r)
      else if(FEM_prm%iflag_imp_correct .eq. id_Crank_nicolson_cmass)   &
     & then
        call cal_magne_co_consist_crank                                 &
     &     (i_magne, cd_prop%coef_magne, dt, FEM_prm,                   &
     &      node, ele, conduct, nod_fld, Bnod_bcs%nod_bc_b, g_FEM,      &
     &      jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl)
      end if
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_sol_magne_pre_crank'
      call solver_crank_vector                                          &
     &   (node, FEM_prm%MG_param, Bmatrix%nlevel_MG,                    &
     &    Bmatrix%MG_interpolate, Bmatrix%MG_comm_table,                &
     &    Bmatrix%MG_DJDS_table, Bmatrix%mat_MG_DJDS,                   &
     &    FEM_PRM%method_33, FEM_PRM%precond_33,                        &
     &    FEM_prm%eps_4_magne_crank, FEM_prm%CG11_param%MAXIT,          &
     &    i_magne, MG_vector, f_l, v_sol%b_vec, nod_fld,                &
     &    v_sol%x_vec, SR_sig, SR_r)
!
      end subroutine cal_magnetic_co_imp
!
! -----------------------------------------------------------------------
!
      end module implicit_vector_correct
