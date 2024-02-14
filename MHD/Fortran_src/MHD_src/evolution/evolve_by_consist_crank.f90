!>@file   evolve_by_consist_crank.f90
!!@brief  module evolve_by_consist_crank
!!
!!@author  H.Matsui and H.Okuda
!!@date   Programmed in July, 2000 (ver 1.1)
!!@n      Modified in Sep., 2005
!
!>@brief Time stepinng by consistent mass matrix
!!
!!@verbatim
!!      subroutine cal_velo_pre_consist_crank                           &
!!     &         (iflag_commute_velo, ifilter_final, i_velo, i_pre_mom, &
!!     &          ak_d_velo, dt, FEM_prm, node, ele, fluid, fl_prop,    &
!!     &          Vnod_bcs, g_FEM, jac_3d, rhs_tbl, FEM_elens, ak_diff, &
!!     &          Vmatrix, MG_vector, mhd_fem_wk, fem_wk,               &
!!     &          f_l, f_nl, nod_fld, v_sol, SR_sig, SR_r)
!!      subroutine cal_vect_p_pre_consist_crank                         &
!!     &         (iflag_commute_magne, ifilter_final, i_vecp, i_pre_uxb,&
!!     &          ak_d_magne, nod_bc_a, dt, FEM_prm, node, ele, conduct,&
!!     &          cd_prop, g_FEM, jac_3d, rhs_tbl, FEM_elens, ak_diff,  &
!!     &          Bmatrix, MG_vector, mhd_fem_wk, fem_wk, f_l, f_nl,    &
!!     &          nod_fld, v_sol, SR_sig, SR_r)
!!       subroutine cal_magne_pre_consist_crank                         &
!!     &         (iflag_commute_magne, ifilter_final,                   &
!!     &          i_magne, i_pre_uxb, ak_d_magne, nod_bc_b,             &
!!     &          dt, FEM_prm, node, ele, conduct, cd_prop,             &
!!     &          g_FEM, jac_3d, rhs_tbl, FEM_elens, ak_diff,           &
!!     &          Bmatrix, MG_vector, mhd_fem_wk, fem_wk,               &
!!     &          f_l, f_nl, nod_fld, v_sol, SR_sig, SR_r)
!!
!!      subroutine cal_temp_pre_consist_crank                           &
!!     &         (iflag_commute_field, ifilter_final, i_field,          &
!!     &          i_pre_advect, ak_diffuese, eps_4_crank, dt,           &
!!     &          FEM_prm, node, ele, fluid, property, Snod_bcs,        &
!!     &          g_FEM, jac_3d, rhs_tbl,FEM_elens, ak_diff, matrix,    &
!!     &          MG_vector, mhd_fem_wk, fem_wk, f_l, f_nl,             &
!!     &          nod_fld, v_sol, SR_sig, SR_r)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: property
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(nodal_bcs_4_scalar_type), intent(in) :: nod_bcs
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_a
!!        type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_b
!!        type(MHD_MG_matrix), intent(in) :: Vmatrix
!!        type(MHD_MG_matrix), intent(in) :: Bmatrix
!!        type(MHD_MG_matrix), intent(in) :: Tmatrix
!!        type(MHD_MG_matrix), intent(in) :: matrix
!!        type(vectors_4_solver), intent(inout)                         &
!!       &           :: MG_vector(0:num_MG_level)
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module evolve_by_consist_crank
!
      use m_precision
      use calypso_mpi
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
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_nodal_bc_data
      use t_vector_for_solver
      use t_solver_djds
      use t_solver_djds_MHD
      use t_interpolate_table
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
      subroutine cal_velo_pre_consist_crank                             &
     &         (iflag_commute_velo, ifilter_final, i_velo, i_pre_mom,   &
     &          ak_d_velo, dt, FEM_prm, node, ele, fluid, fl_prop,      &
     &          Vnod_bcs, g_FEM, jac_3d, rhs_tbl, FEM_elens, ak_diff,   &
     &          Vmatrix, MG_vector, mhd_fem_wk, fem_wk,                 &
     &          f_l, f_nl, nod_fld, v_sol, SR_sig, SR_r)
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
      integer(kind = kint), intent(in) :: iflag_commute_velo
      integer(kind = kint), intent(in) :: ifilter_final
      integer(kind = kint), intent(in) :: i_velo, i_pre_mom
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_MG_matrix), intent(in) :: Vmatrix
!
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: ak_d_velo(ele%numele)
      real(kind = kreal), intent(in) :: ak_diff(ele%numele)
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
      if (fl_prop%coef_imp .gt. zero) then
        call int_sk_4_fixed_velo(iflag_commute_velo, ifilter_final,     &
     &      FEM_prm%npoint_t_evo_int, i_velo, node, ele, nod_fld,       &
     &      fl_prop, g_FEM, jac_3d, rhs_tbl, FEM_elens,                 &
     &      Vnod_bcs%nod_bc_v, Vnod_bcs%nod_bc_rot, ak_d_velo,          &
     &      ak_diff, fem_wk, f_l)
      end if
!
      call reset_ff_t_smp(node%max_nod_smp, mhd_fem_wk)
!
      call int_vol_initial_vector(FEM_prm%npoint_t_evo_int,             &
     &    fluid%istack_ele_fld_smp, i_velo, fl_prop%coef_velo,          &
     &    node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl,                   &
     &    fem_wk, mhd_fem_wk)
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
!
      call set_boundary_velo_4_rhs(node, Vnod_bcs, f_l, f_nl)
!
      call cal_vector_pre_consist(dt, node, fl_prop%coef_velo,          &
     &    n_vector, i_pre_mom, nod_fld, rhs_tbl, mhd_fem_wk, f_nl, f_l)
!
      call solver_crank_vector                                          &
     &   (node, FEM_prm%MG_param, Vmatrix%nlevel_MG,                    &
     &    Vmatrix%MG_interpolate, Vmatrix%MG_comm_table,                &
     &    Vmatrix%MG_DJDS_table, Vmatrix%mat_MG_DJDS,                   &
     &    FEM_PRM%method_33, FEM_PRM%precond_33,                        &
     &    FEM_prm%eps_4_velo_crank, FEM_prm%CG11_param%MAXIT,           &
     &    i_velo, MG_vector, f_l, v_sol%b_vec, nod_fld,                 &
     &    v_sol%x_vec, SR_sig, SR_r)
!
      end subroutine cal_velo_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_vect_p_pre_consist_crank                           &
     &         (iflag_commute_magne, ifilter_final, i_vecp, i_pre_uxb,  &
     &          ak_d_magne, nod_bc_a, dt, FEM_prm, node, ele, conduct,  &
     &          cd_prop, g_FEM, jac_3d, rhs_tbl, FEM_elens, ak_diff,    &
     &          Bmatrix, MG_vector, mhd_fem_wk, fem_wk, f_l, f_nl,      &
     &          nod_fld, v_sol, SR_sig, SR_r)
!
      use cal_sol_vector_pre_crank
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
      use set_boundary_scalars
      use copy_nodal_fields
!
      integer(kind = kint), intent(in) :: iflag_commute_magne
      integer(kind = kint), intent(in) :: ifilter_final
      integer(kind = kint), intent(in) :: i_vecp, i_pre_uxb
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_a
      type(MHD_MG_matrix), intent(in) :: Bmatrix
!
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
      real(kind = kreal), intent(in) :: ak_diff(ele%numele)
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
      if (cd_prop%coef_imp .gt. 0.0d0) then
        call int_sk_4_fixed_vector(iflag_commute_magne,                 &
     &      ifilter_final, FEM_prm%npoint_t_evo_int,                    &
     &      i_vecp, node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl,         &
     &      FEM_elens, nod_bc_a, ak_d_magne, cd_prop%coef_imp,          &
     &      ak_diff, fem_wk, f_l)
      end if
!
      call reset_ff_t_smp(node%max_nod_smp, mhd_fem_wk)
!
      call int_vol_initial_vector(FEM_prm%npoint_t_evo_int,             &
     &    conduct%istack_ele_fld_smp, i_vecp, cd_prop%coef_magne,       &
     &    node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl,                   &
     &    fem_wk,  mhd_fem_wk)
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
!
      call delete_vector_ffs_on_bc(node, nod_bc_a, f_l, f_nl)
!
      call cal_vector_pre_consist(dt, node, cd_prop%coef_magne,         &
     &    n_vector, i_pre_uxb, nod_fld, rhs_tbl,                        &
     &    mhd_fem_wk, f_nl, f_l)
!
      if (iflag_debug.eq.1) write(*,*) 'time_evolution'
      call solver_crank_vector                                          &
     &   (node, FEM_prm%MG_param, Bmatrix%nlevel_MG,                    &
     &    Bmatrix%MG_interpolate, Bmatrix%MG_comm_table,                &
     &    Bmatrix%MG_DJDS_table, Bmatrix%mat_MG_DJDS,                   &
     &    FEM_PRM%method_33, FEM_PRM%precond_33,                        &
     &    FEM_prm%eps_4_magne_crank, FEM_prm%CG11_param%MAXIT,          &
     &    i_vecp, MG_vector, f_l, v_sol%b_vec, nod_fld,                 &
     &    v_sol%x_vec, SR_sig, SR_r)
!
      end subroutine cal_vect_p_pre_consist_crank
!
! ----------------------------------------------------------------------
!
       subroutine cal_magne_pre_consist_crank                           &
     &         (iflag_commute_magne, ifilter_final,                     &
     &          i_magne, i_pre_uxb, ak_d_magne, nod_bc_b,               &
     &          dt, FEM_prm, node, ele, conduct, cd_prop,               &
     &          g_FEM, jac_3d, rhs_tbl, FEM_elens, ak_diff,             &
     &          Bmatrix, MG_vector, mhd_fem_wk, fem_wk,                 &
     &          f_l, f_nl, nod_fld, v_sol, SR_sig, SR_r)
!
      use cal_sol_vector_pre_crank
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
      use set_boundary_scalars
!
      integer(kind = kint), intent(in) :: i_magne, i_pre_uxb
      integer(kind = kint), intent(in) :: ifilter_final
!
      integer(kind = kint), intent(in) :: iflag_commute_magne
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_b
      type(MHD_MG_matrix), intent(in) :: Bmatrix
!
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
      real(kind = kreal), intent(in) :: ak_diff(ele%numele)
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
      if (cd_prop%coef_imp .gt. 0.0d0) then
        call int_sk_4_fixed_vector(iflag_commute_magne,                 &
     &      ifilter_final, FEM_prm%npoint_t_evo_int,                    &
     &      i_magne, node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl,        &
     &      FEM_elens, nod_bc_b, ak_d_magne, cd_prop%coef_imp,          &
     &      ak_diff, fem_wk, f_l)
      end if
!
      call reset_ff_t_smp(node%max_nod_smp, mhd_fem_wk)
      call int_vol_initial_vector(FEM_prm%npoint_t_evo_int,             &
     &    conduct%istack_ele_fld_smp, i_magne, cd_prop%coef_magne,      &
     &    node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl,                   &
     &    fem_wk, mhd_fem_wk)
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'bc_4_magne_rhs'
      call delete_vector_ffs_on_bc(node, nod_bc_b, f_l, f_nl)
!
      call cal_vector_pre_consist(dt, node, cd_prop%coef_magne,         &
     &    n_vector, i_pre_uxb, nod_fld, rhs_tbl,                        &
     &    mhd_fem_wk, f_nl, f_l)
!
      if (iflag_debug.eq.1)                                             &
     &        write(*,*) 'time_evolution for magnetic field'
      call solver_crank_vector                                          &
     &   (node, FEM_prm%MG_param, Bmatrix%nlevel_MG,                    &
     &    Bmatrix%MG_interpolate, Bmatrix%MG_comm_table,                &
     &    Bmatrix%MG_DJDS_table, Bmatrix%mat_MG_DJDS,                   &
     &    FEM_PRM%method_33, FEM_PRM%precond_33,                        &
     &    FEM_prm%eps_4_magne_crank, FEM_prm%CG11_param%MAXIT,          &
     &    i_magne, MG_vector, f_l, v_sol%b_vec, nod_fld,                &
     &    v_sol%x_vec, SR_sig, SR_r)
!
       end subroutine cal_magne_pre_consist_crank
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_temp_pre_consist_crank                             &
     &         (iflag_commute_field, ifilter_final, i_field,            &
     &          i_pre_advect, ak_diffuese, eps_4_crank, dt,             &
     &          FEM_prm, node, ele, fluid, property, Snod_bcs,          &
     &          g_FEM, jac_3d, rhs_tbl,FEM_elens, ak_diff, matrix,      &
     &          MG_vector, mhd_fem_wk, fem_wk, f_l, f_nl,               &
     &          nod_fld, v_sol, SR_sig, SR_r)
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
      integer(kind = kint), intent(in) :: iflag_commute_field
      integer(kind = kint), intent(in) :: ifilter_final
      integer(kind = kint), intent(in) :: i_field, i_pre_advect
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(scalar_property), intent(in) :: property
      type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_MG_matrix), intent(in) :: matrix
!
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: eps_4_crank
      real(kind = kreal), intent(in) :: ak_diffuese(ele%numele)
      real(kind = kreal), intent(in) :: ak_diff(ele%numele)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:matrix%nlevel_MG)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if (property%coef_imp .gt. zero) then
        call int_sk_fixed_temp(iflag_commute_field,                     &
     &      ifilter_final, FEM_prm%npoint_t_evo_int, i_field,           &
     &      node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,      &
     &      Snod_bcs%nod_bc_s, ak_diffuese, ak_diff, property%coef_imp, &
     &      fem_wk, f_l)
!        if (MHD_step1%iflag_initial_step.eq.1) then
!          property%coef_imp = 1.0d0 / property%coef_imp
!        end if
      end if
!
      call reset_ff_t_smp(node%max_nod_smp, mhd_fem_wk)
!
      call int_vol_initial_scalar(FEM_prm%npoint_t_evo_int,             &
     &    fluid%istack_ele_fld_smp, i_field, property%coef_advect,      &
     &    node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl,                   &
     &    fem_wk, mhd_fem_wk)
      call set_ff_nl_smp_2_ff(n_scalar, node, rhs_tbl, f_l, f_nl)
!
      call set_boundary_rhs_scalar(node, Snod_bcs%nod_bc_s, f_l, f_nl)
!
      call cal_vector_pre_consist(dt, node, property%coef_advect,       &
     &    n_scalar, i_pre_advect, nod_fld, rhs_tbl,                     &
     &    mhd_fem_wk, f_nl, f_l)
!
      call solver_crank_scalar                                          &
     &   (node, FEM_prm%MG_param, matrix%nlevel_MG,                     &
     &    matrix%MG_interpolate, matrix%MG_comm_table,                  &
     &    matrix%MG_DJDS_table, matrix%mat_MG_DJDS,                     &
     &    FEM_PRM%CG11_param%METHOD, FEM_PRM%CG11_param%PRECOND,        &
     &    eps_4_crank, FEM_prm%CG11_param%MAXIT,                        &
     &    i_field, MG_vector, f_l, v_sol%b_vec, nod_fld,                &
     &    v_sol%x_vec, SR_sig, SR_r)
!
      end subroutine cal_temp_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      end module evolve_by_consist_crank
