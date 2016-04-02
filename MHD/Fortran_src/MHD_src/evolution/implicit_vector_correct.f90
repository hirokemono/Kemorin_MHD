!implicit_vector_correct.f90
!      module implicit_vector_correct
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on March, 2006
!
!!      subroutine cal_velocity_co_exp(i_velo, i_p_phi,                 &
!!     &          nod_comm, node, ele, fluid, jac_3d, rhs_tbl,          &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_vector_p_co_exp(i_vecp, nod_comm, node, ele,     &
!!     &          jac_3d, rhs_tbl, m_lump, mhd_fem_wk, fem_wk,          &
!!     &          f_l, f_nl, nod_fld)
!!      subroutine cal_magnetic_co_exp(i_magne, nod_comm, node, ele,    &
!!     &          jac_3d, rhs_tbl, m_lump, mhd_fem_wk, fem_wk,          &
!!     &          f_l, f_nl, nod_fld)
!!
!!      subroutine cal_velocity_co_imp(i_velo,                          &
!!     &          nod_comm, node, ele, fluid, Vnod_bcs,                 &
!!     &          iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          diff_coefs, num_MG_level, MG_interpolate,             &
!!     &          MG_comm_fluid, MG_DJDS_fluid, Vmat_MG_DJDS, MG_vector,&
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_vector_p_co_imp(i_vecp,                          &
!!     &          nod_comm, node, ele, conduct, Bnod_bcs,               &
!!     &          iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          diff_coefs, num_MG_level, MG_interpolate,             &
!!     &          MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS, MG_vector,&
!!     &          m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_magnetic_co_imp(i_magne,                         &
!!     &          nod_comm, node, ele, conduct, Bnod_bcs,               &
!!     &          iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          diff_coefs, num_MG_level, MG_interpolate,             &
!!     &          MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS, MG_vector,&
!!     &          m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(MHD_coefficients_type), intent(in) :: diff_coefs
!!        type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
!!        type(communication_table), intent(in)                         &
!!       &           :: MG_comm_table(0:num_MG_level)
!!        type(communication_table), intent(in)                         &
!!       &           :: MG_comm_fluid(0:num_MG_level)
!!        type(DJDS_ordering_table), intent(in)                         &
!!       &           :: MG_DJDS_table(0:num_MG_level)
!!        type(DJDS_ordering_table), intent(in)                         &
!!       &           :: MG_DJDS_fluid(0:num_MG_level)
!!        type(vectors_4_solver), intent(inout)                         &
!!       &           :: MG_vector(0:num_MG_level)
!!        type(DJDS_MATRIX), intent(in) :: Vmat_MG_DJDS(0:num_MG_level)
!!        type(DJDS_MATRIX), intent(in) :: Bmat_MG_DJDS(0:num_MG_level)
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
      use m_control_parameter
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
      use t_solver_djds
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
      subroutine cal_velocity_co_exp(i_velo, i_p_phi,                   &
     &          nod_comm, node, ele, fluid, jac_3d, rhs_tbl,            &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use cal_multi_pass
      use cal_sol_vector_correct
!
      integer(kind=kint), intent(in) :: i_velo, i_p_phi
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_multi_pass_4_vector_fl'
      call cal_multi_pass_4_vector_ff                                   &
     &   (fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl,                &
     &    nod_comm, node, ele, jac_3d, rhs_tbl,                         &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_velo_co'
      call cal_sol_velocity_co                                          &
     &   (nod_fld%n_point, node%istack_internal_smp,                    &
     &    mhd_fem_wk%mlump_fl%ml, f_l%ff, nod_fld%ntot_phys,            &
     &    i_velo, i_p_phi, nod_fld%d_fld)
!
      end subroutine cal_velocity_co_exp
!
! ----------------------------------------------------------------------
!
      subroutine cal_vector_p_co_exp(i_vecp, nod_comm, node, ele,       &
     &          jac_3d, rhs_tbl, m_lump, mhd_fem_wk, fem_wk,            &
     &          f_l, f_nl, nod_fld)
!
      use cal_multi_pass
      use cal_sol_vector_correct
!
      integer(kind=kint), intent(in) :: i_vecp
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
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
      call cal_multi_pass_4_vector_ff(ele%istack_ele_smp, m_lump,       &
     &    nod_comm, node, ele, jac_3d, rhs_tbl,                         &
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
      subroutine cal_magnetic_co_exp(i_magne, nod_comm, node, ele,      &
     &          jac_3d, rhs_tbl, m_lump, mhd_fem_wk, fem_wk,            &
     &          f_l, f_nl, nod_fld)
!
      use cal_multi_pass
      use cal_sol_vector_correct
!
      integer(kind=kint), intent(in) :: i_magne
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
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
      call cal_multi_pass_4_vector_ff(ele%istack_ele_smp, m_lump,      &
     &    nod_comm, node, ele, jac_3d, rhs_tbl,                        &
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
      subroutine cal_velocity_co_imp(i_velo,                            &
     &          nod_comm, node, ele, fluid, Vnod_bcs,                   &
     &          iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          diff_coefs, num_MG_level, MG_interpolate,               &
     &          MG_comm_fluid, MG_DJDS_fluid, Vmat_MG_DJDS, MG_vector,  &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
!
      use m_ele_material_property
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_SGS_address
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
      integer(kind=kint), intent(in) :: i_velo
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
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
      if (iflag_debug.eq.1) write(*,*) 'int_vol_viscosity_co'
      if (coef_imp_v.gt.zero) then
        call int_vol_vector_diffuse_ele(fluid%istack_ele_fld_smp,       &
     &      node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,             &
     &      diff_coefs, iak_diff_v, coef_imp_v, ak_d_velo, i_velo,      &
     &      fem_wk, f_l)
      end if
!
      if (coef_imp_v.gt.0.0d0) then
        if (iflag_debug.eq.1) write(*,*) 'int_sk_4_fixed_velo'
        call int_sk_4_fixed_velo(i_velo, iak_diff_v, node, ele,         &
     &      nod_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs,            &
     &      Vnod_bcs%nod_bc_v, Vnod_bcs%nod_bc_rot, fem_wk, f_l)
      end if
!
      if ( iflag_4_coriolis .eq. id_Coriolis_ele_imp) then
        if (iflag_debug.eq.1) write(*,*) 'int_vol_coriolis_crank_ele'
        call int_vol_coriolis_crank_ele(node, ele, fluid, jac_3d,       &
     &      rhs_tbl, i_velo, nod_fld, fem_wk, f_l)
      end if
!
!
      if (     iflag_implicit_correct.eq.3) then
        call cal_velo_co_lumped_crank                                   &
     &     (i_velo, nod_comm, node, ele, fluid, Vnod_bcs, nod_fld,      &
     &      iphys_ele, ele_fld, jac_3d, rhs_tbl,                        &
     &      mhd_fem_wk, fem_wk, f_l, f_nl)
      else if (iflag_implicit_correct.eq.4) then
        call cal_velo_co_consist_crank(i_velo, coef_velo,               &
     &      node, ele, fluid, Vnod_bcs, nod_fld, jac_3d,                &
     &      rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl)
      end if
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_velo_pre_crank'
      call solver_crank_vector(node, num_MG_level,                      &
     &    MG_interpolate, MG_comm_fluid, MG_DJDS_fluid, Vmat_MG_DJDS,   &
     &    method_4_velo, precond_4_crank, eps_4_velo_crank, itr,        &
     &    i_velo, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_velocity_co_imp
!
! ----------------------------------------------------------------------
!
      subroutine cal_vector_p_co_imp(i_vecp,                            &
     &          nod_comm, node, ele, conduct, Bnod_bcs,                 &
     &          iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          diff_coefs, num_MG_level, MG_interpolate,               &
     &          MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS, MG_vector,  &
     &          m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_SGS_address
      use m_ele_material_property
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
!
      integer(kind=kint), intent(in) :: i_vecp
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_coefficients_type), intent(in) :: diff_coefs
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
      if (iflag_debug.eq.1) write(*,*) 'int_vol_vecp_diffuse_co'
      if (coef_imp_b.gt.zero) then
        call int_vol_vector_diffuse_ele(ele%istack_ele_smp,             &
     &      node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,             &
     &      diff_coefs, iak_diff_b, coef_imp_b, ak_d_magne,             &
     &      i_vecp, fem_wk, f_l)
      end if
!
      if (coef_imp_b.gt.0.0d0) then
        if (iflag_debug.eq.1) write(*,*) 'int_sk_4_fixed_vector_p'
        call int_sk_4_fixed_vector(iflag_commute_magne,                 &
     &      i_vecp, node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,     &
     &      diff_coefs, Bnod_bcs%nod_bc_a, ak_d_magne,                  &
     &      coef_imp_b, iak_diff_b, fem_wk, f_l)
      end if
!
!
      if (     iflag_implicit_correct.eq.3) then
        call cal_magne_co_lumped_crank                                  &
     &     (i_vecp, nod_comm, node, ele, nod_fld,                       &
     &      iphys_ele, ele_fld, Bnod_bcs%nod_bc_a, jac_3d, rhs_tbl,     &
     &      m_lump, mhd_fem_wk, fem_wk, f_l, f_nl)
      else if (iflag_implicit_correct.eq.4) then
        call cal_magne_co_consist_crank(i_vecp, coef_magne,             &
     &      node, ele, conduct, nod_fld, Bnod_bcs%nod_bc_a, jac_3d,     &
     &      rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_vect_p_pre_crank'
      call solver_crank_vector(node, num_MG_level,                      &
     &    MG_interpolate, MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS,   &
     &    method_4_velo, precond_4_crank, eps_4_magne_crank, itr,       &
     &    i_vecp, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_vector_p_co_imp
!
! ----------------------------------------------------------------------
!
      subroutine cal_magnetic_co_imp(i_magne,                           &
     &          nod_comm, node, ele, conduct, Bnod_bcs,                 &
     &          iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          diff_coefs, num_MG_level, MG_interpolate,               &
     &          MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS, MG_vector,  &
     &          m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_SGS_address
      use m_ele_material_property
!
      use int_vol_diffusion_ele
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use evolve_by_lumped_crank
      use evolve_by_consist_crank
      use set_nodal_bc_id_data
      use cal_sol_vector_co_crank
!
!
      integer(kind=kint), intent(in) :: i_magne
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_coefficients_type), intent(in) :: diff_coefs
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
      if (iflag_debug.eq.1)  write(*,*) 'int_vol_magne_diffuse_co'
      if (coef_imp_b.gt.zero) then
        call int_vol_vector_diffuse_ele(conduct%istack_ele_fld_smp,     &
     &      node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,             &
     &      diff_coefs, iak_diff_b, coef_imp_b, ak_d_magne, i_magne,    &
     &      fem_wk, f_l)
      end if
!
      if (coef_imp_b.gt.0.0d0) then
        if (iflag_debug.eq.1)  write(*,*) 'int_sk_4_fixed_magne'
        call int_sk_4_fixed_vector(iflag_commute_magne,                 &
     &      i_magne, node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,    &
     &      diff_coefs, Bnod_bcs%nod_bc_b, ak_d_magne,                  &
     &      coef_imp_b, iak_diff_b, fem_wk, f_l)
      end if
!
!
      if     (iflag_implicit_correct.eq.3) then
        call cal_magne_co_lumped_crank                                  &
     &     (i_magne, nod_comm, node, ele, nod_fld,                      &
     &      iphys_ele, ele_fld, Bnod_bcs%nod_bc_b, jac_3d, rhs_tbl,     &
     &      m_lump, mhd_fem_wk, fem_wk, f_l, f_nl)
      else if(iflag_implicit_correct.eq.4) then
        call cal_magne_co_consist_crank(i_magne, coef_magne,            &
     &      node, ele, conduct, nod_fld, Bnod_bcs%nod_bc_b, jac_3d,     &
     &      rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl)
      end if
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_sol_magne_pre_crank'
      call solver_crank_vector(node, num_MG_level,                      &
     &    MG_interpolate, MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS,   &
     &    method_4_velo, precond_4_crank, eps_4_magne_crank, itr,       &
     &    i_magne, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      end subroutine cal_magnetic_co_imp
!
! -----------------------------------------------------------------------
!
      end module implicit_vector_correct
