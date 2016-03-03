!
!      module cal_mod_vel_potential
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on July, 2006
!
!!      subroutine cal_mod_potential(iak_diff_v,                        &
!!     &          node, ele, surf, fluid, sf_grp, Vnod_bcs, iphys,      &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_l, rhs_tbl, FEM_elens, &
!!     &          num_MG_level, MG_interpolate, MG_comm_fluid,          &
!!     &          MG_DJDS_lin_fl, Pmat_MG_DJDS, MG_vector,              &
!!     &          fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_electric_potential(iak_diff_b)
!!     &          node, ele, surf, sf_grp, iphys,                       &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_l, rhs_tbl, FEM_elens, &
!!     &          num_MG_level, MG_interpolate, MG_comm_table,          &
!!     &          MG_DJDS_linear, Fmat_MG_DJDS, MG_vector,              &
!!     &          fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_mag_potential(iak_diff_b,                        &
!!     &          node, ele, surf, sf_grp, iphys,                       &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_l, rhs_tbl, FEM_elens, &
!!     &          num_MG_level, MG_interpolate, MG_comm_table,          &
!!     &          MG_DJDS_linear, Fmat_MG_DJDS, MG_vector,              &
!!     &          fem_wk, f_l, f_nl, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
!!        type(communication_table), intent(in)                         &
!!       &           :: MG_comm_fluid(0:num_MG_level)
!!        type(communication_table), intent(in)                         &
!!       &           :: MG_comm_table(0:num_MG_level)
!!        type(DJDS_ordering_table), intent(in)                         &
!!       &           :: MG_DJDS_linear(0:num_MG_level)
!!        type(DJDS_ordering_table), intent(in)                         &
!!       &           :: MG_DJDS_lin_fl(0:num_MG_level)
!!        type(DJDS_MATRIX), intent(in) :: Pmat_MG_DJDS(0:num_MG_level)
!!        type(DJDS_MATRIX), intent(in) :: Fmat_MG_DJDS(0:num_MG_level)
!!        type(vectors_4_solver), intent(inout)                         &
!!       &           :: MG_vector(0:num_MG_level)
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_mod_vel_potential
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
!
      use m_array_for_send_recv
      use m_solver_djds_MHD
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_address
      use t_phys_data
      use t_jacobian_3d
      use t_jacobian_2d
      use t_table_FEM_const
      use t_filter_elength
      use t_finite_element_mat
      use t_solver_djds
      use t_interpolate_table
      use t_vector_for_solver
      use t_bc_data_velo
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_mod_potential(iak_diff_v,                          &
     &          node, ele, surf, fluid, sf_grp, Vnod_bcs, iphys,        &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_l, rhs_tbl, FEM_elens,   &
     &          num_MG_level, MG_interpolate, MG_comm_fluid,            &
     &          MG_DJDS_lin_fl, Pmat_MG_DJDS, MG_vector,                &
     &          fem_wk, f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_surf_data_torque
      use m_surf_data_press
      use m_bc_data_velo
      use m_surf_data_press
!
      use int_vol_fractional_div
      use int_sk_4_fixed_boundary
      use int_surf_div_sgs
      use int_surf_fixed_gradients
      use int_surf_normal_fields
      use set_boundary_scalars
      use set_nodal_bc_id_data
      use cal_solver_MHD
!
      integer(kind = kint), intent(in) :: iak_diff_v
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid
      type(surface_group_data), intent(in) :: sf_grp
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(phys_address), intent(in) :: iphys
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
      type(communication_table), intent(in)                             &
     &           :: MG_comm_fluid(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &           :: MG_DJDS_lin_fl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: Pmat_MG_DJDS(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:num_MG_level)
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff(node%numnod, f_l)
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
!    take divergence of velocity
!
      call int_vol_fractional_div_ele                                   &
     &   (fluid%istack_ele_fld_smp, iphys%i_velo, iak_diff_v,           &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l,                       &
     &    rhs_tbl, FEM_elens, fem_wk, f_l)
!
      call int_surf_normal_vector                                       &
     &   (iphys%i_velo, sf_bc1_wall_p, sf_bc1_spin_p, sf_bc1_spout_p,   &
     &    node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, rhs_tbl,      &
     &    fem_wk, f_l)
!
!      if (iflag_commute_velo .eq. id_SGS_commute_ON) then
!        call int_surf_sgs_div_velo_ele                                 &
!     &     (node, ele, surf, sf_grp, nod_fld,                          &
!     &      jac_sf_grp_q, jac_sf_grp_l, rhs_tbl, FEM_elens,            &
!     &      intg_point_poisson, sf_sgs1_grad_v%nmax_sf_dat,            &
!     &      sf_sgs1_grad_v%ngrp_sf_dat, sf_sgs1_grad_v%id_grp_sf_dat,  &
!     &      ifilter_final, ak_diff(1,iak_diff_v), iphys%i_velo,        &
!     &      fem_wk, f_l)
!      end if
!
!   set boundary condition for wall
!
      call int_sf_grad_press(node, ele, surf, sf_grp,                   &
     &    jac_sf_grp_l, rhs_tbl, sf_bc1_grad_p,                         &
     &    intg_point_poisson, fem_wk, f_l)
!
!   add boundary term for fixed velocity
!
      call int_vol_sk_po_bc(iphys%i_p_phi, iak_diff_v, node, ele,       &
     &     nod_fld, jac_3d_l, rhs_tbl, FEM_elens, Vnod_bcs%nod_bc_p,    &
     &     fem_wk, f_l)
!
!   add boundary term for fixed pressure
!
      call set_boundary_ff(node, Vnod_bcs%nod_bc_p, f_l)
!
!   solve Poission equation
!
      call solver_poisson_scalar(node, num_MG_level,                    &
     &    MG_interpolate, MG_comm_fluid, MG_DJDS_lin_fl, Pmat_MG_DJDS,  &
     &    method_4_solver, precond_4_solver, eps, itr,                  &
     &    iphys%i_p_phi, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      call set_boundary_scalar                                          &
     &   (Vnod_bcs%nod_bc_p, iphys%i_p_phi, nod_fld)
!
      end subroutine cal_mod_potential
!
!-----------------------------------------------------------------------
!
      subroutine cal_electric_potential(iak_diff_b,                     &
     &          node, ele, surf, sf_grp, iphys,                         &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_l, rhs_tbl, FEM_elens,   &
     &          num_MG_level, MG_interpolate, MG_comm_table,            &
     &          MG_DJDS_linear, Fmat_MG_DJDS, MG_vector,                &
     &          fem_wk, f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_surf_data_vector_p
      use m_surf_data_magne_p
      use m_bc_data_magne
!
      use int_vol_fractional_div
      use int_sk_4_fixed_boundary
      use int_surf_div_sgs
      use set_boundary_scalars
      use int_surf_normal_fields
      use cal_solver_MHD
!
      integer(kind = kint), intent(in) :: iak_diff_b
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_address), intent(in) :: iphys
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
      type(communication_table), intent(in)                             &
     &           :: MG_comm_table(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &           :: MG_DJDS_linear(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: Fmat_MG_DJDS(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:num_MG_level)
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff(node%numnod, f_l)
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      if (iflag_debug .gt. 0)  write(*,*) 'int_vol_divergence_vect_p'
      call int_vol_fractional_div_ele                                   &
     &   (ele%istack_ele_smp, iphys%i_vecp, iak_diff_b,                 &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l,                       &
     &    rhs_tbl, FEM_elens, fem_wk, f_l)
!
!      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
!        call int_surf_sgs_div_velo_ele                                 &
!     &     (node, ele, surf, sf_grp, nod_fld,                          &
!     &      jac_sf_grp_q, jac_sf_grp_l, rhs_tbl, FEM_elens,            &
!     &      intg_point_poisson, sf_sgs1_grad_a%nmax_sf_dat,            &
!     &      sf_sgs1_grad_a%ngrp_sf_dat, sf_sgs1_grad_a%id_grp_sf_dat,  &
!     &      ifilter_final, ak_diff(1,iak_diff_b),                      &
!     &      iphys%i_vecp, fem_wk, f_l)
!      end if
!
      call int_surf_normal_vector                                       &
     &   (iphys%i_vecp, sf_bc1_wall_f, sf_bc1_spin_f, sf_bc1_spout_f,   &
     &    node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, rhs_tbl,      &
     &    fem_wk, f_l)
!
      call int_vol_sk_mp_bc(iphys%i_m_phi, iak_diff_b, node, ele,       &
     &     nod_fld, jac_3d_l, rhs_tbl, FEM_elens, fem_wk, f_l)
!
      call set_boundary_ff(node, nod_bc1_f, f_l)
!
      if (iflag_debug .gt. 0)  write(*,*) 'cal_sol_mag_po'
      call solver_poisson_scalar(node,  num_MG_level,                   &
     &    MG_interpolate, MG_comm_table, MG_DJDS_linear, Fmat_MG_DJDS,  &
     &    method_4_solver, precond_4_solver, eps, itr,                  &
     &    iphys%i_m_phi, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      if (iflag_debug .gt. 0)  write(*,*) 'set_boundary_m_phi'
      call set_boundary_scalar(nod_bc1_f, iphys%i_m_phi, nod_fld)
!
      end subroutine cal_electric_potential
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_mag_potential(iak_diff_b,                          &
     &          node, ele, surf, sf_grp, iphys,                         &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_l, rhs_tbl, FEM_elens,   &
     &          num_MG_level, MG_interpolate, MG_comm_table,            &
     &          MG_DJDS_linear, Fmat_MG_DJDS, MG_vector,                &
     &          fem_wk, f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
      use m_surf_data_magne
      use m_surf_data_magne_p
      use m_bc_data_magne
!
      use int_vol_fractional_div
      use int_sk_4_fixed_boundary
      use int_surf_div_sgs
      use int_surf_fixed_gradients
      use set_boundary_scalars
      use int_surf_normal_fields
      use cal_solver_MHD
!
      integer(kind = kint), intent(in) :: iak_diff_b
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_address), intent(in) :: iphys
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
      type(communication_table), intent(in)                             &
     &           :: MG_comm_table(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &           :: MG_DJDS_linear(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: Fmat_MG_DJDS(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:num_MG_level)
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff(node%numnod, f_l)
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      call int_vol_fractional_div_ele                                   &
     &   (ele%istack_ele_smp, iphys%i_magne, iak_diff_b,                &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l,                       &
     &    rhs_tbl, FEM_elens, fem_wk, f_l)
!
!      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
!        call int_surf_sgs_div_velo_ele(node, ele, surf, sf_grp,        &
!     &      nod_fld, jac_sf_grp_q, jac_sf_grp_l,                       &
!     &      rhs_tbl, FEM_elens, intg_point_poisson,                    &
!     &      sf_sgs1_grad_b%nmax_sf_dat, sf_sgs1_grad_b%ngrp_sf_dat,    &
!     &      sf_sgs1_grad_b%id_grp_sf_dat, ifilter_final,               &
!     &      ak_diff(1,iak_diff_b), iphys%i_magne, fem_wk, f_l)
!      end if
!
      call int_surf_normal_vector                                       &
     &   (iphys%i_magne, sf_bc1_wall_f, sf_bc1_spin_f, sf_bc1_spout_f,  &
     &    node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, rhs_tbl,      &
     &    fem_wk, f_l)
      call int_sf_grad_press(node, ele, surf, sf_grp,                   &
     &    jac_sf_grp_l, rhs_tbl, sf_bc1_grad_f,                         &
     &    intg_point_poisson, fem_wk, f_l)
!
      call int_vol_sk_mp_bc(iphys%i_m_phi, iak_diff_b, node, ele,       &
     &    nod_fld, jac_3d_l, rhs_tbl, FEM_elens, fem_wk, f_l)
!
      call set_boundary_ff(node, nod_bc1_f, f_l)
!
      call solver_poisson_scalar(node, num_MG_level,                    &
     &    MG_interpolate, MG_comm_table, MG_DJDS_linear, Fmat_MG_DJDS,  &
     &    method_4_solver, precond_4_solver, eps, itr,                  &
     &    iphys%i_m_phi, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      call set_boundary_scalar(nod_bc1_f, iphys%i_m_phi, nod_fld)
!
      end subroutine cal_mag_potential
!
! ----------------------------------------------------------------------
!
      end module cal_mod_vel_potential
