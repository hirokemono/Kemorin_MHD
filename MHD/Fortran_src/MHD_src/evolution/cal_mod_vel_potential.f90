!
!      module cal_mod_vel_potential
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on July, 2006
!
!!      subroutine cal_mod_potential                                    &
!!     &        (iak_diff_v, node, ele, surf, fluid, sf_grp,            &
!!     &         Vnod_bcs, Vsf_bcs, Psf_bcs, iphys,                     &
!!     &         jac_3d_q, jac_3d_l, jac_sf_grp_l, rhs_tbl, FEM_elens,  &
!!     &         diff_coefs, Pmatrix, MG_vector, fem_wk, surf_wk,       &
!!     &         f_l, f_nl, nod_fld)
!!      subroutine cal_electric_potential(iak_diff_b,                   &
!!     &         node, ele, surf, sf_grp, Bnod_bcs, Asf_bcs, Fsf_bcs,   &
!!     &         iphys, jac_3d_q, jac_3d_l, jac_sf_grp_l, rhs_tbl,      &
!!     &         FEM_elens, diff_coefs, Fmatrix, MG_vector,             &
!!     &         fem_wk, surf_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_mag_potential(iak_diff_b,                        &
!!     &         node, ele, surf, sf_grp, Bnod_bcs, Bsf_bcs, Fsf_bcs,   &
!!     &         iphys, jac_3d_q, jac_3d_l, jac_sf_grp_l, rhs_tbl,      &
!!     &         FEM_elens, diff_coefs, Fmatrix, MG_vector,             &
!!     &         fem_wk, surf_wk, f_l, f_nl, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
!!        type(velocity_surf_bc_type), intent(in) :: Asf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Fsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(MHD_MG_matrix), intent(in) :: Pmatrix
!!        type(MHD_MG_matrix), intent(in) :: Fmatrix
!!        type(vectors_4_solver), intent(inout)                         &
!!       &           :: MG_vector(0:num_MG_level)
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
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
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
      use t_material_property
      use t_solver_djds
      use t_solver_djds_MHD
      use t_interpolate_table
      use t_vector_for_solver
      use t_bc_data_velo
      use t_bc_data_magne
      use t_surface_bc_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_mod_potential                                      &
     &        (iak_diff_v, node, ele, surf, fluid, sf_grp,              &
     &         Vnod_bcs, Vsf_bcs, Psf_bcs, iphys,                       &
     &         jac_3d_q, jac_3d_l, jac_sf_grp_l, rhs_tbl, FEM_elens,    &
     &         diff_coefs, Pmatrix, MG_vector, fem_wk, surf_wk,         &
     &         f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
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
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(potential_surf_bc_type), intent(in) :: Psf_bcs
      type(phys_address), intent(in) :: iphys
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(MHD_MG_matrix), intent(in) :: Pmatrix
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Pmatrix%nlevel_MG)
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
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
     &    rhs_tbl, FEM_elens, diff_coefs, fem_wk, f_l)
!
      call int_surf_normal_vector(iphys%i_velo, Psf_bcs%wall,           &
     &    Psf_bcs%sph_in, Psf_bcs%sph_out,                              &
     &    node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, rhs_tbl,      &
     &    fem_wk, surf_wk, f_l)
!
!      if (iflag_commute_velo .eq. id_SGS_commute_ON) then
!        call int_surf_sgs_div_velo_ele                                 &
!     &     (node, ele, surf, sf_grp, nod_fld,                          &
!     &      jac_sf_grp_q, jac_sf_grp_l, rhs_tbl, FEM_elens,            &
!     &      intg_point_poisson, Vsf_bcs%sgs%nmax_sf_dat,               &
!     &      Vsf_bcs%sgs%ngrp_sf_dat, Vsf_bcs%sgs%id_grp_sf_dat,        &
!     &      ifilter_final, diff_coefs%num_field, iak_diff_v,           &
!     &      diff_coefs%ak,  iphys%i_velo, fem_wk, surf_wk, f_l)
!      end if
!
!   set boundary condition for wall
!
      call int_sf_grad_press(node, ele, surf, sf_grp,                   &
     &    jac_sf_grp_l, rhs_tbl, Psf_bcs%grad,                          &
     &    intg_point_poisson, fem_wk, f_l)
!
!   add boundary term for fixed velocity
!
      call int_vol_sk_po_bc(iphys%i_p_phi, iak_diff_v, node, ele,       &
     &     nod_fld, jac_3d_l, rhs_tbl, FEM_elens, diff_coefs,           &
     &     Vnod_bcs%nod_bc_p, fem_wk, f_l)
!
!   add boundary term for fixed pressure
!
      call set_boundary_ff(node, Vnod_bcs%nod_bc_p, f_l)
!
!   solve Poission equation
!
      call solver_poisson_scalar(node, Pmatrix%nlevel_MG,               &
     &    Pmatrix%MG_interpolate,  Pmatrix%MG_comm_table,               &
     &    Pmatrix%MG_DJDS_table, Pmatrix%mat_MG_DJDS,                   &
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
     &         node, ele, surf, sf_grp, Bnod_bcs, Asf_bcs, Fsf_bcs,     &
     &         iphys, jac_3d_q, jac_3d_l, jac_sf_grp_l, rhs_tbl,        &
     &         FEM_elens, diff_coefs, Fmatrix, MG_vector,               &
     &         fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
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
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(MHD_MG_matrix), intent(in) :: Fmatrix
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Fmatrix%nlevel_MG)
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
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
     &    rhs_tbl, FEM_elens, diff_coefs, fem_wk, f_l)
!
!      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
!        call int_surf_sgs_div_velo_ele                                 &
!     &     (node, ele, surf, sf_grp, nod_fld,                          &
!     &      jac_sf_grp_q, jac_sf_grp_l, rhs_tbl, FEM_elens,            &
!     &      intg_point_poisson, Asf_bcs%sgs%nmax_sf_dat,               &
!     &      Asf_bcs%sgs%ngrp_sf_dat, Asf_bcs%sgs%id_grp_sf_dat,        &
!     &      ifilter_final, diff_coefs%num_field, iak_diff_b,           &
!     &      diff_coefs%ak, iphys%i_vecp, fem_wk, surf_wk, f_l)
!      end if
!
      call int_surf_normal_vector(iphys%i_vecp,                         &
     &    Fsf_bcs%wall, Fsf_bcs%sph_in, Fsf_bcs%sph_out,                &
     &    node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, rhs_tbl,      &
     &    fem_wk, surf_wk, f_l)
!
      call int_vol_sk_mp_bc(iphys%i_m_phi, iak_diff_b, node, ele,       &
     &     nod_fld, jac_3d_l, rhs_tbl, FEM_elens, diff_coefs,           &
     &     Bnod_bcs%nod_bc_f, fem_wk, f_l)
!
      call set_boundary_ff(node, Bnod_bcs%nod_bc_f, f_l)
!
      if (iflag_debug .gt. 0)  write(*,*) 'cal_sol_mag_po'
      call solver_poisson_scalar(node, Fmatrix%nlevel_MG,               &
     &    Fmatrix%MG_interpolate, Fmatrix%MG_comm_table,                &
     &    Fmatrix%MG_DJDS_table, Fmatrix%mat_MG_DJDS,                   &
     &    method_4_solver, precond_4_solver, eps, itr,                  &
     &    iphys%i_m_phi, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      if (iflag_debug .gt. 0)  write(*,*) 'set_boundary_m_phi'
      call set_boundary_scalar                                          &
     &   (Bnod_bcs%nod_bc_f, iphys%i_m_phi, nod_fld)
!
      end subroutine cal_electric_potential
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_mag_potential(iak_diff_b,                          &
     &         node, ele, surf, sf_grp, Bnod_bcs, Bsf_bcs, Fsf_bcs,     &
     &         iphys, jac_3d_q, jac_3d_l, jac_sf_grp_l, rhs_tbl,        &
     &         FEM_elens, diff_coefs, Fmatrix, MG_vector,               &
     &         fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      use m_iccg_parameter
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
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(MHD_MG_matrix), intent(in) :: Fmatrix
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Fmatrix%nlevel_MG)
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
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
     &    rhs_tbl, FEM_elens, diff_coefs, fem_wk, f_l)
!
!      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
!        call int_surf_sgs_div_velo_ele(node, ele, surf, sf_grp,        &
!     &      nod_fld, jac_sf_grp_q, jac_sf_grp_l,                       &
!     &      rhs_tbl, FEM_elens, intg_point_poisson,                    &
!     &      Bsf_bcs%sgs%nmax_sf_dat, Bsf_bcs%sgs%ngrp_sf_dat,          &
!     &      Bsf_bcs%sgs%id_grp_sf_dat, ifilter_final,                  &
!     &      diff_coefs%num_field, iak_diff_b, diff_coefs%ak,           &
!     &      iphys%i_magne, fem_wk, surf_wk, f_l)
!      end if
!
      call int_surf_normal_vector(iphys%i_magne,                        &
     &    Fsf_bcs%wall, Fsf_bcs%sph_in, Fsf_bcs%sph_out,                &
     &    node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, rhs_tbl,      &
     &    fem_wk, surf_wk, f_l)
      call int_sf_grad_press(node, ele, surf, sf_grp, jac_sf_grp_l,     &
     &    rhs_tbl, Fsf_bcs%grad, intg_point_poisson, fem_wk, f_l)
!
      call int_vol_sk_mp_bc(iphys%i_m_phi, iak_diff_b, node, ele,       &
     &    nod_fld, jac_3d_l, rhs_tbl, FEM_elens, diff_coefs,            &
     &    Bnod_bcs%nod_bc_f, fem_wk, f_l)
!
      call set_boundary_ff(node, Bnod_bcs%nod_bc_f, f_l)
!
      call solver_poisson_scalar(node, Fmatrix%nlevel_MG,               &
     &    Fmatrix%MG_interpolate, Fmatrix%MG_comm_table,                &
     &    Fmatrix%MG_DJDS_table, Fmatrix%mat_MG_DJDS,                   &
     &    method_4_solver, precond_4_solver, eps, itr,                  &
     &    iphys%i_m_phi, MG_vector, f_l, b_vec, x_vec, nod_fld)
!
      call set_boundary_scalar                                          &
     &   (Bnod_bcs%nod_bc_f, iphys%i_m_phi, nod_fld)
!
      end subroutine cal_mag_potential
!
! ----------------------------------------------------------------------
!
      end module cal_mod_vel_potential
