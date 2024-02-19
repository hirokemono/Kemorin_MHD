!>@file   cal_mod_vel_potential.f90
!!@brief  module cal_mod_vel_potential
!!
!!@author  H.Matsui and H.Okuda
!!@date   Programmed in July, 2000 (ver 1.1)
!!@n      Modified in July, 2006
!
!>@brief obtain potentials by Poisson equation
!!
!!@verbatim
!!      subroutine cal_mod_potential(FEM_prm, SGS_param, cmt_param,     &
!!     &          mesh, fluid, sf_grp, Vnod_bcs, Vsf_bcs, Psf_bcs,      &
!!     &          iphys, jacs, rhs_tbl, FEM_elens, Cdiff_velo, Pmatrix, &
!!     &          MG_vector, fem_wk, surf_wk, f_l, f_nl, nod_fld,       &
!!     &          v_sol, SR_sig, SR_r)
!!      subroutine cal_electric_potential(FEM_prm, SGS_param, cmt_param,&
!!     &          node, ele, surf, sf_grp, Bnod_bcs, Asf_bcs, Fsf_bcs,  &
!!     &          iphys, jacs, rhs_tbl, FEM_elens, Cdiff_magne,         &
!!     &          Fmatrix, MG_vector, fem_wk, surf_wk, f_l, f_nl,       &
!!     &          nod_fld, v_sol, SR_sig, SR_r)
!!      subroutine cal_mag_potential(FEM_prm, SGS_param, cmt_param,     &
!!     &          mesh, sf_grp, Bnod_bcs, Bsf_bcs, Fsf_bcs,             &
!!     &          iphys, jacs, rhs_tbl, FEM_elens, Cdiff_magne,         &
!!     &          Fmatrix, MG_vector, fem_wk, surf_wk, f_l, f_nl,       &
!!     &          nod_fld, v_sol, SR_sig, SR_r)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
!!        type(velocity_surf_bc_type), intent(in) :: Asf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Fsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(jacobians_type), intent(in) :: jacs
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_model_coefficient), intent(in) :: Cdiff_velo
!!        type(SGS_model_coefficient), intent(in) :: Cdiff_magne
!!        type(MHD_MG_matrix), intent(in) :: Pmatrix
!!        type(MHD_MG_matrix), intent(in) :: Fmatrix
!!        type(vectors_4_solver), intent(inout)                         &
!!       &           :: MG_vector(0:num_MG_level)
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module cal_mod_vel_potential
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_mesh_data
      use t_geometry_data_MHD
      use t_base_field_labels
      use t_phys_address
      use t_phys_data
      use t_jacobians
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
      use t_surface_bc_scalar
      use t_surface_bc_vector
      use t_surface_bc_velocity
      use t_solver_SR
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_mod_potential(FEM_prm, SGS_param, cmt_param,       &
     &          mesh, fluid, sf_grp, Vnod_bcs, Vsf_bcs, Psf_bcs,        &
     &          iphys, jacs, rhs_tbl, FEM_elens, Cdiff_velo, Pmatrix,   &
     &          MG_vector, fem_wk, surf_wk, f_l, f_nl, nod_fld,         &
     &          v_sol, SR_sig, SR_r)
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
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid
      type(surface_group_data), intent(in) :: sf_grp
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(potential_surf_bc_type), intent(in) :: Psf_bcs
      type(phys_address), intent(in) :: iphys
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_model_coefficient), intent(in) :: Cdiff_velo
!
      type(MHD_MG_matrix), intent(in) :: Pmatrix
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Pmatrix%nlevel_MG)
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call reset_ff(n_vector, mesh%node, f_l)
      call reset_ff_smps(mesh%node, f_l, f_nl)
!
!    take divergence of velocity
!
      call int_vol_fractional_div_ele(SGS_param%ifilter_final,          &
     &    fluid%istack_ele_fld_smp, FEM_prm%npoint_poisson_int,         &
     &    iphys%base%i_velo, mesh%node, mesh%ele, nod_fld,              &
     &    jacs%g_FEM, jacs%jac_3d, jacs%jac_3d_l, rhs_tbl, FEM_elens,   &
     &    Cdiff_velo, fem_wk, f_l)
!
      call int_surf_normal_vector                                       &
     &   (iphys%base%i_velo, FEM_prm%npoint_poisson_int,                &
     &    Psf_bcs%wall, Psf_bcs%sph_in, Psf_bcs%sph_out,                &
     &    mesh%node, mesh%ele, mesh%surf, sf_grp, nod_fld, jacs%g_FEM,  &
     &    jacs%jac_sf_grp_l, rhs_tbl, fem_wk, surf_wk, f_l)
!
!      if(SGS_param%SGS_momentum%iflag_commute_field                    &
!     &                        .eq. id_SGS_commute_ON) then
!        call int_surf_sgs_div_velo_ele(mesh%node, mesh%ele, mesh%surf, &
!     &      sf_grp, nod_fld, jacs%g_FEM,                               &
!     &      jacs%jac_sf_grp_q, jacs%jac_sf_grp_l, rhs_tbl, FEM_elens,  &
!     &      FEM_prm%npoint_poisson_int, Vsf_bcs%sgs%nmax_sf_dat,       &
!     &      Vsf_bcs%sgs%ngrp_sf_dat, Vsf_bcs%sgs%id_grp_sf_dat,        &
!     &      SGS_param%ifilter_final, Cdiff_velo%coef(1,1),             &
!     &      iphys%base%i_velo, fem_wk, surf_wk, f_l)
!      end if
!
!   set boundary condition for wall
!
      call int_sf_grad_press(mesh%node, mesh%ele, mesh%surf, sf_grp,    &
     &    jacs%g_FEM, jacs%jac_sf_grp_l, rhs_tbl, Psf_bcs%grad,         &
     &    FEM_prm%npoint_poisson_int, fem_wk, f_l)
!
!   add boundary term for fixed velocity
!
      call int_vol_sk_po_bc(SGS_param%SGS_momentum%iflag_commute_field, &
     &    SGS_param%ifilter_final, FEM_prm%npoint_poisson_int,          &
     &    iphys%exp_work%i_p_phi, mesh%node, mesh%ele, nod_fld,         &
     &    jacs%g_FEM, jacs%jac_3d_l, rhs_tbl, FEM_elens,                &
     &    Vnod_bcs%nod_bc_p, Cdiff_velo%coef(1,1), fem_wk, f_l)
!
!   add boundary term for fixed pressure
!
      call set_boundary_ff(mesh%node, Vnod_bcs%nod_bc_p, f_l)
!
!   solve Poission equation
!
      call solver_poisson_scalar                                        &
     &   (mesh%node, FEM_prm%MG_param, Pmatrix%nlevel_MG,               &
     &    Pmatrix%MG_interpolate,  Pmatrix%MG_comm_table,               &
     &    Pmatrix%MG_DJDS_table, Pmatrix%mat_MG_DJDS,                   &
     &    FEM_PRM%CG11_param%METHOD, FEM_PRM%CG11_param%PRECOND,        &
     &    FEM_prm%CG11_param%EPS, FEM_prm%CG11_param%MAXIT,             &
     &    iphys%exp_work%i_p_phi, MG_vector, f_l, v_sol%b_vec,          &
     &    nod_fld, v_sol%x_vec, SR_sig, SR_r)
!
      call set_boundary_scalar                                          &
     &   (Vnod_bcs%nod_bc_p, iphys%exp_work%i_p_phi, nod_fld)
!
      end subroutine cal_mod_potential
!
!-----------------------------------------------------------------------
!
      subroutine cal_electric_potential(FEM_prm, SGS_param, cmt_param,  &
     &          mesh, group, Bnod_bcs, Asf_bcs, Fsf_bcs,                &
     &          iphys, jacs, rhs_tbl, FEM_elens, Cdiff_magne,           &
     &          Fmatrix, MG_vector, fem_wk, surf_wk, f_l, f_nl,         &
     &          nod_fld, v_sol, SR_sig, SR_r)
!
      use int_vol_fractional_div
      use int_sk_4_fixed_boundary
      use int_surf_div_sgs
      use set_boundary_scalars
      use int_surf_normal_fields
      use cal_solver_MHD
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_model_coefficient), intent(in) :: Cdiff_magne
      type(MHD_MG_matrix), intent(in) :: Fmatrix
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Fmatrix%nlevel_MG)
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call reset_ff(n_vector, mesh%node, f_l)
      call reset_ff_smps(mesh%node, f_l, f_nl)
!
      if (iflag_debug .gt. 0)  write(*,*) 'int_vol_divergence_vect_p'
      call int_vol_fractional_div_ele(SGS_param%ifilter_final,          &
     &    mesh%ele%istack_ele_smp, FEM_prm%npoint_poisson_int,          &
     &    iphys%base%i_vecp, mesh%node, mesh%ele,                       &
     &    nod_fld, jacs%g_FEM, jacs%jac_3d, jacs%jac_3d_l, rhs_tbl,     &
     &    FEM_elens, Cdiff_magne, fem_wk, f_l)
!
!      if (cmt_param%iflag_c_magne .eq. id_SGS_commute_ON) then
!        call int_surf_sgs_div_velo_ele                                 &
!     &     (mesh%node, mesh%ele, mesh%surf, group%surf_grp, nod_fld,   &
!     &      jacs%g_FEM, jacs%jac_sf_grp_q, jacs%jac_sf_grp_l, rhs_tbl, &
!     &      FEM_elens, FEM_prm%npoint_poisson_int,                     &
!     &      Asf_bcs%sgs%nmax_sf_dat, Asf_bcs%sgs%ngrp_sf_dat,          &
!     &      Asf_bcs%sgs%id_grp_sf_dat, SGS_param%ifilter_final,        &
!     &      Cdiff_magne%coef(1,1), iphys%base%i_vecp,                  &
!     &      fem_wk, surf_wk, f_l)
!      end if
!
      call int_surf_normal_vector                                       &
     &   (iphys%base%i_vecp, FEM_prm%npoint_poisson_int,                &
     &    Fsf_bcs%wall, Fsf_bcs%sph_in, Fsf_bcs%sph_out,                &
     &    mesh%node, mesh%ele, mesh%surf, group%surf_grp, nod_fld,      &
     &    jacs%g_FEM, jacs%jac_sf_grp_l, rhs_tbl, fem_wk, surf_wk, f_l)
!
      call int_vol_sk_mp_bc                                             &
     &   (cmt_param%iflag_c_magne, SGS_param%ifilter_final,             &
     &    FEM_prm%npoint_poisson_int, iphys%exp_work%i_m_phi,           &
     &    mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d_l,      &
     &    rhs_tbl, FEM_elens, Bnod_bcs%nod_bc_f, Cdiff_magne%coef(1,1), &
     &    fem_wk, f_l)
!
      call set_boundary_ff(mesh%node, Bnod_bcs%nod_bc_f, f_l)
!
      if (iflag_debug .gt. 0)  write(*,*) 'cal_sol_mag_po'
      call solver_poisson_scalar                                        &
     &   (mesh%node, FEM_prm%MG_param, Fmatrix%nlevel_MG,               &
     &    Fmatrix%MG_interpolate, Fmatrix%MG_comm_table,                &
     &    Fmatrix%MG_DJDS_table, Fmatrix%mat_MG_DJDS,                   &
     &    FEM_PRM%CG11_param%METHOD, FEM_PRM%CG11_param%PRECOND,        &
     &    FEM_prm%CG11_param%EPS, FEM_prm%CG11_param%MAXIT,             &
     &    iphys%exp_work%i_m_phi, MG_vector, f_l, v_sol%b_vec,          &
     &    nod_fld, v_sol%x_vec, SR_sig, SR_r)
!
      if (iflag_debug .gt. 0)  write(*,*) 'set_boundary_m_phi'
      call set_boundary_scalar                                          &
     &   (Bnod_bcs%nod_bc_f, iphys%exp_work%i_m_phi, nod_fld)
!
      end subroutine cal_electric_potential
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_mag_potential(FEM_prm, SGS_param, cmt_param,       &
     &          mesh, sf_grp, Bnod_bcs, Bsf_bcs, Fsf_bcs,               &
     &          iphys, jacs, rhs_tbl, FEM_elens, Cdiff_magne,           &
     &          Fmatrix, MG_vector, fem_wk, surf_wk, f_l, f_nl,         &
     &          nod_fld, v_sol, SR_sig, SR_r)
!
      use int_vol_fractional_div
      use int_sk_4_fixed_boundary
      use int_surf_div_sgs
      use int_surf_fixed_gradients
      use set_boundary_scalars
      use int_surf_normal_fields
      use cal_solver_MHD
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(mesh_geometry), intent(in) :: mesh
      type(surface_group_data), intent(in) :: sf_grp
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_model_coefficient), intent(in) :: Cdiff_magne
      type(MHD_MG_matrix), intent(in) :: Fmatrix
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Fmatrix%nlevel_MG)
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call reset_ff(n_vector, mesh%node, f_l)
      call reset_ff_smps(mesh%node, f_l, f_nl)
!
      call int_vol_fractional_div_ele(SGS_param%ifilter_final,          &
     &    mesh%ele%istack_ele_smp, FEM_prm%npoint_poisson_int,          &
     &    iphys%base%i_magne, mesh%node, mesh%ele, nod_fld,             &
     &    jacs%g_FEM, jacs%jac_3d, jacs%jac_3d_l, rhs_tbl, FEM_elens,   &
     &    Cdiff_magne, fem_wk, f_l)
!
!      if (cmt_param%iflag_c_magne .eq. id_SGS_commute_ON) then
!        call int_surf_sgs_div_velo_ele                                 &
!     &     (mesh%node, mesh%ele, mesh%surf, sf_grp,                    &
!     &      nod_fld, jacs%g_FEM, jacs%jac_sf_grp_q, jacs%jac_sf_grp_l, &
!     &      rhs_tbl, FEM_elens, FEM_prm%npoint_poisson_int,            &
!     &      Bsf_bcs%sgs%nmax_sf_dat, Bsf_bcs%sgs%ngrp_sf_dat,          &
!     &      Bsf_bcs%sgs%id_grp_sf_dat, SGS_param%ifilter_final,        &
!     &      Cdiff_magne%coef(1,1), iphys%base%i_magne,                 &
!     &      fem_wk, surf_wk, f_l)
!      end if
!
      call int_surf_normal_vector                                       &
     &   (iphys%base%i_magne, FEM_prm%npoint_poisson_int,               &
     &    Fsf_bcs%wall, Fsf_bcs%sph_in, Fsf_bcs%sph_out,                &
     &    mesh%node, mesh%ele, mesh%surf, sf_grp, nod_fld, jacs%g_FEM,  &
     &    jacs%jac_sf_grp_l, rhs_tbl, fem_wk, surf_wk, f_l)
      call int_sf_grad_press(mesh%node, mesh%ele, mesh%surf, sf_grp,    &
     &    jacs%g_FEM, jacs%jac_sf_grp_l, rhs_tbl,                       &
     &    Fsf_bcs%grad, FEM_prm%npoint_poisson_int, fem_wk, f_l)
!
      call int_vol_sk_mp_bc                                             &
     &   (cmt_param%iflag_c_magne, SGS_param%ifilter_final,             &
     &    FEM_prm%npoint_poisson_int, iphys%exp_work%i_m_phi,           &
     &    mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d_l,      &
     &    rhs_tbl, FEM_elens, Bnod_bcs%nod_bc_f, Cdiff_magne%coef(1,1), &
     &    fem_wk, f_l)
!
      call set_boundary_ff(mesh%node, Bnod_bcs%nod_bc_f, f_l)
!
      call solver_poisson_scalar                                        &
     &   (mesh%node, FEM_prm%MG_param, Fmatrix%nlevel_MG,               &
     &    Fmatrix%MG_interpolate, Fmatrix%MG_comm_table,                &
     &    Fmatrix%MG_DJDS_table, Fmatrix%mat_MG_DJDS,                   &
     &    FEM_PRM%CG11_param%METHOD, FEM_PRM%CG11_param%PRECOND,        &
     &    FEM_prm%CG11_param%EPS, FEM_prm%CG11_param%MAXIT,             &
     &    iphys%exp_work%i_m_phi, MG_vector, f_l, v_sol%b_vec,          &
     &    nod_fld, v_sol%x_vec, SR_sig, SR_r)
!
      call set_boundary_scalar                                          &
     &   (Bnod_bcs%nod_bc_f, iphys%exp_work%i_m_phi, nod_fld)
!
      end subroutine cal_mag_potential
!
! ----------------------------------------------------------------------
!
      end module cal_mod_vel_potential
