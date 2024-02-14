!
!      module cal_magnetic_pre
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!!      subroutine cal_magnetic_field_pre(ak_d_magne, dt,               &
!!     &          FEM_prm, SGS_param, cmt_param, filter_param,          &
!!     &          mesh, conduct, group, cd_prop, Bnod_bcs,              &
!!     &          Asf_bcs, Bsf_bcs, iphys, iphys_LES, iphys_ele_base,   &
!!     &          ele_fld, jacs, rhs_tbl, iak_diff_base, iak_diff_sgs,  &
!!     &          icomp_sgs_term, iphys_elediff_vec,                    &
!!     &          sgs_coefs, sgs_coefs_nod, diff_coefs, FEM_filters,    &
!!     &          mlump_cd, Bmatrix, MG_vector, wk_filter,              &
!!     &          mhd_fem_wk, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
!!      subroutine cal_magnetic_co                                      &
!!     &         (ak_d_magne, dt, FEM_prm, SGS_param, cmt_param,        &
!!     &          mesh, conduct, group, cd_prop, Bnod_bcs, Fsf_bcs,     &
!!     &          iphys, iphys_ele_base, ele_fld, jacs, rhs_tbl,        &
!!     &          FEM_elens, iflag_diff_b, ak_diff, m_lump,             &
!!     &          Bmatrix, MG_vector, mhd_fem_wk, rhs_mat, nod_fld,     &
!!     &          v_sol, SR_sig, SR_r)
!!      subroutine cal_magnetic_co_outside                              &
!!     &         (FEM_prm, SGS_param, cmt_param, mesh, insulate, group, &
!!     &          Bnod_bcs, Fsf_bcs, iphys, jacs, rhs_tbl, FEM_elens,   &
!!     &          iflag_diff_b, ak_diff, mlump_ins, mhd_fem_wk,         &
!!     &          rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
!!        type(base_field_address), intent(in) :: iak_diff_base
!!        type(SGS_term_address), intent(in) :: iak_diff_sgs
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(SGS_filtering_params), intent(in) :: filter_param
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(field_geometry_data), intent(in) :: insulate
!!        type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
!!        type(velocity_surf_bc_type), intent(in) :: Asf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Fsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_term_address), intent(in) :: icomp_sgs_term
!!        type(base_field_address), intent(in) :: iphys_elediff_vec
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(lumped_mass_matrices), intent(in) :: mlump_cd
!!        type(lumped_mass_matrices), intent(in) :: mlump_ins
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(MHD_MG_matrix), intent(in) :: Bmatrix
!!        type(vectors_4_solver), intent(inout)                         &
!!       &           :: MG_vector(0:num_MG_level)
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module cal_magnetic_pre
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_mesh_data
      use t_geometry_data_MHD
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_base_field_labels
      use t_SGS_term_labels
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
      use t_solver_djds
      use t_solver_djds_MHD
      use t_interpolate_table
      use t_vector_for_solver
      use t_bc_data_magne
      use t_surface_bc_scalar
      use t_surface_bc_vector
      use t_surface_bc_velocity
      use t_material_property
      use t_work_FEM_integration
      use t_FEM_SGS_model_coefs
      use t_FEM_MHD_filter_data
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
      subroutine cal_magnetic_field_pre(ak_d_magne, dt,                 &
     &          FEM_prm, SGS_param, cmt_param, filter_param,            &
     &          mesh, conduct, group, cd_prop, Bnod_bcs,                &
     &          Asf_bcs, Bsf_bcs, iphys, iphys_LES, iphys_ele_base,     &
     &          ele_fld, jacs, rhs_tbl, iak_diff_base, iak_diff_sgs,    &
     &          icomp_sgs_term, iphys_elediff_vec,                      &
     &          sgs_coefs, sgs_coefs_nod, diff_coefs, FEM_filters,      &
     &          mlump_cd, Bmatrix, MG_vector, wk_filter,                &
     &          mhd_fem_wk, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
!
      use calypso_mpi
!
      use set_boundary_scalars
      use nod_phys_send_recv
      use cal_sgs_fluxes
      use int_vol_diffusion_ele
      use int_vol_magne_pre
      use int_surf_magne_pre
      use evolve_by_1st_euler
      use evolve_by_adams_bashforth
      use evolve_by_lumped_crank
      use evolve_by_consist_crank
!
      type(base_field_address), intent(in) :: iak_diff_base
      type(SGS_term_address), intent(in) :: iak_diff_sgs
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(SGS_term_address), intent(in) :: icomp_sgs_term
      type(base_field_address), intent(in) :: iphys_elediff_vec
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_matrices), intent(in) :: mlump_cd
      type(MHD_MG_matrix), intent(in) :: Bmatrix
!
      real(kind = kreal), intent(in) :: ak_d_magne(mesh%ele%numele)
      real(kind = kreal), intent(in) :: dt
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Bmatrix%nlevel_MG)
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if ( SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
        call cal_sgs_magne_induction                                    &
     &    (dt, FEM_prm, SGS_param, filter_param,                        &
     &     mesh%nod_comm, mesh%node, mesh%ele, conduct, cd_prop,        &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%SGS_term,        &
     &     iphys_ele_base, ele_fld, jacs, rhs_tbl,                      &
     &     FEM_filters%FEM_elens, FEM_filters%filtering,                &
     &     icomp_sgs_term, iphys_elediff_vec, sgs_coefs, sgs_coefs_nod, &
     &     mlump_cd, wk_filter, mhd_fem_wk, rhs_mat%fem_wk,             &
     &     rhs_mat%f_l, nod_fld, v_sol, SR_sig, SR_r)
      end if
!
      call reset_ff_smps(mesh%node, rhs_mat%f_l, rhs_mat%f_nl)
!
! lead diffusion term
!
      if (cd_prop%coef_magne .gt. zero                                  &
     &      .and. cd_prop%coef_exp .gt. zero) then
        call int_vol_vector_diffuse_ele(SGS_param%ifilter_final,        &
     &      conduct%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &      mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,      &
     &      rhs_tbl, FEM_filters%FEM_elens,                             &
     &      diff_coefs%Cdiff_magne%num_comp, diff_coefs%Cdiff_magne%coef, &
     &      cd_prop%coef_exp, ak_d_magne, iphys%base%i_magne,           &
     &      rhs_mat%fem_wk, rhs_mat%f_l)
      end if
!
! lead induction terms
!
      if (iflag_debug .eq. 0 ) write(*,*) 'coefs_4_time_evolution'
      if (FEM_prm%iflag_magne_supg .gt. id_turn_OFF) then
       call int_vol_magne_pre_ele_upm(FEM_prm%npoint_t_evo_int, dt,     &
     &     SGS_param, cmt_param, mesh%node, mesh%ele, conduct, cd_prop, &
     &     iphys%base, iphys_LES%SGS_term, nod_fld,                     &
     &     ele_fld%ntot_phys, ele_fld%d_fld, iphys_ele_base,            &
     &     jacs%g_FEM, jacs%jac_3d, rhs_tbl, FEM_filters%FEM_elens,     &
     &     diff_coefs%ak(1,iak_diff_sgs%i_SGS_induction), mhd_fem_wk,   &
     &     rhs_mat%fem_wk, rhs_mat%f_nl)
      else
       call int_vol_magne_pre_ele(FEM_prm%npoint_t_evo_int,             &
     &     SGS_param, cmt_param, mesh%node, mesh%ele, conduct, cd_prop, &
     &     iphys%base, iphys_LES%SGS_term, nod_fld,                     &
     &     ele_fld%ntot_phys, ele_fld%d_fld, iphys_ele_base,            &
     &     jacs%g_FEM, jacs%jac_3d, rhs_tbl, FEM_filters%FEM_elens,     &
     &     diff_coefs%ak(1,iak_diff_sgs%i_SGS_induction), mhd_fem_wk,   &
     &     rhs_mat%fem_wk, rhs_mat%f_nl)
      end if
!
!
      call int_surf_magne_pre_ele                                       &
     &   (SGS_param, cmt_param, FEM_prm%npoint_t_evo_int, ak_d_magne,   &
     &    mesh%node, mesh%ele, mesh%surf, group%surf_grp,               &
     &    Asf_bcs, Bsf_bcs, iphys%base, iphys_LES%SGS_term, nod_fld,    &
     &    jacs%g_FEM, jacs%jac_sf_grp, rhs_tbl, FEM_filters%FEM_elens,  &
     &    diff_coefs%ak(1,iak_diff_sgs%i_SGS_induction),                &
     &    rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl)
!
      if(cd_prop%iflag_Bevo_scheme .eq. id_explicit_euler) then
        call cal_magne_pre_euler(iphys%base%i_magne, dt, FEM_prm,       &
     &      mesh%nod_comm, mesh%node, mesh%ele, conduct,                &
     &      iphys_ele_base, ele_fld, jacs%g_FEM, jacs%jac_3d, rhs_tbl,  &
     &      mlump_cd, mhd_fem_wk, rhs_mat%fem_wk,                       &
     &      rhs_mat%f_l, rhs_mat%f_nl, nod_fld, v_sol, SR_sig, SR_r)
      else if(cd_prop%iflag_Bevo_scheme .eq. id_explicit_adams2) then
        call cal_magne_pre_adams                                        &
     &     (iphys%base%i_magne, iphys%exp_work%i_pre_uxb, dt, FEM_prm,  &
     &      mesh%nod_comm, mesh%node, mesh%ele, conduct,                &
     &      iphys_ele_base, ele_fld, jacs%g_FEM, jacs%jac_3d, rhs_tbl,  &
     &      mlump_cd, mhd_fem_wk, rhs_mat%fem_wk,                       &
     &      rhs_mat%f_l, rhs_mat%f_nl, nod_fld, v_sol, SR_sig, SR_r)
      else if(cd_prop%iflag_Bevo_scheme .eq. id_Crank_nicolson) then
        call cal_magne_pre_lumped_crank                                 &
     &     (cmt_param%iflag_c_magne, SGS_param%ifilter_final,           &
     &      iphys%base%i_magne, iphys%exp_work%i_pre_uxb,               &
     &      ak_d_magne, Bnod_bcs%nod_bc_b, dt, FEM_prm,                 &
     &      mesh%nod_comm, mesh%node, mesh%ele, conduct, cd_prop,       &
     &      iphys_ele_base, ele_fld, jacs%g_FEM, jacs%jac_3d, rhs_tbl,  &
     &      FEM_filters%FEM_elens, diff_coefs%Cdiff_magne%coef,         &
     &      mlump_cd, Bmatrix, MG_vector, mhd_fem_wk, rhs_mat%fem_wk,   &
     &      rhs_mat%f_l, rhs_mat%f_nl, nod_fld, v_sol, SR_sig, SR_r)
      else if(cd_prop%iflag_Bevo_scheme .eq. id_Crank_nicolson_cmass)   &
     & then
        call cal_magne_pre_consist_crank                                &
     &    (cmt_param%iflag_c_magne, SGS_param%ifilter_final,            &
     &     iphys%base%i_magne, iphys%exp_work%i_pre_uxb,                &
     &     ak_d_magne, Bnod_bcs%nod_bc_b, dt, FEM_prm,                  &
     &     mesh%node, mesh%ele, conduct, cd_prop,                       &
     &     jacs%g_FEM, jacs%jac_3d, rhs_tbl, FEM_filters%FEM_elens,     &
     &     diff_coefs%Cdiff_magne%coef, Bmatrix, MG_vector,             &
     &     mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl,       &
     &     nod_fld, v_sol, SR_sig, SR_r)
      end if
!
      call set_boundary_vect                                            &
     &   (Bnod_bcs%nod_bc_b, iphys%base%i_magne, nod_fld)
!
      call vector_send_recv(iphys%base%i_magne, mesh%nod_comm,          &
     &                      nod_fld, v_sol, SR_sig, SR_r)
!
      end subroutine cal_magnetic_field_pre
!
! ----------------------------------------------------------------------
!
      subroutine cal_magnetic_co                                        &
     &         (ak_d_magne, dt, FEM_prm, SGS_param, cmt_param,          &
     &          mesh, conduct, group, cd_prop, Bnod_bcs, Fsf_bcs,       &
     &          iphys, iphys_ele_base, ele_fld, jacs, rhs_tbl,          &
     &          FEM_elens, iflag_diff_b, ak_diff, m_lump,               &
     &          Bmatrix, MG_vector, mhd_fem_wk, rhs_mat, nod_fld,       &
     &          v_sol, SR_sig, SR_r)
!
      use set_boundary_scalars
      use nod_phys_send_recv
      use int_vol_solenoid_correct
      use int_surf_grad_sgs
      use implicit_vector_correct
      use cal_multi_pass
      use cal_sol_vector_co_crank
!
      integer(kind = kint), intent(in) :: iflag_diff_b
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_MG_matrix), intent(in) :: Bmatrix
!
      real(kind = kreal), intent(in) :: ak_d_magne(mesh%ele%numele)
      real(kind = kreal), intent(in) :: ak_diff(mesh%ele%numele)
      real(kind = kreal), intent(in) :: dt
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Bmatrix%nlevel_MG)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call reset_ff_smps(mesh%node, rhs_mat%f_l, rhs_mat%f_nl)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_vol_magne_co'
      call int_vol_solenoid_co                                          &
     &   (FEM_prm%npoint_poisson_int, SGS_param%ifilter_final,          &
     &    mesh%ele%istack_ele_smp, iphys%exp_work%i_m_phi,              &
     &    iflag_diff_b, mesh%node, mesh%ele, nod_fld,                   &
     &    jacs%g_FEM, jacs%jac_3d, jacs%jac_3d_l,                       &
     &    rhs_tbl, FEM_elens, ak_diff, rhs_mat%fem_wk, rhs_mat%f_nl)
!
      if (cmt_param%iflag_c_magne .eq. id_SGS_commute_ON                &
     &     .and. Fsf_bcs%sgs%ngrp_sf_dat .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'int_surf_sgs_velo_co_ele',    &
                             iphys%exp_work%i_m_phi
         call int_surf_sgs_velo_co_ele                                  &
     &     (mesh%node, mesh%ele, mesh%surf, group%surf_grp, nod_fld,    &
     &      jacs%g_FEM, jacs%jac_sf_grp, jacs%jac_sf_grp_l,             &
     &      rhs_tbl, FEM_elens, FEM_prm%npoint_poisson_int,             &
     &      Fsf_bcs%sgs%ngrp_sf_dat, Fsf_bcs%sgs%id_grp_sf_dat,         &
     &      SGS_param%ifilter_final, ak_diff, iphys%exp_work%i_m_phi,   &
     &      rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_nl)
      end if
!
!
!
      if (   FEM_prm%iflag_imp_correct .eq. id_Crank_nicolson           &
     &  .or. FEM_prm%iflag_imp_correct .eq. id_Crank_nicolson_cmass)    &
     & then
        call cal_magnetic_co_imp(iphys%base%i_magne, iflag_diff_b,      &
     &      ak_d_magne, dt, FEM_prm, SGS_param, cmt_param,              &
     &      mesh%nod_comm, mesh%node, mesh%ele, conduct, cd_prop,       &
     &      Bnod_bcs, iphys_ele_base, ele_fld, jacs%g_FEM, jacs%jac_3d, &
     &      rhs_tbl, FEM_elens, ak_diff, m_lump, Bmatrix, MG_vector,    &
     &      mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl,      &
     &      nod_fld, v_sol, SR_sig, SR_r)
      else
        call cal_magnetic_co_exp(iphys%base%i_magne, FEM_prm,           &
     &      mesh%nod_comm, mesh%node, mesh%ele,                         &
     &      jacs%g_FEM, jacs%jac_3d, rhs_tbl, m_lump, mhd_fem_wk,       &
     &      rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld,         &
     &      v_sol, SR_sig, SR_r)
      end if
!
!
      if (iflag_debug.eq.1)   write(*,*) 'set_boundary_vect magne'
      call set_boundary_vect                                            &
     &   (Bnod_bcs%nod_bc_b, iphys%base%i_magne, nod_fld)
!
      call vector_send_recv(iphys%base%i_magne, mesh%nod_comm,          &
     &                      nod_fld, v_sol, SR_sig, SR_r)
      call scalar_send_recv(iphys%base%i_mag_p, mesh%nod_comm,          &
     &                      nod_fld, v_sol, SR_sig, SR_r)
!
      end subroutine cal_magnetic_co
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_magnetic_co_outside                                &
     &         (FEM_prm, SGS_param, cmt_param, mesh, insulate, group,   &
     &          Bnod_bcs, Fsf_bcs, iphys, jacs, rhs_tbl, FEM_elens,     &
     &          iflag_diff_b, ak_diff, mlump_ins, mhd_fem_wk,           &
     &          rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
!
      use set_boundary_scalars
      use nod_phys_send_recv
      use int_vol_solenoid_correct
      use int_surf_grad_sgs
      use cal_sol_vector_correct
      use cal_multi_pass
      use cal_sol_vector_co_crank
!
      integer(kind = kint), intent(in) :: iflag_diff_b
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(field_geometry_data), intent(in) :: insulate
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(lumped_mass_matrices), intent(in) :: mlump_ins
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      real(kind = kreal), intent(in) :: ak_diff(mesh%ele%numele)
!
!
      call reset_ff_smps(mesh%node, rhs_mat%f_l, rhs_mat%f_nl)
!
      call int_vol_solenoid_co                                          &
     &   (FEM_prm%npoint_poisson_int, SGS_param%ifilter_final,          &
     &    insulate%istack_ele_fld_smp, iphys%base%i_mag_p,              &
     &    iflag_diff_b, mesh%node, mesh%ele, nod_fld,                   &
     &    jacs%g_FEM, jacs%jac_3d, jacs%jac_3d_l, rhs_tbl, FEM_elens,   &
     &    ak_diff, rhs_mat%fem_wk, rhs_mat%f_nl)
!
      if (cmt_param%iflag_c_magne .eq. id_SGS_commute_ON                &
     &     .and. Fsf_bcs%sgs%ngrp_sf_dat .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'int_surf_sgs_velo_co_ele',    &
                             iphys%exp_work%i_m_phi
         call int_surf_sgs_velo_co_ele                                  &
     &     (mesh%node, mesh%ele, mesh%surf, group%surf_grp, nod_fld,    &
     &      jacs%g_FEM, jacs%jac_sf_grp, jacs%jac_sf_grp_l,             &
     &      rhs_tbl, FEM_elens, FEM_prm%npoint_poisson_int,             &
     &      Fsf_bcs%sgs%ngrp_sf_dat, Fsf_bcs%sgs%id_grp_sf_dat,         &
     &      SGS_param%ifilter_final, ak_diff, iphys%exp_work%i_m_phi,   &
     &      rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_nl)
      end if
!
!
      call cal_multi_pass_4_vector_ff                                   &
     &   (insulate%istack_ele_fld_smp, FEM_prm, mlump_ins,              &
     &    mesh%nod_comm, mesh%node, mesh%ele,                           &
     &    jacs%g_FEM, jacs%jac_3d, rhs_tbl, mhd_fem_wk%ff_m_smp,        &
     &    rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl,                    &
     &    v_sol, SR_sig, SR_r)
!
      call cal_sol_magne_insulate                                       &
     &   (nod_fld%n_point, insulate%istack_inter_fld_smp,               &
     &    insulate%numnod_fld, insulate%inod_fld, rhs_mat%f_l%ff,       &
     &    nod_fld%ntot_phys, iphys%base%i_magne, nod_fld%d_fld)
!
      call set_boundary_vect                                            &
     &   (Bnod_bcs%nod_bc_b, iphys%base%i_magne, nod_fld)
!
      call vector_send_recv(iphys%base%i_magne, mesh%nod_comm,          &
     &                      nod_fld, v_sol, SR_sig, SR_r)
!
      end subroutine cal_magnetic_co_outside
!
! -----------------------------------------------------------------------
!
      end module cal_magnetic_pre
