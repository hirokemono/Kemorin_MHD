!
!      module cal_temperature
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!!      subroutine cal_temperature_field(i_field, dt, FEM_prm, SGS_par, &
!!     &          mesh, group, fluid,  property, ref_param,             &
!!     &          nod_bcs, sf_bcs, iphys, iphys_ele, ele_fld, fem_int,  &
!!     &          FEM_elens, icomp_sgs, ifld_diff, iphys_elediff,       &
!!     &          sgs_coefs, sgs_coefs_nod, diff_coefs, filtering,      &
!!     &          mk_MHD, Smatrix, ak_MHD, MGCG_WK, FEM_SGS_wk,         &
!!     &          mhd_fem_wk, rhs_mat, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(scalar_property), intent(in) :: property
!!        type(reference_scalar_param), intent(in) :: ref_param
!!        type(nodal_bcs_4_scalar_type), intent(in) :: nod_bcs
!!        type(scaler_surf_bc_type), intent(in) :: sf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(SGS_terms_address), intent(in) :: icomp_sgs
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_terms_address), intent(in) :: iphys_elediff
!!        type(phys_data), intent(in) :: ele_fld
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(MHD_MG_matrix), intent(in) :: Smatrix
!!        type(MGCG_data), intent(inout) :: MGCG_WK
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_temperature
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_phys_constants
!
      use t_reference_scalar_param
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_mesh_data
      use t_geometry_data_MHD
      use t_surface_data
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_table_FEM_const
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_layering_ele_list
      use t_filter_elength
      use t_filtering_data
      use t_bc_data_temp
      use t_surface_bc_data
      use t_material_property
      use t_SGS_model_coefs
      use t_solver_djds_MHD
      use t_MGCG_data
      use t_work_FEM_integration
      use t_work_FEM_dynamic_SGS
!
      implicit none
!
      private :: cal_temperature_pre
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_temperature_field(i_field, dt, FEM_prm, SGS_par,   &
     &          mesh, group, fluid,  property, ref_param,               &
     &          nod_bcs, sf_bcs, iphys, iphys_ele, ele_fld, fem_int,    &
     &          FEM_elens, icomp_sgs, ifld_diff, iphys_elediff,         &
     &          sgs_coefs, sgs_coefs_nod, diff_coefs, filtering,        &
     &          mk_MHD, Smatrix, ak_MHD, MGCG_WK, FEM_SGS_wk,           &
     &          mhd_fem_wk, rhs_mat, nod_fld)
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(field_geometry_data), intent(in) :: fluid
      type(scalar_property), intent(in) :: property
      type(reference_scalar_param), intent(in) :: ref_param
      type(nodal_bcs_4_scalar_type), intent(in) :: nod_bcs
      type(scaler_surf_bc_type), intent(in) :: sf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(SGS_terms_address), intent(in) :: icomp_sgs
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(phys_data), intent(in) :: ele_fld
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
      type(filtering_data_type), intent(in) :: filtering
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(MHD_MG_matrix), intent(in) :: Smatrix
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_temperature_pre                                          &
     &   (i_field, dt, FEM_prm, SGS_par%model_p, SGS_par%commute_p,     &
     &    SGS_par%filter_p, mesh, group, fluid,                         &
     &    property, ref_param, nod_bcs, sf_bcs, iphys, iphys_ele,       &
     &    ele_fld, fem_int%jcs, fem_int%rhs_tbl, FEM_elens,             &
     &    icomp_sgs, ifld_diff, iphys_elediff, sgs_coefs,               &
     &    sgs_coefs_nod, diff_coefs, filtering, mk_MHD%mlump_fl,        &
     &    Smatrix, ak_MHD%ak_d_temp, MGCG_WK, FEM_SGS_wk%wk_filter,     &
     &    mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%surf_wk,                  &
     &    rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
      end subroutine cal_temperature_field
!
! ----------------------------------------------------------------------
!
      subroutine cal_temperature_pre(i_field, dt, FEM_prm,              &
     &          SGS_param, cmt_param, filter_param, mesh, group,        &
     &          fluid, property, ref_param, nod_bcs, sf_bcs,            &
     &          iphys, iphys_ele, ele_fld, jacs, rhs_tbl,               &
     &          FEM_elens, icomp_sgs, ifld_diff, iphys_elediff,         &
     &          sgs_coefs, sgs_coefs_nod, diff_coefs, filtering,        &
     &          mlump_fl, Smatrix, ak_diffuse, MGCG_WK, wk_filter,      &
     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      use nod_phys_send_recv
      use cal_sgs_fluxes
      use set_boundary_scalars
      use int_vol_diffusion_ele
      use int_vol_thermal_ele
      use int_surf_div_fluxes_sgs
      use int_surf_fixed_gradients
      use cal_stratification_by_temp
      use evolve_by_1st_euler
      use evolve_by_adams_bashforth
      use evolve_by_lumped_crank
      use evolve_by_consist_crank
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(field_geometry_data), intent(in) :: fluid
      type(scalar_property), intent(in) :: property
      type(reference_scalar_param), intent(in) :: ref_param
      type(nodal_bcs_4_scalar_type), intent(in) :: nod_bcs
      type(scaler_surf_bc_type), intent(in) :: sf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: icomp_sgs
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(filtering_data_type), intent(in) :: filtering
      type(lumped_mass_matrices), intent(in) :: mlump_fl
      type(MHD_MG_matrix), intent(in) :: Smatrix
!
      real(kind = kreal), intent(in) :: ak_diffuse(mesh%ele%numele)
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
!      call check_jacobians_triquad(mesh%ele, jacs%jac_3d)
!
      if (SGS_param%iflag_SGS_h_flux .ne. id_SGS_none) then
        call cal_sgs_heat_flux                                          &
     &     (FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int, dt,      &
     &      SGS_param%iflag_SGS_h_flux, SGS_param%itype_Csym_h_flux,    &
     &      i_field, iphys%filter_fld%i_temp,                           &
     &      iphys%base%i_velo, iphys%filter_fld%i_velo,                 &
     &      iphys%SGS_term%i_SGS_h_flux,                                &
     &      icomp_sgs%i_heat_flux, iphys_elediff%i_velo, SGS_param,     &
     &      filter_param, mesh%nod_comm, mesh%node, mesh%ele, fluid,    &
     &      iphys_ele, ele_fld, jacs, rhs_tbl, FEM_elens, filtering,    &
     &      sgs_coefs, sgs_coefs_nod, mlump_fl, wk_filter,              &
     &      mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, 3, iphys%SGS_term%i_SGS_h_flux)
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_ff_smps(mesh%node%max_nod_smp, f_l, f_nl)
!
!  ----------  lead diffusion term
!
      if (property%coef_advect .gt. zero                                &
     &     .and. property%coef_exp .gt. zero) then
        call int_vol_scalar_diffuse_ele(SGS_param%ifilter_final,        &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,      &
     &      rhs_tbl, FEM_elens, diff_coefs, ifld_diff%i_temp,           &
     &      property%coef_exp,  ak_diffuse, i_field, fem_wk, f_l)
      end if
!
!  ----------  lead advection term
!
      if (FEM_prm%iflag_temp_supg .gt. id_turn_OFF) then
        call int_vol_temp_ele_upw                                       &
     &     (SGS_param%iflag_SGS_h_flux, cmt_param%iflag_c_hf,           &
     &      SGS_param%ifilter_final, FEM_prm%npoint_t_evo_int,          &
     &      dt, iphys%base%i_temp, iphys%base%i_velo,                   &
     &      iphys%SGS_term%i_SGS_h_flux, ifld_diff%i_heat_flux,         &
     &      mesh%node, mesh%ele, fluid, property, nod_fld,              &
     &      jacs%g_FEM, jacs%jac_3d, rhs_tbl, FEM_elens, diff_coefs,    &
     &      ele_fld%ntot_phys, iphys_ele%base%i_velo, ele_fld%d_fld,    &
     &      mhd_fem_wk, fem_wk, f_nl)
      else
        call int_vol_temp_ele                                           &
     &     (SGS_param%iflag_SGS_h_flux, cmt_param%iflag_c_hf,           &
     &      SGS_param%ifilter_final, FEM_prm%npoint_t_evo_int,          &
     &      iphys%base%i_temp, iphys%base%i_velo,                       &
     &      iphys%SGS_term%i_SGS_h_flux, ifld_diff%i_heat_flux,         &
     &      mesh%node, mesh%ele, fluid, property, nod_fld,              &
     &      jacs%g_FEM, jacs%jac_3d, rhs_tbl, FEM_elens, diff_coefs,    &
     &      ele_fld%ntot_phys, iphys_ele%base%i_velo, ele_fld%d_fld,    &
     &      mhd_fem_wk, fem_wk, f_nl)
      end if
!
!      call check_ff_smp(my_rank, n_scalar, mesh%node%max_nod_smp, f_l)
!      call check_ff_smp(my_rank, n_scalar, mesh%node%max_nod_smp, f_nl)
!
      call int_sf_scalar_flux                                           &
     &   (mesh%node, mesh%ele, mesh%surf, group%surf_grp, jacs%g_FEM,   &
     &    jacs%jac_sf_grp, rhs_tbl, sf_bcs%flux,                        &
     &    FEM_prm%npoint_t_evo_int, ak_diffuse, fem_wk, f_l)
!
      if(cmt_param%iflag_c_temp .ne. id_SGS_commute_OFF                 &
          .and. SGS_param%iflag_SGS_h_flux .ne. id_SGS_none) then
        call int_sf_skv_sgs_div_v_flux(mesh%node, mesh%ele, mesh%surf,  &
     &      group%surf_grp, nod_fld, jacs%g_FEM, jacs%jac_sf_grp,       &
     &      rhs_tbl, FEM_elens, FEM_prm%npoint_t_evo_int,               &
     &      sf_bcs%sgs%ngrp_sf_dat, sf_bcs%sgs%id_grp_sf_dat,           &
     &      SGS_param%ifilter_final, iphys%SGS_term%i_SGS_h_flux,       &
     &      iphys%base%i_velo, iphys%base%i_temp,                       &
     &      diff_coefs%num_field, ifld_diff%i_heat_flux,                &
     &      diff_coefs%ak, property%coef_advect, fem_wk, surf_wk, f_nl)
      end if
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_scalar, i_field)
!      call check_nodal_data                                            &
!     &   ((50+my_rank), ele_fld, n_vector, iphys_ele%base%i_velo)
!      call check_ff_smp(my_rank, n_scalar, mesh%node%max_nod_smp, f_l)
!      call check_ff_smp(my_rank, n_scalar, mesh%node%max_nod_smp, f_nl)
!
      if (ref_param%iflag_reference .eq. id_takepiro_temp) then
        if (FEM_prm%iflag_temp_supg .gt. id_turn_OFF) then
          call cal_stratified_layer_upw                                 &
     &       (iphys%grad_fld%i_grad_ref_t, FEM_prm%npoint_t_evo_int,    &
     &        dt, mesh%node, mesh%ele, fluid, nod_fld,                  &
     &        ele_fld%ntot_phys, iphys_ele%base%i_velo, ele_fld%d_fld,  &
     &        jacs%g_FEM, jacs%jac_3d, rhs_tbl, mhd_fem_wk,             &
     &        fem_wk, f_nl)
        else
          call cal_stratified_layer                                     &
     &       (iphys%grad_fld%i_grad_ref_t, FEM_prm%npoint_t_evo_int,    &
     &        mesh%node, mesh%ele, fluid, nod_fld,                      &
     &        ele_fld%ntot_phys, iphys_ele%base%i_velo, ele_fld%d_fld,  &
     &        jacs%g_FEM, jacs%jac_3d, rhs_tbl, mhd_fem_wk,             &
     &        fem_wk, f_nl)
        end if
      end if
!
!
      if (property%iflag_scheme .eq. id_explicit_euler) then
        call cal_scalar_pre_euler(FEM_prm%iflag_temp_supg, i_field, dt, &
     &      FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,         &
     &      iphys_ele, ele_fld, jacs%g_FEM, jacs%jac_3d, rhs_tbl,       &
     &      mlump_fl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      else if (property%iflag_scheme .eq. id_explicit_adams2) then
        call cal_scalar_pre_adams(FEM_prm%iflag_temp_supg, i_field,     &
     &      iphys%exp_work%i_pre_heat, dt,                              &
     &      FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,         &
     &      iphys_ele, ele_fld, jacs%g_FEM, jacs%jac_3d, rhs_tbl,       &
     &      mlump_fl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      else if (property%iflag_scheme .eq. id_Crank_nicolson) then
        call cal_temp_pre_lumped_crank(FEM_prm%iflag_temp_supg,         &
     &      cmt_param%iflag_c_temp, SGS_param%ifilter_final,            &
     &      i_field, iphys%exp_work%i_pre_heat, ifld_diff%i_temp,       &
     &      ak_diffuse, FEM_prm%eps_4_temp_crank, dt, FEM_prm,          &
     &      mesh%nod_comm, mesh%node, mesh%ele, fluid, property,        &
     &      nod_bcs, iphys_ele, ele_fld, jacs%g_FEM, jacs%jac_3d,       &
     &      rhs_tbl, FEM_elens, diff_coefs, mlump_fl, Smatrix,          &
     &      MGCG_WK%MG_vector, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      else if (property%iflag_scheme .eq. id_Crank_nicolson_cmass) then 
        call cal_temp_pre_consist_crank                                 &
     &     (cmt_param%iflag_c_temp, SGS_param%ifilter_final,            &
     &      i_field, iphys%exp_work%i_pre_heat, ifld_diff%i_temp,       &
     &      ak_diffuse, FEM_prm%eps_4_temp_crank, dt, FEM_prm,          &
     &      mesh%node, mesh%ele, fluid, property, nod_bcs, jacs%g_FEM,  &
     &      jacs%jac_3d, rhs_tbl, FEM_elens, diff_coefs, Smatrix,       &
     &      MGCG_WK%MG_vector, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      call set_boundary_scalar                                          &
     &   (nod_bcs%nod_bc_s, i_field, nod_fld)
!
      call scalar_send_recv(i_field, mesh%nod_comm, nod_fld)
!
      end subroutine cal_temperature_pre
!
! ----------------------------------------------------------------------
!
      end module cal_temperature
