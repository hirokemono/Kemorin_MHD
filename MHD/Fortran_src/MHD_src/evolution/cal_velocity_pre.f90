!
!      module cal_velocity_pre
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!!      subroutine s_cal_velocity_pre(time, dt, FEM_prm, SGS_par,       &
!!     &          nod_comm, node, ele, surf, fluid, sf_grp, sf_grp_nod, &
!!     &          fl_prop, cd_prop, Vnod_bcs, Vsf_bcs, Bsf_bcs,         &
!!     &          iphys, iphys_ele, ak_MHD, fem_int, FEM_elens,         &
!!     &          ifld_sgs, icomp_sgs, ifld_diff, iphys_elediff,        &
!!     &          sgs_coefs_nod, diff_coefs, filtering, layer_tbl,      &
!!     &          mlump_fl, Vmatrix, MG_vector, wk_lsq, wk_sgs,         &
!!     &          wk_filter, mhd_fem_wk, rhs_mat, nod_fld, ele_fld,     &
!!     &          sgs_coefs)
!!      subroutine cal_velocity_co(time, FEM_prm, SGS_par,              &
!!     &         nod_comm, node, ele, surf, fluid, sf_grp, sf_grp_nod,  &
!!     &         fl_prop, Vnod_bcs, Vsf_bcs, Psf_bcs, iphys,            &
!!     &         iphys_ele, ele_fld, ak_MHD, fem_int, FEM_elens,        &
!!     &         ifld_diff, diff_coefs, mlump_fl, Vmatrix, MG_vector,   &
!!     &         mhd_fem_wk, rhs_mat, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Psf_bcs
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_terms_address), intent(in) :: ifld_sgs
!!        type(SGS_terms_address), intent(in) :: icomp_sgs
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_terms_address), intent(in) :: iphys_elediff
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(MHD_MG_matrix), intent(in) :: Vmatrix
!!        type(vectors_4_solver), intent(inout)                         &
!!       &           :: MG_vector(0:num_MG_level)
!!        type(dynamic_least_suare_data), intent(inout) :: wk_lsq
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!       type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!
      module cal_velocity_pre
!
      use m_precision
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
      use t_filtering_data
      use t_layering_ele_list
      use t_solver_djds
      use t_solver_djds_MHD
      use t_interpolate_table
      use t_material_property
      use t_SGS_model_coefs
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_vector_for_solver
      use t_bc_data_velo
      use t_surface_bc_data
      use t_physical_property
      use t_MHD_finite_element_mat
      use t_work_FEM_integration
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_cal_velocity_pre(time, dt, FEM_prm, SGS_par,         &
     &          nod_comm, node, ele, surf, fluid, sf_grp, sf_grp_nod,   &
     &          fl_prop, cd_prop, Vnod_bcs, Vsf_bcs, Bsf_bcs,           &
     &          iphys, iphys_ele, ak_MHD, fem_int, FEM_elens,           &
     &          ifld_sgs, icomp_sgs, ifld_diff, iphys_elediff,          &
     &          sgs_coefs_nod, diff_coefs, filtering, layer_tbl,        &
     &          mlump_fl, Vmatrix, MG_vector, wk_lsq, wk_sgs,           &
     &          wk_filter, mhd_fem_wk, rhs_mat, nod_fld, ele_fld,       &
     &          sgs_coefs)
!
      use nod_phys_send_recv
      use cal_sgs_fluxes
      use set_nodal_bc_id_data
      use set_surface_id_MHD
      use int_vol_diffusion_ele
      use int_vol_velo_pre
      use int_surf_velo_pre
      use int_vol_coriolis_term
      use cal_sgs_m_flux_sgs_buo
      use set_normal_field
      use evolve_by_1st_euler
      use evolve_by_adams_bashforth
      use evolve_by_lumped_crank
      use evolve_by_consist_crank
!
      real(kind = kreal), intent(in) :: time, dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_sgs
      type(SGS_terms_address), intent(in) :: icomp_sgs
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(filtering_data_type), intent(in) :: filtering
      type(layering_tbl), intent(in) :: layer_tbl
      type(lumped_mass_matrices), intent(in) :: mlump_fl
      type(MHD_MG_matrix), intent(in) :: Vmatrix
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Vmatrix%nlevel_MG)
      type(dynamic_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
!
!
!   ----  set SGS fluxes
!
!
      if(SGS_par%model_p%iflag_SGS_gravity .ne. id_SGS_none) then
        call cal_sgs_mom_flux_with_sgs_buo(dt, FEM_prm, SGS_par,        &
     &      nod_comm, node, ele, surf, fluid, layer_tbl, sf_grp,        &
     &      fl_prop, cd_prop, Vsf_bcs, Bsf_bcs, iphys, iphys_ele,       &
     &      ak_MHD, fem_int, FEM_elens, filtering,                      &
     &      ifld_sgs, icomp_sgs, ifld_diff, iphys_elediff,              &
     &      sgs_coefs_nod, diff_coefs, mlump_fl, wk_filter,             &
     &      wk_lsq, wk_sgs, mhd_fem_wk, rhs_mat,                        &
     &      nod_fld, ele_fld, sgs_coefs)
      end if
!
      if(SGS_par%model_p%iflag_SGS_m_flux .ne. id_SGS_none) then
        call cal_sgs_momentum_flux                                      &
     &     (icomp_sgs%i_mom_flux, iphys_elediff%i_velo, dt,             &
     &      FEM_prm, SGS_par%model_p, SGS_par%filter_p,                 &
     &      nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,      &
     &      fem_int%jcs, fem_int%rhs_tbl, FEM_elens, filtering,         &
     &      sgs_coefs, sgs_coefs_nod, mlump_fl, wk_filter, mhd_fem_wk,  &
     &      rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
      end if
!
      if(SGS_par%model_p%iflag_SGS_lorentz .ne. id_SGS_none) then
        call cal_sgs_maxwell                                            &
     &     (icomp_sgs%i_lorentz, iphys_elediff%i_magne, dt,             &
     &      FEM_prm, SGS_par%model_p, SGS_par%filter_p,                 &
     &      nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,      &
     &      fem_int%jcs, fem_int%rhs_tbl, FEM_elens, filtering,         &
     &      sgs_coefs, sgs_coefs_nod, mlump_fl, wk_filter, mhd_fem_wk,  &
     &      rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
      end if
!
!   --- reset work array for time evolution
!
      call reset_ff_smps(node%max_nod_smp, rhs_mat%f_l, rhs_mat%f_nl)
!
! --------   loop for direction of velocity ---------------
!
      if (fl_prop%coef_velo .gt. zero                                   &
     &        .and. fl_prop%coef_exp.gt.zero) then
        call int_vol_vector_diffuse_ele                                 &
     &     (SGS_par%model_p%ifilter_final, fluid%istack_ele_fld_smp,    &
     &      FEM_prm%npoint_t_evo_int, node, ele, nod_fld,               &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,     &
     &      FEM_elens, diff_coefs, ifld_diff%i_velo, fl_prop%coef_exp,  &
     &      ak_MHD%ak_d_velo, iphys%i_velo, rhs_mat%fem_wk,             &
     &      rhs_mat%f_l)
      end if
!
      if (fl_prop%iflag_4_coriolis .eq. id_Coriolis_ele_imp) then
         if (iflag_debug.eq.1) write(*,*) 'int_vol_coriolis_crank_ele'
        call int_vol_coriolis_crank_ele(FEM_prm%npoint_t_evo_int,       &
     &      node, ele, fluid, fl_prop,                                  &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,     &
     &      iphys%i_velo, nod_fld, rhs_mat%fem_wk, rhs_mat%f_l)
      end if
!
! -------     advection and forces
!
      if (FEM_prm%iflag_velo_supg .eq. id_turn_ON) then
        call int_vol_velo_pre_ele_upwind                                &
     &     (FEM_prm%iflag_rotate_form, FEM_prm%npoint_t_evo_int, dt,    &
     &      SGS_par%model_p, SGS_par%commute_p, node, ele, fluid,       &
     &      fl_prop, cd_prop, iphys, nod_fld, ak_MHD,                   &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,         &
     &      iphys_ele, ifld_diff%i_mom_flux, ifld_diff%i_lorentz,       &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,     &
     &      FEM_elens, diff_coefs,                                      &
     &      mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_nl)
      else if (FEM_prm%iflag_velo_supg .eq. id_magnetic_SUPG) then
        call int_vol_velo_pre_ele_upwind                                &
     &     (FEM_prm%iflag_rotate_form, FEM_prm%npoint_t_evo_int, dt,    &
     &      SGS_par%model_p, SGS_par%commute_p, node, ele, fluid,       &
     &      fl_prop, cd_prop, iphys, nod_fld, ak_MHD,                   &
     &      ele_fld%ntot_phys, iphys_ele%i_magne, ele_fld%d_fld,        &
     &      iphys_ele, ifld_diff%i_mom_flux, ifld_diff%i_lorentz,       &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,     &
     &      FEM_elens, diff_coefs,                                      &
     &      mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_nl)
      else
        call int_vol_velo_pre_ele                                       &
     &     (FEM_prm%iflag_rotate_form, FEM_prm%npoint_t_evo_int,        &
     &      SGS_par%model_p, SGS_par%commute_p, node, ele, fluid,       &
     &      fl_prop, cd_prop, iphys, nod_fld, ak_MHD,                   &
     &      ele_fld%ntot_phys, ele_fld%d_fld, iphys_ele,                &
     &      ifld_diff%i_mom_flux, ifld_diff%i_lorentz,                  &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,     &
     &      FEM_elens, diff_coefs,                                      &
     &      mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_nl)
      end if
!
!    ---  lead surface boundaries
!
      call int_surf_velo_pre_ele                                        &
     &   (ifld_diff%i_mom_flux, ifld_diff%i_lorentz,                    &
     &    ak_MHD%ak_d_velo, FEM_prm%npoint_t_evo_int,                   &
     &    SGS_par%model_p, SGS_par%commute_p,                           &
     &    node, ele, surf, sf_grp, fl_prop,                             &
     &    Vsf_bcs, Bsf_bcs, iphys, nod_fld,                             &
     &    fem_int%jcs%g_FEM, fem_int%jcs%jac_sf_grp, fem_int%rhs_tbl,   &
     &    FEM_elens, diff_coefs,                                        &
     &    rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl)
!
!
      if (fl_prop%iflag_scheme .eq. id_explicit_euler) then
        call cal_velo_pre_euler(dt, FEM_prm, nod_comm, node, ele,       &
     &    fluid, fl_prop, iphys, iphys_ele, ele_fld, fem_int%jcs%g_FEM, &
     &    fem_int%jcs%jac_3d, fem_int%rhs_tbl, mlump_fl, mhd_fem_wk,    &
     &    rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
      else if(fl_prop%iflag_scheme .eq. id_explicit_adams2) then
        call cal_velo_pre_adams(dt, FEM_prm, nod_comm, node, ele,       &
     &    fluid, fl_prop, iphys, iphys_ele, ele_fld, fem_int%jcs%g_FEM, &
     &    fem_int%jcs%jac_3d, fem_int%rhs_tbl, mlump_fl, mhd_fem_wk,    &
     &    rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
      else if(fl_prop%iflag_scheme .eq. id_Crank_nicolson) then
        call cal_velo_pre_lumped_crank(SGS_par%commute_p%iflag_c_velo,  &
     &      SGS_par%model_p%ifilter_final,                              &
     &      ifld_diff%i_velo, ak_MHD%ak_d_velo, dt, FEM_prm,            &
     &      nod_comm, node, ele, fluid, fl_prop,                        &
     &      Vnod_bcs, iphys, iphys_ele, ele_fld, fem_int%jcs%g_FEM,     &
     &      fem_int%jcs%jac_3d, fem_int%rhs_tbl, FEM_elens, diff_coefs, &
     &      mlump_fl, Vmatrix, MG_vector, mhd_fem_wk, rhs_mat%fem_wk,   &
     &      rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
      else if(fl_prop%iflag_scheme .eq. id_Crank_nicolson_cmass) then 
        call cal_velo_pre_consist_crank(SGS_par%commute_p%iflag_c_velo, &
     &      SGS_par%model_p%ifilter_final,                              &
     &      iphys%i_velo, iphys%i_pre_mom, ifld_diff%i_velo,            &
     &      ak_MHD%ak_d_velo, dt, FEM_prm, node, ele, fluid, fl_prop,   &
     &      Vnod_bcs, fem_int%jcs%g_FEM, fem_int%jcs%jac_3d,            &
     &      fem_int%rhs_tbl, FEM_elens, diff_coefs, Vmatrix,            &
     &      MG_vector, mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_l,         &
     &       rhs_mat%f_nl, nod_fld)
      end if
!
      call set_boundary_velo                                            &
     &   (time, node, Vnod_bcs, iphys%i_velo, nod_fld)
      call set_normal_velocity(sf_grp, sf_grp_nod, fl_prop,             &
     &    Vsf_bcs%normal, iphys%i_velo, nod_fld)
!
      call vector_send_recv(iphys%i_velo, nod_comm, nod_fld)
!
      end subroutine s_cal_velocity_pre
!
! ----------------------------------------------------------------------
!
      subroutine cal_velocity_co(time, dt, FEM_prm, SGS_par,            &
     &         nod_comm, node, ele, surf, fluid, sf_grp, sf_grp_nod,    &
     &         fl_prop, Vnod_bcs, Vsf_bcs, Psf_bcs, iphys,              &
     &         iphys_ele, ele_fld, ak_MHD, fem_int, FEM_elens,          &
     &         ifld_diff, diff_coefs, mlump_fl, Vmatrix, MG_vector,     &
     &         mhd_fem_wk, rhs_mat, nod_fld)
!
      use nod_phys_send_recv
      use int_vol_solenoid_correct
      use int_surf_grad_sgs
      use set_nodal_bc_id_data
      use set_surface_id_MHD
      use set_normal_field
      use cal_multi_pass
      use set_nodal_bc_id_data
      use cal_sol_vector_co_crank
      use implicit_vector_correct
!
      real(kind = kreal), intent(in) :: dt, time
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(potential_surf_bc_type), intent(in) :: Psf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_fl
      type(MHD_MG_matrix), intent(in) :: Vmatrix
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:Vmatrix%nlevel_MG)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, rhs_mat%f_l, rhs_mat%f_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'int_vol_velo_co'
      call int_vol_solenoid_co(FEM_prm%npoint_poisson_int,              &
     &    SGS_par%model_p%ifilter_final, fluid%istack_ele_fld_smp,      &
     &    iphys%i_p_phi, ifld_diff%i_velo, node, ele, nod_fld,          &
     &    fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%jcs%jac_3d_l,  &
     &    fem_int%rhs_tbl, FEM_elens, diff_coefs,                       &
     &    rhs_mat%fem_wk, rhs_mat%f_nl)
!
      if (SGS_par%commute_p%iflag_c_velo .eq. id_SGS_commute_ON         &
     &     .and. Psf_bcs%sgs%ngrp_sf_dat.gt.0) then
        if (iflag_debug.eq.1) write(*,*)                                &
                             'int_surf_sgs_velo_co_ele', iphys%i_p_phi
        call int_surf_sgs_velo_co_ele                                   &
     &     (node, ele, surf, sf_grp, nod_fld,                           &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_sf_grp,                  &
     &      fem_int%jcs%jac_sf_grp_l, fem_int%rhs_tbl,                  &
     &      FEM_elens, FEM_prm%npoint_poisson_int,                      &
     &      Psf_bcs%sgs%ngrp_sf_dat, Psf_bcs%sgs%id_grp_sf_dat,         &
     &      SGS_par%model_p%ifilter_final, diff_coefs%num_field,        &
     &      ifld_diff%i_velo, diff_coefs%ak, iphys%i_p_phi,             &
     &      rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_nl)
      end if
!
!
      if (   FEM_prm%iflag_imp_correct .eq. id_Crank_nicolson           &
     &  .or. FEM_prm%iflag_imp_correct .eq. id_Crank_nicolson_cmass)    &
     & then
        call cal_velocity_co_imp                                        &
     &     (iphys%i_velo, ifld_diff%i_velo, ak_MHD%ak_d_velo, dt,       &
     &      FEM_prm, SGS_par%model_p, SGS_par%commute_p,                &
     &      nod_comm, node, ele, fluid, fl_prop,                        &
     &      Vnod_bcs, iphys_ele, ele_fld, fem_int%jcs%g_FEM,            &
     &      fem_int%jcs%jac_3d, fem_int%rhs_tbl, FEM_elens, diff_coefs, &
     &      mlump_fl, Vmatrix, MG_vector, mhd_fem_wk,                   &
     &      rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
      else
        call cal_velocity_co_exp(iphys%i_velo, iphys%i_p_phi,           &
     &      FEM_prm, nod_comm, node, ele, fluid, fem_int%jcs%g_FEM,     &
     &      fem_int%jcs%jac_3d, fem_int%rhs_tbl, mlump_fl, mhd_fem_wk,  &
     &      rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
      end if
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_velo'
      call set_boundary_velo                                            &
     &   (time, node, Vnod_bcs, iphys%i_velo, nod_fld)
      if (iflag_debug.eq.1) write(*,*) 'set_normal_velocity'
      call set_normal_velocity(sf_grp, sf_grp_nod, fl_prop,             &
     &    Vsf_bcs%normal, iphys%i_velo, nod_fld)
!
      if(iflag_debug.eq.1) write(*,*) 'vector_send_recv(iphys%i_velo)'
      call vector_send_recv(iphys%i_velo, nod_comm, nod_fld)
      if(iflag_debug.eq.1) write(*,*) 'scalar_send_recv(iphys%i_press)'
      call scalar_send_recv(iphys%i_press, nod_comm, nod_fld)
!
      end subroutine cal_velocity_co
!
! ----------------------------------------------------------------------
!
      end module cal_velocity_pre
