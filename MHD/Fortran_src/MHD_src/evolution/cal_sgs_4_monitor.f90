!cal_sgs_4_monitor.f90
!     module cal_sgs_4_monitor
!
!     Written by H. Matsui
!
!!      subroutine cal_sgs_terms_4_monitor(dt, FEM_prm, SGS_param,      &
!!     &          filter_param, mesh, fluid, conduct, cd_prop,          &
!!     &          iphys, iphys_LES, iphys_ele_base, ele_fld, jacs,      &
!!     &          rhs_tbl, FEM_elens, sgs_coefs, filtering, mk_MHD,     &
!!     &          wk_filter, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,    &
!!     &          v_sol, SR_sig, SR_r)
!!      subroutine cal_diff_of_sgs_terms(dt, FEM_prm,                   &
!!     &          SGS_param, cmt_param, mesh, sf_grp, fluid, conduct,   &
!!     &          fl_prop, cd_prop, ht_prop, cp_prop, nod_bcs, surf_bcs,&
!!     &          iphys, iphys_LES, iphys_ele_base, ak_MHD, fem_int,    &
!!     &          FEM_elens, diff_coefs, mk_MHD, mhd_fem_wk, rhs_mat,   &
!!     &          nod_fld, ele_fld, v_sol, SR_sig, SR_r)
!!      subroutine cal_work_4_sgs_terms(FEM_prm, mesh, conduct,         &
!!     &          fl_prop, cd_prop, iphys, iphys_LES, jacs, rhs_tbl,    &
!!     &          mk_MHD, mhd_fem_wk, fem_wk, f_nl, nod_fld,            &
!!     &          v_sol, SR_sig, SR_r)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(SGS_filtering_params), intent(in) :: filter_param
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(nodal_boundarty_conditions), save :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(jacobians_type), intent(in) :: jacs
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(SGS_commutation_coefs), intent(in) :: diff_coefs
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!
      module cal_sgs_4_monitor
!
      use m_precision
!
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_mesh_data
      use t_geometry_data_MHD
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_SGS_model_addresses
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
      use t_filtering_data
      use t_bc_data_MHD
      use t_surface_bc_data_MHD
      use t_material_property
      use t_FEM_SGS_model_coefs
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_work_FEM_integration
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
      subroutine cal_sgs_terms_4_monitor(dt, FEM_prm, SGS_param,        &
     &          filter_param, mesh, fluid, conduct, cd_prop,            &
     &          iphys, iphys_LES, iphys_ele_base, ele_fld, jacs,        &
     &          rhs_tbl, FEM_elens, sgs_coefs, filtering, mk_MHD,       &
     &          wk_filter, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,      &
     &          v_sol, SR_sig, SR_r)
!
      use m_base_force_labels
      use m_SGS_term_labels
      use cal_sgs_fluxes
      use int_sgs_induction
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(iphys_LES%SGS_term%i_SGS_h_flux .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &          'lead ', trim(SGS_heat_flux%name)
        call cal_sgs_heat_flux                                          &
     &    (FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int, dt,       &
     &     SGS_param%SGS_heat%iflag_SGS_flux,                           &
     &     SGS_param%SGS_heat%itype_Csym_flux,                          &
     &     iphys%base%i_temp, iphys_LES%filter_fld%i_temp,              &
     &     iphys%base%i_velo, iphys_LES%filter_fld%i_velo,              &
     &     iphys_LES%SGS_term%i_SGS_h_flux,                             &
     &     SGS_param, filter_param, mesh%nod_comm, mesh%node, mesh%ele, &
     &     fluid, iphys_ele_base, ele_fld, jacs, rhs_tbl, FEM_elens,    &
     &     filtering, sgs_coefs%Csim_SGS_hf, mk_MHD%mlump_fl,           &
     &     wk_filter, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,           &
     &     v_sol, SR_sig, SR_r)
      end if
!
      if(iphys_LES%SGS_term%i_SGS_c_flux .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &          'lead ', trim(SGS_composit_flux%name)
        call cal_sgs_heat_flux                                          &
     &    (FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int, dt,       &
     &     SGS_param%SGS_light%iflag_SGS_flux,                          &
     &     SGS_param%SGS_light%itype_Csym_flux,                         &
     &     iphys%base%i_light, iphys_LES%filter_fld%i_light,            &
     &     iphys%base%i_velo,  iphys_LES%filter_fld%i_velo,             &
     &     iphys_LES%SGS_term%i_SGS_c_flux,                             &
     &     SGS_param, filter_param, mesh%nod_comm, mesh%node, mesh%ele, &
     &     fluid, iphys_ele_base, ele_fld, jacs, rhs_tbl, FEM_elens,    &
     &     filtering, sgs_coefs%Csim_SGS_cf, mk_MHD%mlump_fl,           &
     &     wk_filter, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,           &
     &     v_sol, SR_sig, SR_r)
      end if
!
      if(iphys_LES%SGS_term%i_SGS_m_flux .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &      'lead ', trim(SGS_momentum_flux%name)
        call cal_sgs_momentum_flux(dt, FEM_prm, SGS_param,              &
     &      filter_param, mesh%nod_comm, mesh%node, mesh%ele, fluid,    &
     &      iphys%base, iphys_LES%filter_fld, iphys_LES%SGS_term,       &
     &      iphys_LES%SGS_wk, iphys_ele_base, ele_fld, jacs, rhs_tbl,   &
     &      FEM_elens, filtering, sgs_coefs%Csim_SGS_mf,                &
     &      mk_MHD%mlump_fl,  wk_filter, mhd_fem_wk, fem_wk, f_l, f_nl, &
     &      nod_fld, v_sol, SR_sig, SR_r)
      end if
!
      if(iphys_LES%SGS_term%i_SGS_maxwell .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(SGS_maxwell_tensor%name)
        call cal_sgs_maxwell(dt, FEM_prm, SGS_param,                    &
     &      filter_param, mesh%nod_comm, mesh%node, mesh%ele, fluid,    &
     &      iphys%base, iphys_LES%filter_fld, iphys_LES%SGS_term,       &
     &      iphys_LES%SGS_wk, iphys_ele_base, ele_fld, jacs, rhs_tbl,   &
     &      FEM_elens, filtering, sgs_coefs%Csim_SGS_lor,               &
     &      mk_MHD%mlump_fl, wk_filter, mhd_fem_wk, fem_wk, f_l, f_nl,  &
     &      nod_fld, v_sol, SR_sig, SR_r)
      end if
!
      if(iphys_LES%SGS_term%i_SGS_induct_t .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &         'lead ', trim(induction_tensor%name)
        call cal_sgs_magne_induction                                    &
     &     (dt, FEM_prm, SGS_param, filter_param,                       &
     &      mesh%nod_comm, mesh%node, mesh%ele, conduct, cd_prop,       &
     &      iphys%base, iphys_LES%filter_fld, iphys_LES%SGS_term,       &
     &      iphys_ele_base, ele_fld, jacs, rhs_tbl, FEM_elens,          &
     &      filtering, sgs_coefs%Csim_SGS_uxb, mk_MHD%mlump_cd,         &
     &      mhd_fem_wk, wk_filter, fem_wk, f_l, nod_fld,                &
     &      v_sol, SR_sig, SR_r)
      end if
!
      if(iphys_LES%SGS_term%i_SGS_vp_induct .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(SGS_vecp_induction%name)
        call cal_sgs_uxb_2_monitor                                      &
     &     (dt, FEM_prm, SGS_param, filter_param,                       &
     &      mesh%nod_comm, mesh%node, mesh%ele, conduct, cd_prop,       &
     &      iphys, iphys_LES, iphys_ele_base, ele_fld, jacs, rhs_tbl,   &
     &      FEM_elens, filtering, sgs_coefs%Csim_SGS_uxb,               &
     &      mk_MHD%mlump_cd, wk_filter, mhd_fem_wk, fem_wk, f_l, f_nl,  &
     &      nod_fld, v_sol, SR_sig, SR_r)
      end if
!
      end subroutine cal_sgs_terms_4_monitor
!
!-----------------------------------------------------------------------
!
      subroutine cal_diff_of_sgs_terms(dt, FEM_prm,                     &
     &          SGS_param, cmt_param, mesh, sf_grp, fluid, conduct,     &
     &          fl_prop, cd_prop, ht_prop, cp_prop, nod_bcs, surf_bcs,  &
     &          iphys, iphys_LES, iphys_ele_base, ak_MHD, fem_int,      &
     &          FEM_elens, diff_coefs, mk_MHD, mhd_fem_wk, rhs_mat,     &
     &          nod_fld, ele_fld, v_sol, SR_sig, SR_r)
!
      use m_SGS_term_labels
      use m_diff_SGS_term_labels
      use cal_terms_for_heat
      use cal_momentum_terms
      use cal_magnetic_terms
      use int_vol_temp_monitor
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(surface_group_data), intent(in) :: sf_grp
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(base_field_address), intent(in) :: iphys_ele_base
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_commutation_coefs), intent(in) :: diff_coefs
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: i, i_fld
!
!
      if (iphys_LES%div_SGS%i_SGS_h_flux .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(div_SGS_h_flux%name)
        call cal_terms_4_heat(iphys_LES%div_SGS%i_SGS_h_flux,           &
     &      iphys%base%i_velo, iphys%base%i_temp,                       &
     &      iphys_LES%SGS_term%i_SGS_h_flux,                            &
     &      FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int,          &
     &      SGS_param%ifilter_final, SGS_param%SGS_heat%iflag_SGS_flux, &
     &      SGS_param%SGS_heat%iflag_commute_flux,                      &
     &      SGS_param%SGS_heat%iflag_commute_field, dt, FEM_prm,        &
     &      mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,              &
     &      fluid, sf_grp, ht_prop, nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, &
     &      iphys_ele_base, ele_fld, fem_int, FEM_elens,                &
     &      diff_coefs%Cdiff_SGS_hf, mk_MHD%mlump_fl, mhd_fem_wk,       &
     &      rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
      end if
!
      if (iphys_LES%div_SGS%i_SGS_c_flux .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(div_SGS_h_flux%name)
        call cal_terms_4_heat(iphys_LES%div_SGS%i_SGS_c_flux,           &
     &      iphys%base%i_velo, iphys%base%i_light,                      &
     &      iphys_LES%SGS_term%i_SGS_c_flux,                            &
     &      FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int,          &
     &      SGS_param%ifilter_final, SGS_param%SGS_light%iflag_SGS_flux,&
     &      SGS_param%SGS_light%iflag_commute_flux,                     &
     &      SGS_param%SGS_light%iflag_commute_field, dt, FEM_prm,       &
     &      mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,              &
     &      fluid, sf_grp, cp_prop, nod_bcs%Cnod_bcs, surf_bcs%Csf_bcs, &
     &      iphys_ele_base, ele_fld, fem_int, FEM_elens,                &
     &      diff_coefs%Cdiff_SGS_cf, mk_MHD%mlump_fl,                   &
     &      mhd_fem_wk, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
      end if
!
      do i = 1, nod_fld%num_phys
        i_fld = nod_fld%istack_component(i-1) + 1
        if(     i_fld .eq. iphys_LES%div_SGS%i_SGS_m_flux               &
     &     .or. i_fld .eq. iphys_LES%SGS_term%i_SGS_Lorentz) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &             write(*,*) 'lead  ', trim(nod_fld%phys_name(i))
          call cal_terms_4_momentum                                     &
     &       (i_fld, dt, FEM_prm, SGS_param, cmt_param,                 &
     &        mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,            &
     &        sf_grp, fluid, fl_prop, cd_prop,                          &
     &        surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs,                       &
     &        iphys%base, iphys%forces, iphys%div_forces,               &
     &        iphys%diffusion, iphys_LES%filter_fld,                    &
     &        iphys_LES%force_by_filter, iphys_LES%SGS_term,            &
     &        iphys_LES%div_SGS, iphys_ele_base, ak_MHD, fem_int,       &
     &        FEM_elens, diff_coefs, mk_MHD%mlump_fl, mhd_fem_wk,       &
     &        rhs_mat, nod_fld, ele_fld, v_sol, SR_sig, SR_r)
        end if
      end do
!
      if(      iphys_LES%SGS_term%i_SGS_induction .gt. 0                &
     &   .and. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(SGS_induction%name)
        call cal_terms_4_magnetic                                       &
     &    (iphys_LES%SGS_term%i_SGS_induction, ak_MHD%ak_d_magne, dt,   &
     &     FEM_prm, SGS_param, cmt_param,                               &
     &     mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,               &
     &     conduct, sf_grp, cd_prop,                                    &
     &     nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Bsf_bcs,        &
     &     iphys%base, iphys%forces, iphys%div_forces, iphys%diffusion, &
     &     iphys_LES%SGS_term, iphys_ele_base, ele_fld, fem_int,        &
     &     FEM_elens, diff_coefs%Cdiff_SGS_uxb, mk_MHD%mlump_cd,        &
     &     mhd_fem_wk, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
      end if
!
      end subroutine cal_diff_of_sgs_terms
!
!-----------------------------------------------------------------------
!
      subroutine cal_work_4_sgs_terms(FEM_prm, mesh, conduct,           &
     &          fl_prop, cd_prop, iphys, iphys_LES, jacs, rhs_tbl,      &
     &          mk_MHD, mhd_fem_wk, fem_wk, f_nl, nod_fld,              &
     &          v_sol, SR_sig, SR_r)
!
      use m_SGS_term_labels
      use products_nodal_fields_smp
      use int_sgs_induction
      use cal_sgs_buoyancy_flux
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: conduct
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if (     iphys_LES%SGS_term%i_SGS_induction .gt. 0                &
     &   .and. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(SGS_induction%name)
        call int_vol_sgs_induction(FEM_prm, mesh%nod_comm,              &
     &      mesh%node, mesh%ele, conduct, iphys, iphys_LES,             &
     &      jacs%g_FEM, jacs%jac_3d, rhs_tbl, mk_MHD%mlump_cd,          &
     &      mhd_fem_wk, fem_wk, f_nl, nod_fld, v_sol, SR_sig, SR_r)
      end if
!
!
      call cal_SGS_buoyancy_fluxes_FEM(mesh%node, fl_prop,              &
     &    iphys_LES%SGS_term, iphys_LES%SGS_ene_flux, nod_fld)
!
      call work_of_SGS_terms                                            &
     &   (iphys%base, iphys_LES%SGS_term, iphys_LES%div_SGS,            &
     &    iphys_LES%SGS_ene_flux, nod_fld)
!
      call work_of_SGS_terms(iphys_LES%filter_fld, iphys_LES%true_SGS,  &
     &    iphys_LES%true_div_SGS, iphys_LES%true_SGS_eflux, nod_fld)
!
      end subroutine cal_work_4_sgs_terms
!
!-----------------------------------------------------------------------
!
      subroutine work_of_SGS_terms                                      &
     &         (base, SGS_term, div_SGS, SGS_ene_flux, fld)
!
      use products_nodal_fields_smp
!
      type(base_field_address), intent(in) :: base
      type(SGS_term_address), intent(in) :: SGS_term
      type(SGS_term_address), intent(in) :: div_SGS
      type(SGS_ene_flux_address), intent(in) :: SGS_ene_flux
!
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      if (SGS_ene_flux%i_SGS_Lor_wk .gt. 0) then
        call cal_phys_dot_product                                       &
     &     (base%i_velo, SGS_term%i_SGS_Lorentz,                        &
     &      SGS_ene_flux%i_SGS_Lor_wk, fld)
      end if
!
      if (SGS_ene_flux%i_reynolds_wk .gt. 0) then
        call cal_phys_dot_product                                       &
     &     (base%i_velo, div_SGS%i_SGS_m_flux,                          &
     &      SGS_ene_flux%i_reynolds_wk, fld)
      end if
!
      if (SGS_ene_flux%i_SGS_temp_gen .gt. 0) then
        call cal_phys_product_4_scalar                                  &
     &     (base%i_temp, div_SGS%i_SGS_h_flux,                          &
     &      SGS_ene_flux%i_SGS_temp_gen, fld)
      end if
!
      if (SGS_ene_flux%i_SGS_comp_gen .gt. 0) then
        call cal_phys_product_4_scalar                                  &
     &     (base%i_light, div_SGS%i_SGS_c_flux,                         &
     &      SGS_ene_flux%i_SGS_comp_gen, fld)
      end if
!
      if (SGS_ene_flux%i_SGS_me_gen .gt. 0) then
        call cal_phys_dot_product                                       &
     &     (base%i_magne, SGS_term%i_SGS_induction,                     &
     &      SGS_ene_flux%i_SGS_me_gen, fld)
      end if
!$omp end parallel
!
      end subroutine work_of_SGS_terms
!
!-----------------------------------------------------------------------
!
      subroutine cal_SGS_buoyancy_fluxes_FEM                            &
     &         (node, fl_prop, iphys_SGS, iphys_sef, nod_fld)
!
      use m_SGS_enegy_flux_labels
      use cal_sgs_buoyancy_flux
!
      type(node_data), intent(in) :: node
      type(fluid_property), intent(in) :: fl_prop
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(SGS_ene_flux_address), intent(in) :: iphys_sef
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iphys_sef%i_SGS_buo_wk .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(SGS_buoyancy_flux%name)
        call cal_SGS_gravity_flux                                       &
     &     (node, fl_prop%i_grav, fl_prop%coef_buo, fl_prop%grav,       &
     &      iphys_SGS%i_SGS_h_flux, iphys_sef%i_SGS_buo_wk, nod_fld)
      end if
!
      if (iphys_sef%i_SGS_comp_buo_wk .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(SGS_comp_buoyancy_flux%name)
        call cal_SGS_gravity_flux                                       &
     &     (node, fl_prop%i_grav, fl_prop%coef_comp_buo, fl_prop%grav,  &
     &      iphys_SGS%i_SGS_h_flux, iphys_sef%i_SGS_comp_buo_wk,        &
     &      nod_fld)
      end if
!
      end subroutine cal_SGS_buoyancy_fluxes_FEM
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_4_monitor
