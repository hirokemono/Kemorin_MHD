!cal_sgs_4_monitor.f90
!     module cal_sgs_4_monitor
!
!     Written by H. Matsui
!
!!      subroutine cal_sgs_terms_4_monitor                              &
!!     &         (nod_comm, node, ele, fluid, conduct, cd_prop, iphys,  &
!!     &          iphys_ele, ele_fld,  jac_3d, rhs_tbl, FEM_elens,      &
!!     &          icomp_sgs, iphys_elediff, sgs_coefs, sgs_coefs_nod,   &
!!     &          filtering, wk_filter, mhd_fem_wk, fem_wk,             &
!!     &          f_l, f_nl, nod_fld)
!!      subroutine cal_diff_of_sgs_terms(nod_comm, node, ele, surf,     &
!!     &          sf_grp, fluid, conduct, fl_prop, cd_prop, ht_prop,    &
!!     &          nod_bcs, surf_bcs, iphys, iphys_ele, ak_MHD,          &
!!     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,               &
!!     &          ifld_diff, diff_coefs, mhd_fem_wk, fem_wk, surf_wk,   &
!!     &          f_l, f_nl, nod_fld, ele_fld)
!!      subroutine cal_work_4_sgs_terms(nod_comm, node, ele, conduct,   &
!!     &          fl_prop, iphys, jac_3d, rhs_tbl, mhd_fem_wk, fem_wk,  &
!!     &          f_nl, nod_fld)
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
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_terms_address), intent(in) :: icomp_sgs
!!        type(SGS_terms_address), intent(in) :: iphys_elediff
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!
      module cal_sgs_4_monitor
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_phys_labels
!
      use t_physical_property
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
      use t_bc_data_MHD
      use t_MHD_boundary_data
      use t_material_property
      use t_SGS_model_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_terms_4_monitor                                &
     &         (nod_comm, node, ele, fluid, conduct, cd_prop, iphys,    &
     &          iphys_ele, ele_fld,  jac_3d, rhs_tbl, FEM_elens,        &
     &          icomp_sgs, iphys_elediff, sgs_coefs, sgs_coefs_nod,     &
     &          filtering, wk_filter, mhd_fem_wk, fem_wk,               &
     &          f_l, f_nl, nod_fld)
!
      use cal_sgs_fluxes
      use int_sgs_induction
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_terms_address), intent(in) :: icomp_sgs
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iphys%i_SGS_h_flux .gt. 0) then
        if(iflag_debug.gt.0) write(*,*) 'lead ', trim(fhd_SGS_h_flux)
        call cal_sgs_heat_flux(iflag_temp_supg,                         &
     &      SGS_param1%iflag_SGS_h_flux, SGS_param1%itype_Csym_h_flux,  &
     &      iphys%i_sgs_temp, iphys%i_filter_temp,                      &
     &      iphys%i_velo, iphys%i_filter_velo, iphys%i_SGS_h_flux,      &
     &      icomp_sgs%i_heat_flux, iphys_elediff%i_velo, SGS_param1,    &
     &      nod_comm, node, ele, fluid, iphys_ele, ele_fld,             &
     &      jac_3d, rhs_tbl, FEM_elens, filtering,                      &
     &      sgs_coefs, sgs_coefs_nod, wk_filter, mhd_fem_wk, fem_wk,    &
     &      f_l, f_nl, nod_fld)
      end if
!
      if (iphys%i_SGS_c_flux .gt. 0) then
        if(iflag_debug.gt.0) write(*,*) 'lead ', trim(fhd_SGS_c_flux)
        call cal_sgs_heat_flux(iflag_comp_supg,                         &
     &      SGS_param1%iflag_SGS_c_flux, SGS_param1%itype_Csym_c_flux,  &
     &      iphys%i_sgs_composit, iphys%i_filter_comp,                  &
     &      iphys%i_velo, iphys%i_filter_velo, iphys%i_SGS_c_flux,      &
     &      icomp_sgs%i_comp_flux, iphys_elediff%i_velo, SGS_param1,    &
     &      nod_comm, node, ele, fluid, iphys_ele, ele_fld,             &
     &      jac_3d, rhs_tbl, FEM_elens, filtering,                      &
     &      sgs_coefs, sgs_coefs_nod, wk_filter, mhd_fem_wk, fem_wk,    &
     &      f_l, f_nl, nod_fld)
      end if
!
      if (iphys%i_SGS_m_flux .gt. 0) then
        if(iflag_debug.gt.0) write(*,*) 'lead ', trim(fhd_SGS_m_flux)
        call cal_sgs_momentum_flux                                      &
     &     (icomp_sgs%i_mom_flux, iphys_elediff%i_velo, SGS_param1,     &
     &      nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,      &
     &      jac_3d, rhs_tbl, FEM_elens, filtering,                      &
     &      sgs_coefs, sgs_coefs_nod, wk_filter, mhd_fem_wk, fem_wk,    &
     &      f_l, f_nl, nod_fld)
      end if
!
      if (iphys%i_SGS_maxwell .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(fhd_SGS_maxwell_t)
        call cal_sgs_maxwell                                            &
     &     (icomp_sgs%i_lorentz, iphys_elediff%i_magne, SGS_param1,     &
     &      nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,      &
     &      jac_3d, rhs_tbl, FEM_elens, filtering,                      &
     &      sgs_coefs, sgs_coefs_nod, wk_filter, mhd_fem_wk, fem_wk,    &
     &      f_l, f_nl, nod_fld)
      end if
!
      if (iphys%i_SGS_induct_t .gt. 0) then
        if(iflag_debug.gt.0) write(*,*) 'lead ', trim(fhd_induct_t)
        call cal_sgs_magne_induction(icomp_sgs%i_induction,             &
     &      iphys_elediff%i_velo, iphys_elediff%i_magne,                &
     &      SGS_param1, nod_comm, node, ele, conduct, cd_prop, iphys,   &
     &      iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens, filtering,  &
     &      sgs_coefs, sgs_coefs_nod, wk_filter, mhd_fem_wk, fem_wk,    &
     &      f_l, nod_fld)
      end if
!
      if (iphys%i_SGS_vp_induct .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(fhd_SGS_vp_induct)
        call cal_sgs_uxb_2_monitor                                      &
     &     (icomp_sgs%i_induction, iphys_elediff%i_velo, SGS_param1,    &
     &      nod_comm, node, ele, conduct, cd_prop, iphys, iphys_ele,    &
     &      ele_fld, jac_3d, rhs_tbl, FEM_elens, filtering, sgs_coefs,  &
     &      wk_filter, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      end subroutine cal_sgs_terms_4_monitor
!
!-----------------------------------------------------------------------
!
      subroutine cal_diff_of_sgs_terms(nod_comm, node, ele, surf,       &
     &          sf_grp, fluid, conduct, fl_prop, cd_prop, ht_prop,      &
     &          nod_bcs, surf_bcs, iphys, iphys_ele, ak_MHD,            &
     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,                 &
     &          ifld_diff, diff_coefs, mhd_fem_wk, fem_wk, surf_wk,     &
     &          f_l, f_nl, nod_fld, ele_fld)
!
      use cal_terms_for_heat
      use cal_momentum_terms
      use cal_magnetic_terms
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop
      type(surface_group_data), intent(in) :: sf_grp
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
!
      integer(kind = kint) :: i, i_fld
!
!
      if (iphys%i_SGS_div_h_flux .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(fhd_div_SGS_h_flux)
        call cal_terms_4_heat(iphys%i_SGS_div_h_flux,                   &
     &      nod_comm, node, ele, surf, fluid, sf_grp, ht_prop,          &
     &      nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, iphys,                  &
     &      iphys_ele, ele_fld,  jac_3d, jac_sf_grp, rhs_tbl,           &
     &      FEM_elens, ifld_diff, diff_coefs, mhd_fem_wk, fem_wk,       &
     &      surf_wk, f_l, f_nl, nod_fld)
      end if
!
      do i = 1, nod_fld%num_phys
        i_fld = nod_fld%istack_component(i-1) + 1
        if(     i_fld .eq. iphys%i_SGS_div_m_flux                       &
     &     .or. i_fld .eq. iphys%i_SGS_Lorentz) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &             write(*,*) 'lead  ', trim(nod_fld%phys_name(i))
          call cal_terms_4_momentum                                     &
     &       (i_fld, ifld_diff%i_mom_flux, ifld_diff%i_lorentz,         &
     &        nod_comm, node, ele, surf,                                &
     &        sf_grp, fluid, fl_prop, cd_prop,                          &
     &        surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs, iphys, iphys_ele,     &
     &        ak_MHD, jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,           &
     &        diff_coefs, mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,       &
     &        nod_fld, ele_fld)
        end if
      end do
!
      if (      iphys%i_SGS_induction .gt. 0                            &
     &   .and. evo_magne%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(fhd_SGS_induction)
        call cal_terms_4_magnetic(iphys%i_SGS_induction,                &
     &      ifld_diff%i_induction, ak_MHD%ak_d_magne,                   &
     &      nod_comm, node, ele, surf, conduct, sf_grp, cd_prop,        &
     &      nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Bsf_bcs,       &
     &      iphys, iphys_ele, ele_fld, jac_3d, jac_sf_grp, rhs_tbl,     &
     &      FEM_elens, diff_coefs, mhd_fem_wk, fem_wk, surf_wk,         &
     &      f_l, f_nl, nod_fld)
      end if
!
!
!      if (iphys%i_SGS_buoyancy .gt. 0) then
!        if(iflag_debug.gt.0) write(*,*)                                &
!     &        'lead ', trim(fhd_SGS_buoyancy)
!         call cal_terms_4_momentum(iphys%i_SGS_buoyancy)
!      end if
!
!      if (iphys%i_SGS_comp_buo .gt. 0) then
!        if(iflag_debug.gt.0) write(*,*)                                &
!     &        'lead ', trim(fhd_SGS_comp_buo)
!         call cal_terms_4_momentum(iphys%i_SGS_comp_buo)
!      end if
!
      end subroutine cal_diff_of_sgs_terms
!
!-----------------------------------------------------------------------
!
      subroutine cal_work_4_sgs_terms(nod_comm, node, ele, conduct,     &
     &          fl_prop, iphys, jac_3d, rhs_tbl, mhd_fem_wk, fem_wk,    &
     &          f_nl, nod_fld)
!
      use m_physical_property
!
      use products_nodal_fields_smp
      use int_sgs_induction
      use sgs_buoyancy_flux
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(fluid_property), intent(in) :: fl_prop
      type(phys_address), intent(in) :: iphys
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (     iphys%i_SGS_induction .gt. 0                             &
     &   .and. evo_vect_p%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(fhd_SGS_induction)
        call int_vol_sgs_induction                                      &
     &     (nod_comm, node, ele, conduct, iphys, jac_3d,                &
     &      rhs_tbl, mhd_fem_wk, fem_wk, f_nl, nod_fld)
      end if
!
!
!$omp parallel
      if (iphys%i_SGS_temp_gen .gt. 0) then
        call cal_phys_product_4_scalar                                  &
     &     (iphys%i_temp, iphys%i_SGS_div_h_flux, iphys%i_SGS_temp_gen, &
     &      nod_fld)
      end if
!
!
      if (iphys%i_reynolds_wk .gt. 0) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_velo, iphys%i_SGS_div_m_flux, iphys%i_reynolds_wk,  &
     &      nod_fld)
      end if
!
      if (iphys%i_SGS_Lor_wk .gt. 0) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_velo, iphys%i_SGS_Lorentz, iphys%i_SGS_Lor_wk,      &
     &      nod_fld)
      end if
!
      if (iphys%i_SGS_me_gen .gt. 0) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_magne, iphys%i_SGS_induction, iphys%i_SGS_me_gen,   &
     &      nod_fld)
      end if
!$omp end parallel
!
      if (iphys%i_SGS_buo_wk .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(fhd_SGS_buo_flux)
        call cal_SGS_gravity_flux                                       &
     &     (node, fl_prop%i_grav, fl_prop%coef_buo, fl_prop%grav,       &
     &      iphys%i_SGS_h_flux, iphys%i_SGS_buo_wk, nod_fld)
      end if
!
      if (iphys%i_SGS_comp_buo_wk .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(fhd_SGS_comp_buo_flux)
        call cal_SGS_gravity_flux                                       &
     &     (node, fl_prop%i_grav, fl_prop%coef_comp_buo, fl_prop%grav,  &
     &      iphys%i_SGS_h_flux, iphys%i_SGS_comp_buo_wk, nod_fld)
      end if
!
!
!$omp parallel
      if (iphys%i_SGS_Lor_wk_tr .gt. 0) then
          call cal_phys_dot_product                                     &
     &       (iphys%i_filter_velo, iphys%i_SGS_Lor_true,                &
     &        iphys%i_SGS_Lor_wk_tr, nod_fld)
      end if
!
      if (iphys%i_reynolds_wk_tr .gt. 0) then
          call cal_phys_dot_product                                     &
     &       (iphys%i_filter_velo, iphys%i_SGS_div_mf_true,             &
     &        iphys%i_reynolds_wk_tr, nod_fld)
      end if
!
      if (iphys%i_SGS_t_gen_tr .gt. 0) then
          call cal_phys_product_4_scalar                                &
     &       (iphys%i_filter_temp, iphys%i_SGS_div_hf_true,             &
     &        iphys%i_SGS_t_gen_tr, nod_fld)
      end if
!
      if (iphys%i_SGS_c_gen_tr .gt. 0) then
          call cal_phys_product_4_scalar                                &
     &       (iphys%i_filter_temp, iphys%i_SGS_div_cf_true,             &
     &        iphys%i_SGS_c_gen_tr, nod_fld)
      end if
!
      if (iphys%i_SGS_me_gen_tr .gt. 0) then
          call cal_phys_dot_product                                     &
     &       (iphys%i_filter_magne, iphys%i_SGS_idct_true,              &
     &        iphys%i_SGS_me_gen_tr, nod_fld)
      end if
!$omp end parallel
!
      end subroutine cal_work_4_sgs_terms
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_4_monitor
