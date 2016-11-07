!
!      module cal_temperature
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!!      subroutine cal_temperature_field(nod_comm, node, ele, surf,     &
!!     &          fluid, sf_grp, Tnod_bcs, Tsf_bcs, iphys,              &
!!     &          iphys_ele, ele_fld, jac_3d, jac_sf_grp, rhs_tbl,      &
!!     &          FEM_elens, icomp_sgs, ifld_diff, iphys_elediff,       &
!!     &          sgs_coefs, sgs_coefs_nod, diff_coefs, filtering,      &
!!     &          Tmatrix, ak_d_temp, wk_filter, mhd_fem_wk, fem_wk,    &
!!     &          surf_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_parturbation_temp(nod_comm, node, ele, surf,     &
!!     &          fluid, sf_grp, Tnod_bcs, Tsf_bcs, iphys,              &
!!     &          iphys_ele, ele_fld, jac_3d, jac_sf_grp, rhs_tbl,      &
!!     &          FEM_elens, icomp_sgs, ifld_diff, iphys_elediff,       &
!!     &          sgs_coefs, sgs_coefs_nod, diff_coefs, filtering,      &
!!     &          Tmatrix, ak_d_temp, wk_filter, mhd_fem_wk, fem_wk,    &
!!     &          surf_wk, f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(nodal_bcs_4_scalar_type), intent(in) :: Tnod_bcs
!!        type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_terms_address), intent(in) :: icomp_sgs
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_terms_address), intent(in) :: iphys_elediff
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(MHD_MG_matrix), intent(in) :: Tmatrix
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_temperature
!
      use m_precision
      use m_machine_parameter
!
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
      use t_bc_data_temp
      use t_surface_bc_data
      use t_material_property
      use t_solver_djds_MHD
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_temperature_field(nod_comm, node, ele, surf,       &
     &          fluid, sf_grp, Tnod_bcs, Tsf_bcs, iphys,                &
     &          iphys_ele, ele_fld, jac_3d, jac_sf_grp, rhs_tbl,        &
     &          FEM_elens, icomp_sgs, ifld_diff, iphys_elediff,         &
     &          sgs_coefs, sgs_coefs_nod, diff_coefs, filtering,        &
     &          Tmatrix, ak_d_temp, wk_filter, mhd_fem_wk, fem_wk,      &
     &          surf_wk, f_l, f_nl, nod_fld)
!
      use m_phys_constants
      use m_control_parameter
      use m_t_int_parameter
      use m_type_AMG_data
!
      use nod_phys_send_recv
      use cal_sgs_fluxes
      use set_boundary_scalars
      use int_vol_diffusion_ele
      use int_surf_temp
      use int_vol_thermal_ele
      use cal_stratification_by_temp
      use copy_nodal_fields
      use evolve_by_1st_euler
      use evolve_by_adams_bashforth
      use evolve_by_lumped_crank
      use evolve_by_consist_crank
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: fluid
      type(nodal_bcs_4_scalar_type), intent(in) :: Tnod_bcs
      type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: icomp_sgs
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(filtering_data_type), intent(in) :: filtering
      type(MHD_MG_matrix), intent(in) :: Tmatrix
!
      real(kind = kreal), intent(in) :: ak_d_temp(ele%numele)
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
!      call check_jacobians_triquad(ele, jac_3d)
!
      if (iflag_SGS_heat .ne. id_SGS_none) then
        call cal_sgs_heat_flux                                          &
     &     (icomp_sgs%i_heat_flux, iphys_elediff%i_velo,                &
     &      nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,      &
     &      jac_3d, rhs_tbl, FEM_elens, filtering, sgs_coefs,           &
     &      sgs_coefs_nod, wk_filter, mhd_fem_wk, fem_wk,               &
     &      f_l, f_nl, nod_fld)
      end if
!
!      call check_nodal_data                                            &
!     &     ((50+my_rank), nod_fld, 3, iphys%i_SGS_h_flux)
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
!  ----------  lead diffusion term
!
      if (coef_temp.gt.zero .and. coef_exp_t.gt.zero) then
        call int_vol_scalar_diffuse_ele(fluid%istack_ele_fld_smp,       &
     &      node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs, &
     &      ifld_diff%i_temp, coef_exp_t, ak_d_temp, iphys%i_temp,      &
     &      fem_wk, f_l)
      end if
!
!  ----------  lead advection term
!
      if (iflag_temp_supg .gt. id_turn_OFF) then
        call int_vol_temp_ele_upw(node, ele, fluid, iphys, nod_fld,     &
     &      jac_3d, rhs_tbl, FEM_elens, diff_coefs,                     &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,         &
     &      ifld_diff%i_heat_flux, mhd_fem_wk, fem_wk, f_nl)
      else
        call int_vol_temp_ele(node, ele, fluid, iphys, nod_fld,         &
     &      jac_3d, rhs_tbl, FEM_elens, diff_coefs,                     &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,         &
     &      ifld_diff%i_heat_flux, mhd_fem_wk, fem_wk, f_nl)
      end if
!
!      call check_ff_smp(my_rank, n_scalar, node%max_nod_smp, f_l)
!      call check_ff_smp(my_rank, n_scalar, node%max_nod_smp, f_nl)
!
      call int_surf_temp_ele(ifld_diff%i_heat_flux, ak_d_temp,          &
     &    node, ele, surf, sf_grp, iphys, nod_fld, Tsf_bcs, jac_sf_grp, &
     &    rhs_tbl, FEM_elens, diff_coefs, fem_wk, surf_wk, f_l, f_nl)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_scalar, iphys%i_temp)
!      call check_nodal_data                                            &
!     &   ((50+my_rank), ele_fld,  n_vector, iphys_ele%i_velo)
!      call check_ff_smp(my_rank, n_scalar, node%max_nod_smp, f_l)
!      call check_ff_smp(my_rank, n_scalar, node%max_nod_smp, f_nl)
!
      if (iflag_t_strat .gt. id_turn_OFF) then
        if (iflag_temp_supg .gt. id_turn_OFF) then
          call cal_stratified_layer_upw                                 &
     &       (node, ele, fluid, iphys, nod_fld,                         &
     &        ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,       &
     &        jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_nl)
        else
          call cal_stratified_layer(node, ele, fluid, iphys, nod_fld,   &
     &        ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,       &
     &        jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_nl)
        end if
      end if
!
!
      if (iflag_t_evo_4_temp .eq. id_explicit_euler) then
        call cal_scalar_pre_euler(iflag_temp_supg, iphys%i_temp,        &
     &      nod_comm, node, ele, fluid, iphys_ele, ele_fld,  jac_3d,    &
     &      rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      else if (iflag_t_evo_4_temp .eq. id_explicit_adams2) then
        call cal_scalar_pre_adams                                       &
     &     (iflag_temp_supg, iphys%i_temp, iphys%i_pre_heat,            &
     &      nod_comm, node, ele, fluid, iphys_ele, ele_fld, jac_3d,     &
     &      rhs_tbl, mhd_fem_wk, fem_wk,  f_l, f_nl,  nod_fld)
      else if (iflag_t_evo_4_temp .eq. id_Crank_nicolson) then
        call cal_temp_pre_lumped_crank                                  &
     &     (iphys%i_temp, iphys%i_pre_heat, ifld_diff%i_temp,           &
     &      ak_d_temp, nod_comm, node, ele, fluid, Tnod_bcs,            &
     &      iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs, &
     &      Tmatrix, MG_vector, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      else if (iflag_t_evo_4_temp .eq. id_Crank_nicolson_cmass) then 
        call cal_temp_pre_consist_crank(iphys%i_temp, iphys%i_pre_heat, &
     &      ifld_diff%i_temp, ak_d_temp,                                &
     &      node, ele, fluid, Tnod_bcs, jac_3d, rhs_tbl, FEM_elens,     &
     &      diff_coefs, Tmatrix, MG_vector, mhd_fem_wk, fem_wk,         &
     &      f_l, f_nl, nod_fld)
      end if
!
      call set_boundary_scalar                                          &
     &   (Tnod_bcs%nod_bc_s, iphys%i_temp, nod_fld)
!
      call scalar_send_recv(iphys%i_temp, nod_comm, nod_fld)
!
      if (iphys%i_par_temp .gt. 0) then
        call subtract_2_nod_scalars(node, nod_fld,                      &
     &      iphys%i_temp, iphys%i_ref_t, iphys%i_par_temp)
      end if
!
      end subroutine cal_temperature_field
!
! ----------------------------------------------------------------------
!
      subroutine cal_parturbation_temp(nod_comm, node, ele, surf,       &
     &          fluid, sf_grp, Tnod_bcs, Tsf_bcs, iphys,                &
     &          iphys_ele, ele_fld, jac_3d, jac_sf_grp, rhs_tbl,        &
     &          FEM_elens, icomp_sgs, ifld_diff, iphys_elediff,         &
     &          sgs_coefs, sgs_coefs_nod, diff_coefs, filtering,        &
     &          Tmatrix, ak_d_temp, wk_filter, mhd_fem_wk, fem_wk,      &
     &          surf_wk, f_l, f_nl, nod_fld)
!
      use m_phys_constants
      use m_control_parameter
      use m_t_int_parameter
      use m_type_AMG_data
!
      use nod_phys_send_recv
      use cal_sgs_fluxes
      use set_boundary_scalars
      use int_vol_diffusion_ele
      use int_surf_temp
      use int_vol_thermal_ele
      use cal_stratification_by_temp
      use copy_nodal_fields
      use evolve_by_1st_euler
      use evolve_by_adams_bashforth
      use evolve_by_lumped_crank
      use evolve_by_consist_crank
!
!      use check_surface_groups
!      use check_jacobians
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: fluid
      type(nodal_bcs_4_scalar_type), intent(in) :: Tnod_bcs
      type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: icomp_sgs
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(filtering_data_type), intent(in) :: filtering
      type(MHD_MG_matrix), intent(in) :: Tmatrix
!
      real(kind = kreal), intent(in) :: ak_d_temp(ele%numele)
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_SGS_heat .ne. id_SGS_none) then
        call cal_sgs_heat_flux                                          &
     &     (icomp_sgs%i_heat_flux, iphys_elediff%i_velo,                &
     &      nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,      &
     &      jac_3d, rhs_tbl, FEM_elens, filtering, sgs_coefs,           &
     &      sgs_coefs_nod, wk_filter, mhd_fem_wk, fem_wk,               &
     &      f_l, f_nl, nod_fld)
      end if
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, 3, iphys%i_SGS_h_flux)
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
!  ----------  lead diffusion term
!
      if (coef_temp.gt.zero .and. coef_exp_t.gt.zero) then
        call int_vol_scalar_diffuse_ele(fluid%istack_ele_fld_smp,       &
     &      node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs, &
     &      ifld_diff%i_temp, coef_exp_t, ak_d_temp, iphys%i_par_temp,  &
     &      fem_wk, f_l)
      end if
!
!  ----------  lead advection term
!
      if (iflag_temp_supg .gt. id_turn_OFF) then
        call int_vol_temp_ele_upw(node, ele, fluid, iphys, nod_fld,     &
     &      jac_3d, rhs_tbl, FEM_elens, diff_coefs,                     &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,         &
     &      ifld_diff%i_heat_flux, mhd_fem_wk, fem_wk, f_nl)
      else
        call int_vol_temp_ele(node, ele, fluid, iphys, nod_fld,         &
     &      jac_3d, rhs_tbl, FEM_elens, diff_coefs,                     &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,         &
     &      ifld_diff%i_heat_flux, mhd_fem_wk, fem_wk, f_nl)
      end if
!
!      call check_ff_smp(my_rank, n_scalar, node%max_nod_smp, f_l)
!      call check_ff_smp(my_rank, n_scalar, node%max_nod_smp, f_nl)
!
      call int_surf_temp_ele(ifld_diff%i_heat_flux, ak_d_temp,          &
     &    node, ele, surf, sf_grp, iphys, nod_fld, Tsf_bcs, jac_sf_grp, &
     &    rhs_tbl, FEM_elens, diff_coefs, fem_wk, surf_wk, f_l, f_nl)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_scalar, iphys%i_temp)
!      call check_nodal_data                                            &
!     &   ((50+my_rank), ele_fld, n_vector, iphys_ele%i_velo)
!      call check_ff_smp(my_rank, n_scalar, node%max_nod_smp, f_l)
!      call check_ff_smp(my_rank, n_scalar, node%max_nod_smp, f_nl)
!
      if (iflag_t_strat .gt. id_turn_OFF) then
        if (iflag_temp_supg .gt. id_turn_OFF) then
          call cal_stratified_layer_upw                                 &
     &       (node, ele, fluid, iphys, nod_fld,                         &
     &        ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,       &
     &        jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_nl)
        else
          call cal_stratified_layer(node, ele, fluid, iphys, nod_fld,   &
     &        ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,       &
     &        jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_nl)
        end if
      end if
!
!
      if (iflag_t_evo_4_temp .eq. id_explicit_euler) then
        call cal_scalar_pre_euler(iflag_temp_supg, iphys%i_par_temp,    &
     &      nod_comm, node, ele, fluid, iphys_ele, ele_fld, jac_3d,     &
     &      rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      else if (iflag_t_evo_4_temp .eq. id_explicit_adams2) then
        call cal_scalar_pre_adams                                       &
     &     (iflag_temp_supg, iphys%i_par_temp, iphys%i_pre_heat,        &
     &      nod_comm, node, ele, fluid, iphys_ele, ele_fld,  jac_3d,    &
     &      rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      else if (iflag_t_evo_4_temp .eq. id_Crank_nicolson) then
        call cal_temp_pre_lumped_crank(iphys%i_par_temp,                &
     &      iphys%i_pre_heat, ifld_diff%i_temp, ak_d_temp,              &
     &      nod_comm, node, ele, fluid, Tnod_bcs, iphys_ele, ele_fld,   &
     &      jac_3d, rhs_tbl, FEM_elens, diff_coefs, Tmatrix, MG_vector, &
     &      mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      else if (iflag_t_evo_4_temp .eq. id_Crank_nicolson_cmass) then 
        call cal_temp_pre_consist_crank(iphys%i_par_temp,               &
     &      iphys%i_pre_heat, ifld_diff%i_temp, ak_d_temp,              &
     &      node, ele, fluid, Tnod_bcs, jac_3d, rhs_tbl, FEM_elens,     &
     &      diff_coefs, Tmatrix, MG_vector, mhd_fem_wk, fem_wk,         &
     &      f_l, f_nl, nod_fld)
      end if
!
      call set_boundary_scalar                                          &
     &   (Tnod_bcs%nod_bc_s, iphys%i_par_temp, nod_fld)
!
      call scalar_send_recv(iphys%i_par_temp, nod_comm, nod_fld)
!
      call add_2_nod_scalars(node, nod_fld,                             &
     &    iphys%i_ref_t, iphys%i_par_temp, iphys%i_temp)
!
      end subroutine cal_parturbation_temp
!
! ----------------------------------------------------------------------
!
      end module cal_temperature
