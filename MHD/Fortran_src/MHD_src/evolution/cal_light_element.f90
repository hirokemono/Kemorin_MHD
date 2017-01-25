!
!      module cal_light_element
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!!      subroutine s_cal_light_element(nod_comm, node, ele, surf,       &
!!     &          fluid, sf_grp, nod_bcs, sf_bcs, iphys,                &
!!     &          iphys_ele, ele_fld, jac_3d, jac_sf_grp, rhs_tbl,      &
!!     &          FEM_elens, ifld_diff, diff_coefs, Cmatrix,            &
!!     &          ak_d_composit, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(scalar_property), intent(in) :: property
!!        type(nodal_bcs_4_scalar_type), intent(in) :: nod_bcs
!!        type(scaler_surf_bc_type), intent(in) :: sf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(MHD_MG_matrix), intent(in) :: Cmatrix
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_light_element
!
      use m_precision
!
      use calypso_mpi
      use m_control_parameter
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
      use t_bc_data_temp
      use t_surface_bc_data
      use t_material_property
      use t_SGS_model_coefs
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
      subroutine s_cal_light_element(nod_comm, node, ele, surf,         &
     &          fluid, sf_grp, property, nod_bcs, sf_bcs, iphys,        &
     &          iphys_ele, ele_fld, jac_3d, jac_sf_grp, rhs_tbl,        &
     &          FEM_elens, ifld_diff, diff_coefs, Cmatrix,              &
     &          ak_d_composit, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_t_int_parameter
      use m_type_AMG_data
!
      use nod_phys_send_recv
      use set_boundary_scalars
      use int_surf_fixed_gradients
      use int_vol_diffusion_ele
      use int_vol_light_comp_ele
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
      type(scalar_property), intent(in) :: property
      type(nodal_bcs_4_scalar_type), intent(in) :: nod_bcs
      type(scaler_surf_bc_type), intent(in) :: sf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(MHD_MG_matrix), intent(in) :: Cmatrix
!
      real(kind = kreal), intent(in) :: ak_d_composit(ele%numele)
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      if (property%coef_advect .gt. zero                                &
     &     .and. evo_comp%coef_exp.gt.zero) then
        call int_vol_scalar_diffuse_ele(fluid%istack_ele_fld_smp,       &
     &      node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs, &
     &      ifld_diff%i_light, evo_comp%coef_exp, ak_d_composit,        &
     &      iphys%i_light, fem_wk, f_l)
      end if
!
      if (iflag_comp_supg .gt. id_turn_OFF) then
        call int_vol_composition_ele_upw(node, ele, fluid,              &
     &     property, iphys, nod_fld, jac_3d, rhs_tbl,                   &
     &     ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,          &
     &     fem_wk, f_nl)
      else
        call int_vol_composition_ele(node, ele, fluid,                  &
     &      property, iphys, nod_fld, jac_3d, rhs_tbl,                  &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,         &
     &      fem_wk, f_nl)
      end if
!
!
      call int_sf_h_flux(node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl,  &
     &    sf_bcs%flux, intg_point_t_evo, ak_d_composit, fem_wk, f_l)
!
!
      if     (evo_comp%iflag_scheme .eq. id_explicit_euler) then
        call cal_scalar_pre_euler(iflag_comp_supg, iphys%i_light,       &
     &      nod_comm, node, ele, fluid, iphys_ele, ele_fld, jac_3d,     &
     &      rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      else if(evo_comp%iflag_scheme .eq. id_explicit_adams2) then
        call cal_scalar_pre_adams                                       &
     &    (iflag_comp_supg, iphys%i_light, iphys%i_pre_composit,        &
     &      nod_comm, node, ele, fluid, iphys_ele, ele_fld, jac_3d,     &
     &      rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      else if(evo_comp%iflag_scheme .eq. id_Crank_nicolson) then
        call cal_composit_pre_lumped_crank(iphys%i_light,               &
     &      iphys%i_pre_composit, ifld_diff%i_light, ak_d_composit,     &
     &      nod_comm, node, ele, fluid, nod_bcs,                        &
     &      iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs, &
     &      Cmatrix, MG_vector, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      else if(evo_comp%iflag_scheme .eq. id_Crank_nicolson_cmass) then
        call cal_composit_pre_consist_crank(iphys%i_light,              &
     &      iphys%i_pre_composit, ifld_diff%i_light, ak_d_composit,     &
     &      node, ele, fluid, evo_comp, property, nod_bcs,              &
     &      jac_3d, rhs_tbl, FEM_elens, diff_coefs, Cmatrix, MG_vector, &
     &      mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      call set_boundary_scalar                                          &
     &   (nod_bcs%nod_bc_s, iphys%i_light, nod_fld)
!
      call scalar_send_recv(iphys%i_light, nod_comm, nod_fld)
!
      end subroutine s_cal_light_element
!
! ----------------------------------------------------------------------
!
      end module cal_light_element
