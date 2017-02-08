!
!     module cal_terms_for_heat
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine cal_terms_4_advect(i_field, i_scalar, iflag_supg,    &
!!     &          nod_comm, node, ele, fluid, property, Snod_bcs,       &
!!     &          iphys_ele, ele_fld, jac_3d, rhs_tbl,                  &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_div_of_scalar_flux(i_field, i_vector, iflag_supg,&
!!     &          nod_comm, node, ele, fluid, property, Snod_bcs,       &
!!     &          iphys_ele, ele_fld, jac_3d, rhs_tbl,                  &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!        type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
!!
!!      subroutine cal_terms_4_heat(i_field, ak_d_temp,                 &
!!     &          nod_comm, node, ele, surf, fluid, sf_grp, property,   &
!!     &          Tnod_bcs, Tsf_bcs, iphys, iphys_ele, ele_fld,         &
!!     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,   &
!!     &          ifld_diff, diff_coefs, mhd_fem_wk, fem_wk, surf_wk,   &
!!     &          f_l, f_nl, nod_fld)
!!      subroutine cal_thermal_diffusion                                &
!!     &         (iak_diff_hf, iak_diff_t, ak_d_temp,                   &
!!     &          nod_comm, node, ele, surf, fluid, sf_grp, property,   &
!!     &          Tnod_bcs, Tsf_bcs, iphys, jac_3d, jac_sf_grp,         &
!!     &          rhs_tbl, FEM_elens, diff_coefs,                       &
!!     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(scalar_property), intent(in) :: property
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(nodal_bcs_4_scalar_type), intent(in) :: Tnod_bcs
!!        type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_terms_for_heat
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
!
      use t_physical_property
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
      use t_bc_data_temp
      use t_surface_bc_data
      use t_material_property
!
      use cal_multi_pass
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
      use set_boundary_scalars
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_terms_4_advect(i_field, i_scalar, iflag_supg,      &
     &          nod_comm, node, ele, fluid, property, Snod_bcs,         &
     &          iphys_ele, ele_fld, jac_3d, rhs_tbl,                    &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use int_vol_inertia
!
      integer (kind=kint), intent(in) :: i_field, i_scalar
      integer (kind=kint), intent(in) :: iflag_supg
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(scalar_property), intent(in) :: property
      type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      if (iflag_supg .gt. id_turn_OFF) then
        call int_vol_scalar_inertia_upw                                 &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      fluid%istack_ele_fld_smp, intg_point_t_evo, i_scalar,       &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, iphys_ele%i_velo,      &
     &      ele_fld%d_fld, property%coef_nega_adv, fem_wk, f_nl)
      else
        call int_vol_scalar_inertia                                     &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      fluid%istack_ele_fld_smp, intg_point_t_evo, i_scalar,       &
     &      ele_fld%ntot_phys, iphys_ele%i_velo,                        &
     &      ele_fld%d_fld, property%coef_nega_adv, fem_wk, f_nl)
      end if
!
      call cal_t_evo_4_scalar(iflag_supg,                               &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl, nod_comm,      &
     &    node, ele, iphys_ele, ele_fld, jac_3d, rhs_tbl,               &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      call set_boundary_rhs_scalar(node, Snod_bcs%nod_bc_s, f_l, f_nl)
!
!       call check_ff(my_rank, n_scalar, node%numnod, f_nl)
!
      call cal_ff_2_scalar(node%numnod, node%istack_nod_smp,            &
     &    f_nl%ff, mhd_fem_wk%mlump_fl%ml,                              &
     &    nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
!   communication
!
      call scalar_send_recv(i_field, nod_comm, nod_fld)
!
      end subroutine cal_terms_4_advect
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_of_scalar_flux(i_field, i_vector, iflag_supg,  &
     &          nod_comm, node, ele, fluid, property, Snod_bcs,         &
     &          iphys_ele, ele_fld, jac_3d, rhs_tbl,                    &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use int_vol_vect_cst_difference
      use int_vol_vect_cst_diff_upw
!
      integer (kind=kint), intent(in) :: i_vector, i_field
      integer (kind=kint), intent(in) :: iflag_supg
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(scalar_property), intent(in) :: property
      type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      if (iflag_supg .gt. id_turn_OFF) then
        call int_vol_div_w_const                                        &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      fluid%istack_ele_fld_smp, intg_point_t_evo,                 &
     &      i_vector, property%coef_nega_adv, fem_wk, f_nl)
      else
        call int_vol_div_w_const_upw                                    &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      fluid%istack_ele_fld_smp, intg_point_t_evo,                 &
     &      i_vector, ele_fld%ntot_phys, iphys_ele%i_velo,              &
     &      ele_fld%d_fld, property%coef_nega_adv, fem_wk, f_nl)
      end if
!
      call cal_t_evo_4_scalar(iflag_temp_supg,                          &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl, nod_comm,      &
     &    node, ele, iphys_ele, ele_fld, jac_3d, rhs_tbl,               &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      call set_boundary_rhs_scalar(node, Snod_bcs%nod_bc_s, f_l, f_nl)
!
!       call check_ff(my_rank, n_scalar, node%numnod, f_nl)
!
      call cal_ff_2_scalar(node%numnod, node%istack_nod_smp,            &
     &    f_nl%ff, mhd_fem_wk%mlump_fl%ml,                              &
     &    nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
!   communication
!
      call scalar_send_recv(i_field, nod_comm, nod_fld)
!
      end subroutine cal_div_of_scalar_flux
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_terms_4_heat(i_field, ak_d_temp,                   &
     &          nod_comm, node, ele, surf, fluid, sf_grp, property,     &
     &          Tnod_bcs, Tsf_bcs, iphys, iphys_ele, ele_fld,           &
     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,                 &
     &          ifld_diff, diff_coefs, mhd_fem_wk, fem_wk, surf_wk,     &
     &          f_l, f_nl, nod_fld)
!
      use int_vol_temp_monitor
      use int_surf_div_fluxes_sgs
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(scalar_property), intent(in) :: property
      type(nodal_bcs_4_scalar_type), intent(in) :: Tnod_bcs
      type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      integer (kind=kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: ak_d_temp(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      if (iflag_temp_supg .gt. id_turn_OFF) then
       call int_vol_ene_monitor_upw(i_field, SGS_param1, cmt_param1,    &
     &     node, ele, fluid, property, iphys, nod_fld,                  &
     &     iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,              &
     &     ifld_diff, diff_coefs, mhd_fem_wk, fem_wk, f_nl)
      else
       call int_vol_ene_monitor(i_field, SGS_param1, cmt_param1,        &
     &     node, ele, fluid, property, iphys, nod_fld,                  &
     &     iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,              &
     &     ifld_diff, diff_coefs, mhd_fem_wk, fem_wk, f_nl)
      end if
!
      if(cmt_param1%iflag_c_temp .ne. id_SGS_commute_OFF                &
          .and. SGS_param1%iflag_SGS_h_flux .ne. id_SGS_none            &
          .and. i_field .eq. iphys%i_SGS_div_h_flux) then
        call int_sf_skv_sgs_div_v_flux(node, ele, surf, sf_grp,         &
     &      nod_fld, jac_sf_grp, rhs_tbl, FEM_elens, intg_point_t_evo,  &
     &      Tsf_bcs%sgs%ngrp_sf_dat, Tsf_bcs%sgs%id_grp_sf_dat,         &
     &      SGS_param1%ifilter_final, iphys%i_SGS_h_flux, iphys%i_velo, &
     &      iphys%i_temp, diff_coefs%num_field, ifld_diff%i_heat_flux,  &
     &      diff_coefs%ak, property%coef_advect, fem_wk, surf_wk, f_nl)
      end if
!
      call cal_t_evo_4_scalar(iflag_temp_supg,                          &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl, nod_comm,      &
     &    node, ele, iphys_ele, ele_fld, jac_3d, rhs_tbl,               &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      call set_boundary_rhs_scalar(node, Tnod_bcs%nod_bc_s, f_l, f_nl)
!
!       call check_ff(my_rank, n_scalar, node%numnod, f_nl)
!
      call cal_ff_2_scalar(node%numnod, node%istack_nod_smp,            &
     &    f_nl%ff, mhd_fem_wk%mlump_fl%ml,                              &
     &    nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
!   communication
!
      call scalar_send_recv(i_field, nod_comm, nod_fld)
!
      end subroutine cal_terms_4_heat
!
!-----------------------------------------------------------------------
!
      subroutine cal_thermal_diffusion                                  &
     &         (iak_diff_hf, iak_diff_t, ak_d_temp,                     &
     &          nod_comm, node, ele, surf, fluid, sf_grp, property,     &
     &          Tnod_bcs, Tsf_bcs, iphys, jac_3d, jac_sf_grp,           &
     &          rhs_tbl, FEM_elens, diff_coefs,                         &
     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      use int_vol_diffusion_ele
      use int_surf_fixed_gradients
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(phys_address), intent(in) :: iphys
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: fluid
      type(scalar_property), intent(in) :: property
      type(nodal_bcs_4_scalar_type), intent(in) :: Tnod_bcs
      type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!
      integer (kind=kint), intent(in) :: iak_diff_hf, iak_diff_t
      real(kind = kreal), intent(in) :: ak_d_temp(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      call int_vol_scalar_diffuse_ele(SGS_param1%ifilter_final,         &
     &    fluid%istack_ele_fld_smp, intg_point_t_evo,                   &
     &    node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs,   &
     &    iak_diff_t, one, ak_d_temp, iphys%i_temp, fem_wk, f_l)
!
      call int_sf_scalar_flux                                           &
     &   (node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl,                 &
     &    Tsf_bcs%flux, intg_point_t_evo, ak_d_temp, fem_wk, f_l)
!
      call set_ff_nl_smp_2_ff(n_scalar, node, rhs_tbl, f_l, f_nl)
!
      call set_boundary_rhs_scalar(node, Tnod_bcs%nod_bc_s, f_l, f_nl)
!
      call cal_ff_2_scalar(node%numnod, node%istack_nod_smp,            &
     &    f_l%ff, mhd_fem_wk%mlump_fl%ml,                               &
     &    nod_fld%ntot_phys, iphys%i_t_diffuse, nod_fld%d_fld)
!
!   communication
!
      call scalar_send_recv(iphys%i_t_diffuse, nod_comm, nod_fld)
!
      end subroutine cal_thermal_diffusion
!
!-----------------------------------------------------------------------
!
      end module cal_terms_for_heat
