!
!     module cal_terms_for_heat
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine cal_terms_4_heat(i_field, iak_diff_hf,               &
!!     &          nod_comm, node, ele, surf, fluid, sf_grp,             &
!!     &          Tnod_bcs, Tsf_bcs, iphys, iphys_ele, ele_fld,         &
!!     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,   &
!!     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_thermal_diffusion(iak_diff_hf, iak_diff_t,       &
!!     &          nod_comm, node, ele, surf, fluid, sf_grp,             &
!!     &          Tnod_bcs, Tsf_bcs, iphys, jac_3d, jac_sf_grp,         &
!!     &          rhs_tbl, FEM_elens, diff_coefs,                       &
!!     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
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
!!        type(MHD_coefficients_type), intent(in) :: diff_coefs
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
      use int_surf_temp
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_terms_4_heat(i_field, iak_diff_hf,                 &
     &          nod_comm, node, ele, surf, fluid, sf_grp,               &
     &          Tnod_bcs, Tsf_bcs, iphys, iphys_ele, ele_fld,           &
     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,     &
     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      use int_vol_temp_monitor
!
      integer (kind=kint), intent(in) :: i_field, iak_diff_hf
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
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
      type(MHD_coefficients_type), intent(in) :: diff_coefs
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
       call int_vol_ene_monitor_upw(i_field, iak_diff_hf,               &
     &     node, ele, fluid, iphys, nod_fld, iphys_ele, ele_fld,        &
     &     jac_3d, rhs_tbl, FEM_elens, diff_coefs,                      &
     &     mhd_fem_wk, fem_wk, f_nl)
      else
       call int_vol_ene_monitor(i_field, iak_diff_hf,                   &
     &     node, ele, fluid, iphys, nod_fld, iphys_ele, ele_fld,        &
     &     jac_3d, rhs_tbl, FEM_elens, diff_coefs,                      &
     &     mhd_fem_wk, fem_wk, f_nl)
      end if
!
      call int_surf_temp_monitor(i_field, iak_diff_hf,                  &
     &    node, ele, surf, sf_grp, iphys, nod_fld, Tsf_bcs, jac_sf_grp, &
     &    rhs_tbl, FEM_elens, diff_coefs, fem_wk, surf_wk, f_l, f_nl)
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
      call scalar_send_recv(i_field, node, nod_comm, nod_fld)
!
      end subroutine cal_terms_4_heat
!
!-----------------------------------------------------------------------
!
      subroutine cal_thermal_diffusion(iak_diff_hf, iak_diff_t,         &
     &          nod_comm, node, ele, surf, fluid, sf_grp,               &
     &          Tnod_bcs, Tsf_bcs, iphys, jac_3d, jac_sf_grp,           &
     &          rhs_tbl, FEM_elens, diff_coefs,                         &
     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      use m_ele_material_property
      use int_vol_diffusion_ele
!
      integer (kind=kint), intent(in) :: iak_diff_hf, iak_diff_t
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(phys_address), intent(in) :: iphys
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: fluid
      type(nodal_bcs_4_scalar_type), intent(in) :: Tnod_bcs
      type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_coefficients_type), intent(in) :: diff_coefs
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      call int_vol_scalar_diffuse_ele(fluid%istack_ele_fld_smp,         &
     &    node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs,   &
     &    iak_diff_t, one, ak_d_temp, iphys%i_temp, fem_wk, f_l)
!
      call int_surf_temp_monitor(iphys%i_t_diffuse, iak_diff_hf,        &
     &    node, ele, surf, sf_grp, iphys, nod_fld, Tsf_bcs, jac_sf_grp, &
     &    rhs_tbl, FEM_elens, diff_coefs, fem_wk, surf_wk, f_l, f_nl)
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
      call scalar_send_recv                                             &
     &   (iphys%i_t_diffuse, node, nod_comm, nod_fld)
!
      end subroutine cal_thermal_diffusion
!
!-----------------------------------------------------------------------
!
      end module cal_terms_for_heat
