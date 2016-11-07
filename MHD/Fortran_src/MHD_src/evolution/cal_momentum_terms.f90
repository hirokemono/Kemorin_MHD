!
!     module cal_momentum_terms
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine cal_terms_4_momentum                                 &
!!     &        (i_field, iak_diff_mf, iak_diff_lor,                    &
!!     &         nod_comm, node, ele, surf, fluid, sf_grp,              &
!!     &         Vsf_bcs, Bsf_bcs, iphys, iphys_ele, ak_MHD,            &
!!     &         jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,    &
!!     &         mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,                &
!!     &         nod_fld, ele_fld)
!!      subroutine cal_viscous_diffusion                                &
!!     &         (iak_diff_v, iak_diff_mf, iak_diff_lor,                &
!!     &          nod_comm, node, ele, surf, fluid, sf_grp,             &
!!     &          Vnod_bcs, Vsf_bcs, Bsf_bcs, iphys, ak_MHD,            &
!!     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,   &
!!     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!
      module cal_momentum_terms
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
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
      use t_material_property
      use t_bc_data_velo
      use t_surface_bc_data
!
      use cal_multi_pass
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
      use set_nodal_bc_id_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_terms_4_momentum                                   &
     &        (i_field, iak_diff_mf, iak_diff_lor,                      &
     &         nod_comm, node, ele, surf, fluid, sf_grp,                &
     &         Vsf_bcs, Bsf_bcs, iphys, iphys_ele, ak_MHD,              &
     &         jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,      &
     &         mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,                  &
     &         nod_fld, ele_fld)
!
      use int_vol_velo_monitor
      use int_surf_velo_pre
!
      integer (kind=kint), intent(in) :: i_field
      integer(kind= kint), intent(in) :: iak_diff_mf, iak_diff_lor
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      if (iflag_velo_supg .eq. id_turn_ON) then
        call int_vol_velo_monitor_upwind                                &
     &     (i_field, iak_diff_mf, iak_diff_lor, iphys_ele%i_velo,       &
     &      node, ele, fluid, iphys, nod_fld, iphys_ele, ak_MHD,        &
     &      jac_3d, rhs_tbl, FEM_elens, diff_coefs, mhd_fem_wk, fem_wk, &
     &      f_nl, ele_fld)
      else if (iflag_velo_supg .eq. id_magnetic_SUPG) then
        call int_vol_velo_monitor_upwind                                &
     &     (i_field, iak_diff_mf, iak_diff_lor, iphys_ele%i_magne,      &
     &      node, ele, fluid, iphys, nod_fld, iphys_ele, ak_MHD,        &
     &      jac_3d, rhs_tbl, FEM_elens, diff_coefs, mhd_fem_wk, fem_wk, &
     &      f_nl, ele_fld)
      else
       call int_vol_velo_monitor_pg(i_field, iak_diff_mf, iak_diff_lor, &
     &     node, ele, fluid, iphys, nod_fld, iphys_ele, ak_MHD,         &
     &     jac_3d, rhs_tbl, FEM_elens, diff_coefs, mhd_fem_wk, fem_wk,  &
     &     f_nl, ele_fld)
      end if
!
      call int_surf_velo_monitor                                        &
     &   (i_field, iak_diff_mf, iak_diff_lor, ak_MHD%ak_d_velo,         &
     &    node, ele, surf, sf_grp, Vsf_bcs, Bsf_bcs, iphys, nod_fld,    &
     &    jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,                   &
     &    fem_wk, surf_wk, f_l, f_nl)
!
      call cal_t_evo_4_vector(iflag_velo_supg,                          &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl, nod_comm,      &
     &    node, ele, iphys_ele, ele_fld, jac_3d, rhs_tbl,               &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!       call set_boundary_velo_4_rhs(node, Vnod_bcs, f_l, f_nl)
!
      call cal_ff_2_vector(node%numnod, node%istack_nod_smp,            &
     &    f_nl%ff, mhd_fem_wk%mlump_fl%ml, nod_fld%ntot_phys,           &
     &    i_field, nod_fld%d_fld)
      call vector_send_recv(i_field, nod_comm, nod_fld)
!
      end subroutine cal_terms_4_momentum
!
!-----------------------------------------------------------------------
!
      subroutine cal_viscous_diffusion                                  &
     &         (iak_diff_v, iak_diff_mf, iak_diff_lor,                  &
     &          nod_comm, node, ele, surf, fluid, sf_grp,               &
     &          Vnod_bcs, Vsf_bcs, Bsf_bcs, iphys, ak_MHD,              &
     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,     &
     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      use int_vol_diffusion_ele
      use int_surf_velo_pre
!
      integer (kind=kint), intent(in) :: iak_diff_v
      integer(kind= kint), intent(in) :: iak_diff_mf, iak_diff_lor
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
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
      call int_vol_vector_diffuse_ele(fluid%istack_ele_fld_smp,         &
     &    node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs,   &
     &    iak_diff_v, one, ak_MHD%ak_d_velo, iphys%i_velo, fem_wk, f_l)
!
      call int_surf_velo_monitor(iphys%i_v_diffuse,                     &
     &    iak_diff_mf, iak_diff_lor, ak_MHD%ak_d_velo,                  &
     &    node, ele, surf, sf_grp, Vsf_bcs, Bsf_bcs, iphys, nod_fld,    &
     &    jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,                   &
     &    fem_wk, surf_wk, f_l, f_nl)
!
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
!
      call set_boundary_velo_4_rhs(node, Vnod_bcs, f_l, f_nl)
!
      call cal_ff_2_vector(node%numnod, node%istack_nod_smp,            &
     &    f_l%ff, mhd_fem_wk%mlump_fl%ml, nod_fld%ntot_phys,            &
     &    iphys%i_v_diffuse, nod_fld%d_fld)
!
      call vector_send_recv(iphys%i_v_diffuse, nod_comm, nod_fld)
!
      end subroutine cal_viscous_diffusion
!
!-----------------------------------------------------------------------
!
      end module cal_momentum_terms
