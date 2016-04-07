!
!     module cal_magnetic_terms
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine cal_terms_4_magnetic(i_field, iak_diff_uxb,          &
!!     &         nod_comm, node, ele, surf, conduct, sf_grp,            &
!!     &         Bnod_bcs, Asf_bcs, Bsf_bcs, iphys, iphys_ele, ele_fld, &
!!     &         jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,    &
!!     &         mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_magnetic_diffusion(iak_diff_b, iak_diff_uxb,     &
!!     &          nod_comm, node, ele, surf, conduct, sf_grp,           &
!!     &          Bnod_bcs, Asf_bcs, Bsf_bcs, iphys,                    &
!!     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,   &
!!     &          m_lump, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
!!        type(velocity_surf_bc_type), intent(in) :: Asf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(MHD_coefficients_type), intent(in) :: diff_coefs
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_magnetic_terms
!
      use m_precision
!
      use m_phys_constants
      use m_control_parameter
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
      use t_bc_data_magne
      use t_surface_bc_data
      use t_material_property
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use cal_multi_pass
      use nod_phys_send_recv
      use int_surf_magne_pre
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_terms_4_magnetic(i_field, iak_diff_uxb,            &
     &         nod_comm, node, ele, surf, conduct, sf_grp,              &
     &         Bnod_bcs, Asf_bcs, Bsf_bcs, iphys, iphys_ele, ele_fld,   &
     &         jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,      &
     &         mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      use int_vol_magne_monitor
      use set_boundary_scalars
!
      integer (kind=kint), intent(in) :: i_field, iak_diff_uxb
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: conduct
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_coefficients_type), intent(in) :: diff_coefs
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      if (iflag_mag_supg .gt. id_turn_OFF) then
        call int_vol_magne_monitor_upm(i_field, iak_diff_uxb,           &
     &     node, ele, conduct, iphys, nod_fld, iphys_ele, ele_fld,      &
     &     jac_3d, rhs_tbl, FEM_elens, diff_coefs,                      &
     &     mhd_fem_wk, fem_wk, f_nl)
      else
        call int_vol_magne_monitor_pg(i_field, iak_diff_uxb,            &
     &     node, ele, conduct, iphys, nod_fld, iphys_ele, ele_fld,      &
     &     jac_3d, rhs_tbl, FEM_elens, diff_coefs,                      &
     &     mhd_fem_wk, fem_wk, f_nl)
      end if
!
      call int_surf_magne_monitor(i_field, iak_diff_uxb,                &
     &    node, ele, surf, sf_grp, Asf_bcs, Bsf_bcs, iphys, nod_fld,    &
     &    jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,                   &
     &    fem_wk, surf_wk, f_l, f_nl)
!
      call cal_t_evo_4_vector_cd(iflag_mag_supg,                        &
     &    conduct%istack_ele_fld_smp, mhd_fem_wk%mlump_cd,              &
     &    nod_comm, node, ele, iphys_ele, ele_fld, jac_3d,              &
     &    rhs_tbl, mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
      call delete_vector_ffs_on_bc(node, Bnod_bcs%nod_bc_b, f_l, f_nl)
!
      call cal_ff_2_vector(node%numnod, node%istack_nod_smp,            &
     &    f_nl%ff, mhd_fem_wk%mlump_cd%ml, nod_fld%ntot_phys,           &
     &    i_field, nod_fld%d_fld)
      call vector_send_recv(i_field, node, nod_comm, nod_fld)
!
      end subroutine cal_terms_4_magnetic
!
!-----------------------------------------------------------------------
!
      subroutine cal_magnetic_diffusion(iak_diff_b, iak_diff_uxb,       &
     &          nod_comm, node, ele, surf, conduct, sf_grp,             &
     &          Bnod_bcs, Asf_bcs, Bsf_bcs, iphys,                      &
     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,     &
     &          m_lump, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      use int_vol_diffusion_ele
      use set_boundary_scalars
!
      integer (kind=kint), intent(in) :: iak_diff_b, iak_diff_uxb
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: conduct
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      call int_vol_vector_diffuse_ele(conduct%istack_ele_fld_smp,       &
     &    node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens, diff_coefs,   &
     &    iak_diff_b, one, ak_d_magne, iphys%i_magne, fem_wk, f_l)
!
      call int_surf_magne_monitor(iphys%i_b_diffuse, iak_diff_uxb,      &
     &    node, ele, surf, sf_grp, Asf_bcs, Bsf_bcs, iphys, nod_fld,    &
     &    jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,                   &
     &    fem_wk, surf_wk, f_l, f_nl)
!
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
!
      call delete_vector_ffs_on_bc(node, Bnod_bcs%nod_bc_b, f_l, f_nl)
!
      call cal_ff_2_vector                                              &
     &   (node%numnod, node%istack_nod_smp, f_l%ff, m_lump%ml,          &
     &    nod_fld%ntot_phys, iphys%i_b_diffuse, nod_fld%d_fld)
      call vector_send_recv                                             &
     &   (iphys%i_b_diffuse, node, nod_comm, nod_fld)
!
      end subroutine cal_magnetic_diffusion
!
!-----------------------------------------------------------------------
!
      end module cal_magnetic_terms
