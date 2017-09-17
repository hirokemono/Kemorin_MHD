!
!     module cal_induction_terms
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine cal_vecp_induction                                   &
!!     &         (dt, FEM_prm, nod_comm, node, ele, conduct, cd_prop,   &
!!     &          Bnod_bcs, iphys, iphys_ele, ele_fld, fem_int,         &
!!     &          mlump_cd, mhd_fem_wk, rhs_mat, nod_fld)
!!      subroutine cal_vecp_diffusion(iak_diff_b, ak_d_magne,           &
!!     &          FEM_prm, SGS_param, nod_comm, node, ele, surf, sf_grp,&
!!     &          Bnod_bcs, Asf_bcs,iphys, jac_3d, jac_sf_grp, rhs_tbl, &
!!     &          Bnod_bcs, Asf_bcs,iphys, fem_int, FEM_elens,          &
!!     &          diff_coefs, mlump_cd, rhs_mat, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
!!        type(velocity_surf_bc_type), intent(in) :: Asf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(lumped_mass_matrices), intent(in) :: mlump_cd
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_induction_terms
!
      use m_precision
!
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_physical_property
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use m_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_bc_data_magne
      use t_MHD_finite_element_mat
      use t_work_FEM_integration
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
      subroutine cal_vecp_induction                                     &
     &         (dt, FEM_prm, nod_comm, node, ele, conduct, cd_prop,     &
     &          Bnod_bcs, iphys, iphys_ele, ele_fld, fem_int,           &
     &          mlump_cd, mhd_fem_wk, rhs_mat, nod_fld)
!
!
      use int_vol_vect_p_pre
      use set_boundary_scalars
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(finite_element_integration), intent(in) :: fem_int
      type(lumped_mass_matrices), intent(in) :: mlump_cd
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, rhs_mat%f_l, rhs_mat%f_nl)
!
      if (FEM_prm%iflag_magne_supg .gt. id_turn_OFF) then
        call int_vol_vect_p_pre_ele_upm(FEM_prm%npoint_t_evo_int, dt,   &
     &      node, ele, conduct, cd_prop, iphys, nod_fld,                &
     &      ele_fld%ntot_phys, iphys_ele%i_magne, ele_fld%d_fld,        &
     &      g_FEM1, fem_int%jcs%jac_3d, fem_int%rhs_tbl, mhd_fem_wk,    &
     &      rhs_mat%fem_wk, rhs_mat%f_nl)
      else
        call int_vol_vect_p_pre_ele(FEM_prm%npoint_t_evo_int,           &
     &      node, ele, conduct, cd_prop, iphys, nod_fld,                &
     &      ele_fld%ntot_phys, iphys_ele%i_magne, ele_fld%d_fld,        &
     &      g_FEM1, fem_int%jcs%jac_3d, fem_int%rhs_tbl, mhd_fem_wk,    &
     &      rhs_mat%fem_wk, rhs_mat%f_nl)
      end if
!
      call cal_t_evo_4_vector_cd(FEM_prm%iflag_magne_supg,              &
     &    conduct%istack_ele_fld_smp, dt, FEM_prm,                      &
     &    mlump_cd, nod_comm, node, ele, iphys_ele, ele_fld, g_FEM1,    &
     &    fem_int%jcs%jac_3d, fem_int%rhs_tbl, mhd_fem_wk%ff_m_smp,     &
     &    rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl)
      call delete_vector_ffs_on_bc(node, Bnod_bcs%nod_bc_a,             &
     &    rhs_mat%f_l, rhs_mat%f_nl)
!
      call cal_ff_2_vector(node%numnod, node%istack_nod_smp,            &
     &    rhs_mat%f_nl%ff, mlump_cd%ml, nod_fld%ntot_phys,              &
     &    iphys%i_vp_induct, nod_fld%d_fld)
      call vector_send_recv(iphys%i_vp_induct, nod_comm, nod_fld)
!
      end subroutine cal_vecp_induction
!
!-----------------------------------------------------------------------
!
      subroutine cal_vecp_diffusion(iak_diff_b, ak_d_magne,             &
     &          FEM_prm, SGS_param, nod_comm, node, ele, surf, sf_grp,  &
     &          Bnod_bcs, Asf_bcs,iphys, fem_int, FEM_elens,            &
     &          diff_coefs, mlump_cd, rhs_mat, nod_fld)
!
      use t_SGS_control_parameter
      use t_surface_bc_data
!
      use int_vol_diffusion_ele
      use int_surf_fixed_gradients
      use set_boundary_scalars
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(phys_address), intent(in) :: iphys
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_cd
!
      integer(kind=kint), intent(in) :: iak_diff_b
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, rhs_mat%f_l, rhs_mat%f_nl)
!
      call int_vol_vector_diffuse_ele                                   &
     &   (SGS_param%ifilter_final, ele%istack_ele_smp,                  &
     &    FEM_prm%npoint_t_evo_int,  node, ele, nod_fld,                &
     &    g_FEM1, fem_int%jcs%jac_3d, fem_int%rhs_tbl,                  &
     &    FEM_elens, diff_coefs, iak_diff_b, one, ak_d_magne,           &
     &    iphys%i_vecp, rhs_mat%fem_wk, rhs_mat%f_l)
!
      call int_sf_grad_velocity(node, ele, surf, sf_grp,                &
     &    g_FEM1, fem_int%jcs%jac_sf_grp, fem_int%rhs_tbl,              &
     &    Asf_bcs%grad, FEM_prm%npoint_t_evo_int, ak_d_magne,           &
     &    rhs_mat%fem_wk, rhs_mat%f_l)
!
      call set_ff_nl_smp_2_ff(n_vector, node, fem_int%rhs_tbl,          &
     &    rhs_mat%f_l, rhs_mat%f_nl)
!
      call delete_vector_ffs_on_bc(node, Bnod_bcs%nod_bc_a,             &
     &    rhs_mat%f_l, rhs_mat%f_nl)
!
      call cal_ff_2_vector(node%numnod, node%istack_nod_smp,            &
     &    rhs_mat%f_l%ff, mlump_cd%ml, nod_fld%ntot_phys,               &
     &    iphys%i_vp_diffuse, nod_fld%d_fld)
!
      call vector_send_recv(iphys%i_vp_diffuse, nod_comm, nod_fld)
!
      end subroutine cal_vecp_diffusion
!
!-----------------------------------------------------------------------
!
      end module cal_induction_terms
