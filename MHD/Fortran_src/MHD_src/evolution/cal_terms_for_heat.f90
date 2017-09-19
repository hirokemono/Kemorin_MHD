!
!     module cal_terms_for_heat
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine cal_terms_4_advect                                   &
!!     &         (i_field, i_scalar, iflag_supg, num_int, dt,           &
!!     &          FEM_prm, nod_comm, node, ele, fluid, property,        &
!!     &          Snod_bcs, iphys_ele, ele_fld, fem_int, mlump_fl,      &
!!     &          mhd_fem_wk, rhs_mat, nod_fld)
!!      subroutine cal_div_of_scalar_flux                               &
!!     &         (i_field, i_vector, iflag_supg, num_int, dt,           &
!!     &          FEM_prm, nod_comm, node, ele, fluid, property,        &
!!     &          Snod_bcs,  iphys_ele, ele_fld, fem_int, mlump_fl,     &
!!     &          mhd_fem_wk, rhs_mat, nod_fld)
!!        type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
!!
!!      subroutine cal_thermal_diffusion                                &
!!     &         (i_field, i_scalar, iak_diffuse, ak_diffuse, num_int,  &
!!     &          SGS_param, nod_comm, node, ele, surf, fluid, sf_grp,  &
!!     &          Snod_bcs, Ssf_bcs, fem_int, FEM_elens, diff_coefs,    &
!!     &          mlump_fl, rhs_mat, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(scalar_property), intent(in) :: property
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
!!        type(scaler_surf_bc_type), intent(in) :: Ssf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_terms_for_heat
!
      use m_precision
!
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
      use t_bc_data_temp
      use t_surface_bc_data
      use t_material_property
      use t_MHD_finite_element_mat
      use t_work_FEM_integration
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
      subroutine cal_terms_4_advect                                     &
     &         (i_field, i_scalar, iflag_supg, num_int, dt,             &
     &          FEM_prm, nod_comm, node, ele, fluid, property,          &
     &          Snod_bcs, iphys_ele, ele_fld, fem_int, mlump_fl,        &
     &          mhd_fem_wk, rhs_mat, nod_fld)
!
      use int_vol_inertia
!
      integer (kind=kint), intent(in) :: i_field, i_scalar
      integer (kind=kint), intent(in) :: iflag_supg, num_int
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(scalar_property), intent(in) :: property
      type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(finite_element_integration), intent(in) :: fem_int
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, rhs_mat%f_l, rhs_mat%f_nl)
!
      if (iflag_supg .gt. id_turn_OFF) then
        call int_vol_scalar_inertia_upw(node, ele,                      &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,     &
     &      nod_fld, fluid%istack_ele_fld_smp, num_int, dt, i_scalar,   &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, iphys_ele%i_velo,      &
     &      ele_fld%d_fld, property%coef_nega_adv,                      &
     &      rhs_mat%fem_wk, rhs_mat%f_nl)
      else
        call int_vol_scalar_inertia (node, ele,                         &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,     &
     &      nod_fld, fluid%istack_ele_fld_smp, num_int, i_scalar,       &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,         &
     &      property%coef_nega_adv, rhs_mat%fem_wk, rhs_mat%f_nl)
      end if
!
      call cal_t_evo_4_scalar(iflag_supg, fluid%istack_ele_fld_smp, dt, &
     &    FEM_prm, mlump_fl, nod_comm, node, ele, iphys_ele, ele_fld,   &
     &    fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,       &
     &    mhd_fem_wk%ff_m_smp, rhs_mat%fem_wk,                          &
     &    rhs_mat%f_l, rhs_mat%f_nl)
!
      call set_boundary_rhs_scalar                                      &
     &   (node, Snod_bcs%nod_bc_s, rhs_mat%f_l, rhs_mat%f_nl)
!
!       call check_ff(my_rank, n_scalar, node%numnod, rhs_mat%f_nl)
!
      call cal_ff_2_scalar                                              &
     &   (node%numnod, node%istack_nod_smp, rhs_mat%f_nl%ff,            &
     &    mlump_fl%ml, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
!   communication
!
      call scalar_send_recv(i_field, nod_comm, nod_fld)
!
      end subroutine cal_terms_4_advect
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_of_scalar_flux                                 &
     &         (i_field, i_vector, iflag_supg, num_int, dt,             &
     &          FEM_prm, nod_comm, node, ele, fluid, property,          &
     &          Snod_bcs,  iphys_ele, ele_fld, fem_int, mlump_fl,       &
     &          mhd_fem_wk, rhs_mat, nod_fld)
!
      use int_vol_vect_cst_difference
      use int_vol_vect_cst_diff_upw
!
      integer (kind=kint), intent(in) :: i_vector, i_field
      integer (kind=kint), intent(in) :: iflag_supg, num_int
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(scalar_property), intent(in) :: property
      type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(finite_element_integration), intent(in) :: fem_int
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, rhs_mat%f_l, rhs_mat%f_nl)
!
      if (iflag_supg .gt. id_turn_OFF) then
        call int_vol_div_w_const(node, ele,                             &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,     &
     &      nod_fld, fluid%istack_ele_fld_smp, num_int, i_vector,       &
     &      property%coef_nega_adv, rhs_mat%fem_wk, rhs_mat%f_nl)
      else
        call int_vol_div_w_const_upw(node, ele,                         &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,     &
     &      nod_fld, fluid%istack_ele_fld_smp, num_int, dt,             &
     &      i_vector, ele_fld%ntot_phys, iphys_ele%i_velo,              &
     &      ele_fld%d_fld, property%coef_nega_adv,                      &
     &      rhs_mat%fem_wk, rhs_mat%f_nl)
      end if
!
      call cal_t_evo_4_scalar                                           &
     &   (iflag_supg, fluid%istack_ele_fld_smp, dt, FEM_prm,            &
     &    mlump_fl, nod_comm, node, ele, iphys_ele, ele_fld,            &
     &    fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,       &
     &    mhd_fem_wk%ff_m_smp, rhs_mat%fem_wk, rhs_mat%f_l,             &
     &    rhs_mat%f_nl)
!
      call set_boundary_rhs_scalar                                      &
     &   (node, Snod_bcs%nod_bc_s, rhs_mat%f_l, rhs_mat%f_nl)
!
!       call check_ff(my_rank, n_scalar, node%numnod, rhs_mat%f_nl)
!
      call cal_ff_2_scalar                                              &
     &   (node%numnod, node%istack_nod_smp, rhs_mat%f_nl%ff,            &
     &    mlump_fl%ml, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
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
      subroutine cal_thermal_diffusion                                  &
     &         (i_field, i_scalar, iak_diffuse, ak_diffuse, num_int,    &
     &          SGS_param, nod_comm, node, ele, surf, fluid, sf_grp,    &
     &          Snod_bcs, Ssf_bcs, fem_int, FEM_elens, diff_coefs,      &
     &          mlump_fl, rhs_mat, nod_fld)
!
      use int_vol_diffusion_ele
      use int_surf_fixed_gradients
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: fluid
      type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
      type(scaler_surf_bc_type), intent(in) :: Ssf_bcs
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      integer (kind=kint), intent(in) :: i_field, i_scalar
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: iak_diffuse
      real(kind = kreal), intent(in) :: ak_diffuse(ele%numele)
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, rhs_mat%f_l, rhs_mat%f_nl)
!
      call int_vol_scalar_diffuse_ele(SGS_param%ifilter_final,          &
     &    fluid%istack_ele_fld_smp, num_int, node, ele, nod_fld,        &
     &    fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,       &
     &    FEM_elens, diff_coefs, iak_diffuse, one, ak_diffuse,          &
     &    i_scalar, rhs_mat%fem_wk, rhs_mat%f_l)
!
      call int_sf_scalar_flux(node, ele, surf, sf_grp,                  &
     &    fem_int%jcs%g_FEM, fem_int%jcs%jac_sf_grp, fem_int%rhs_tbl,   &
     &    Ssf_bcs%flux, num_int, ak_diffuse,                            &
     &    rhs_mat%fem_wk, rhs_mat%f_l)
!
      call set_ff_nl_smp_2_ff                                           &
     &   (n_scalar, node, fem_int%rhs_tbl, rhs_mat%f_l, rhs_mat%f_nl)
!
      call set_boundary_rhs_scalar                                      &
     &   (node, Snod_bcs%nod_bc_s, rhs_mat%f_l, rhs_mat%f_nl)
!
      call cal_ff_2_scalar                                              &
     &   (node%numnod, node%istack_nod_smp, rhs_mat%f_l%ff,             &
     &    mlump_fl%ml, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
!   communication
!
      call scalar_send_recv(i_field, nod_comm, nod_fld)
!
      end subroutine cal_thermal_diffusion
!
!-----------------------------------------------------------------------
!
      end module cal_terms_for_heat
