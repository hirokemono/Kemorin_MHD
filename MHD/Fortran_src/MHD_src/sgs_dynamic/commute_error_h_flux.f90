!commute_error_h_flux.f90
!     module commute_error_h_flux
!
!     Written by H. Matsui
!
!!      subroutine cal_commute_error_4_sf                               &
!!     &         (num_int, iele_fsmp_stack, m_lump,                     &
!!     &          node, ele, surf, sf_grp, jac_3d, jac_sf_grp, rhs_tbl, &
!!     &          FEM_elens, sgs_sf, i_filter, i_sgs, i_flux, i_vect,   &
!!     &          i_scalar, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_commute_error_4_mf                               &
!!     &         (num_int, iele_fsmp_stack, m_lump,                     &
!!     &          node, ele, surf, sf_grp, jac_3d, jac_sf_grp, rhs_tbl, &
!!     &          FEM_elens, sgs_sf, i_filter, i_sgs, i_flux, i_vect,   &
!!     &          fem_wk, surf_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_commute_error_4_idct                             &
!!     &         (num_int, iele_fsmp_stack, m_lump,                     &
!!     &          node, ele, surf, sf_grp, Bsf_bcs, jac_3d, jac_sf_grp, &
!!     &          rhs_tbl, FEM_elens, i_filter, i_sgs, i_flux, i_v, i_b,&
!!     &          fem_wk, surf_wk, f_l, f_nl, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!         i_filter: ID for filter function
!!         i_sgs: field ID for obtained difference term
!!         i_flux: field ID for SGS term
!!         i_vect: field ID for origianl vector field
!!         i_scalar: field ID for origianl scalar field
!
      module commute_error_h_flux
!
      use m_precision
      use m_machine_parameter
!
      use m_phys_constants
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_jacobian_2d
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_surface_bc_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_4_sf                                 &
     &         (num_int, iele_fsmp_stack, m_lump,                       &
     &          node, ele, surf, sf_grp, jac_3d, jac_sf_grp, rhs_tbl,   &
     &          FEM_elens, sgs_sf, i_filter, i_sgs, i_flux, i_vect,     &
     &          i_scalar, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      use int_vol_commute_error
      use int_surf_div_fluxes_sgs
      use cal_ff_smp_to_ffs
      use cal_for_ffs
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: i_flux, i_vect, i_scalar
      integer(kind = kint), intent(in) :: i_sgs, i_filter
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      call int_vol_commute_div_v_flux(iele_fsmp_stack, num_int,         &
     &    node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,               &
     &    i_filter, i_flux, i_vect, i_scalar, fem_wk, f_nl)
      call int_sf_skv_commute_sgs_v_flux(node, ele, surf, sf_grp,       &
     &    nod_fld, jac_sf_grp, rhs_tbl, FEM_elens, num_int, sgs_sf,     &
     &    i_filter, i_flux, i_vect, i_scalar, fem_wk, surf_wk, f_nl)
!
      call set_ff_nl_smp_2_ff(n_scalar, node, rhs_tbl, f_l, f_nl)
      call cal_ff_2_scalar(node%numnod, node%istack_nod_smp,            &
     &    f_nl%ff, m_lump%ml, nod_fld%ntot_phys, i_sgs, nod_fld%d_fld)
!
      end subroutine cal_commute_error_4_sf
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_4_mf                                 &
     &         (num_int, iele_fsmp_stack, m_lump,                       &
     &          node, ele, surf, sf_grp, jac_3d, jac_sf_grp, rhs_tbl,   &
     &          FEM_elens, sgs_sf, i_filter, i_sgs, i_flux, i_vect,     &
     &          fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      use t_surface_bc_data
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use int_vol_commute_error
      use int_surf_div_fluxes_sgs
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: i_flux, i_vect
      integer(kind = kint), intent(in) :: i_sgs, i_filter
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      call int_vol_commute_div_m_flux(iele_fsmp_stack, num_int,         &
     &    node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,               &
     &    i_filter, i_flux, i_vect, fem_wk, f_nl)
!
      call int_sf_skv_commute_sgs_t_flux(node, ele, surf, sf_grp,       &
     &    nod_fld, jac_sf_grp, rhs_tbl, FEM_elens, sgs_sf, num_int,     &
     &    i_filter, i_flux, i_vect, i_vect, fem_wk, surf_wk, f_nl)
!
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
      call cal_ff_2_vector(node%numnod, node%istack_nod_smp,            &
     &    f_nl%ff, m_lump%ml, nod_fld%ntot_phys, i_sgs, nod_fld%d_fld)
!
      end subroutine cal_commute_error_4_mf
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_4_idct                               &
     &         (num_int, iele_fsmp_stack, m_lump,                       &
     &          node, ele, surf, sf_grp, Bsf_bcs, jac_3d, jac_sf_grp,   &
     &          rhs_tbl, FEM_elens, i_filter, i_sgs, i_flux, i_v, i_b,  &
     &          fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      use t_surface_bc_data
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use int_vol_commute_error
      use int_surf_div_induct_tsr_sgs
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: i_flux, i_v, i_b
      integer(kind = kint), intent(in) :: i_sgs, i_filter
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      call int_vol_commute_induct_t(iele_fsmp_stack, num_int,           &
     &    node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,               &
     &    i_filter, i_flux, i_v, i_b, fem_wk, f_nl)
!
      call int_surf_commute_induct_t(node, ele, surf, sf_grp,           &
     &     nod_fld, jac_sf_grp, rhs_tbl, FEM_elens, Bsf_bcs%sgs,        &
     &     num_int, i_flux, i_filter, i_v, i_b, fem_wk, surf_wk, f_nl)
!
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
      call cal_ff_2_vector(node%numnod, node%istack_nod_smp,            &
     &    f_nl%ff, m_lump%ml, nod_fld%ntot_phys, i_sgs, nod_fld%d_fld)
!
      end subroutine cal_commute_error_4_idct
!
!-----------------------------------------------------------------------
!
      end module commute_error_h_flux
