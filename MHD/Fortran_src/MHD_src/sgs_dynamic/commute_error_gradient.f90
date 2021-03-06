!commute_error_gradient.f90
!     module commute_error_gradient
!
!     Written by H. Matsui
!
!!      subroutine cal_grad_commute                                     &
!!     &         (num_int, iele_fsmp_stack, m_lump, node, ele, surf,    &
!!     &          sf_grp, g_FEM, jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,&
!!     &          sgs_sf, i_filter, i_sgs, i_scalar, fem_wk, surf_wk,   &
!!     &          f_l, f_nl, nod_fld)
!!      subroutine cal_rotation_commute                                 &
!!     &         (num_int, iele_fsmp_stack, m_lump, node, ele, surf,    &
!!     &          sf_grp, g_FEM, jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,&
!!     &          sgs_sf, i_filter, i_sgs, i_vect, fem_wk, surf_wk,     &
!!     &          f_l, f_nl, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(scaler_surf_bc_data_type), intent(in) :: sgs_sf
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data),    intent(inout) :: nod_fld
!!         i_filter: ID for filter function
!!         i_sgs: field ID for obtained difference term
!!         i_scalar: field ID for origianl scalar field
!
      module commute_error_gradient
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
      use t_fem_gauss_int_coefs
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
      subroutine cal_grad_commute                                       &
     &         (num_int, iele_fsmp_stack, m_lump, node, ele, surf,      &
     &          sf_grp, g_FEM, jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,  &
     &          sgs_sf, i_filter, i_sgs, i_scalar, fem_wk, surf_wk,     &
     &          f_l, f_nl, nod_fld)
!
      use int_surf_grad_sgs
      use int_vol_commute_error
      use cal_ff_smp_to_ffs
      use cal_for_ffs
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(scaler_surf_bc_data_type), intent(in) :: sgs_sf
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: i_filter, num_int
      integer(kind = kint), intent(in) :: i_sgs, i_scalar
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data),    intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node, f_l, f_nl)
!
      call int_vol_commute_grad(iele_fsmp_stack, num_int,               &
     &    node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,        &
     &    i_filter, i_scalar, fem_wk, f_nl)
!
      call int_surf_grad_commute_sgs(node, ele, surf, sf_grp,           &
     &    nod_fld, g_FEM, jac_sf_grp, rhs_tbl, FEM_elens, num_int,      &
     &    sgs_sf%ngrp_sf_dat, sgs_sf%id_grp_sf_dat, i_filter, i_scalar, &
     &    fem_wk, surf_wk, f_nl)
!
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
      call cal_ff_2_vector(node%numnod, node%istack_nod_smp,            &
     &    f_nl%ff, m_lump%ml, nod_fld%ntot_phys, i_sgs, nod_fld%d_fld)
!
      end subroutine cal_grad_commute
!
!-----------------------------------------------------------------------
!
      subroutine cal_rotation_commute                                   &
     &         (num_int, iele_fsmp_stack, m_lump, node, ele, surf,      &
     &          sf_grp, g_FEM, jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,  &
     &          sgs_sf, i_filter, i_sgs, i_vect, fem_wk, surf_wk,       &
     &          f_l, f_nl, nod_fld)
!
      use int_surf_rot_sgs
      use int_vol_commute_error
      use cal_ff_smp_to_ffs
      use cal_for_ffs
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: i_filter, num_int
      integer(kind = kint), intent(in) :: i_sgs, i_vect
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data),    intent(inout) :: nod_fld
!
!
!  cal commute error using rotation
!
      call reset_ff_smps(node, f_l, f_nl)
!
      call int_vol_commute_rot(iele_fsmp_stack, num_int,                &
     &    node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,        &
     &    i_filter, i_vect, fem_wk, f_nl)
!
      call int_surf_rot_commute_sgs(node, ele, surf, sf_grp, nod_fld,   &
     &    g_FEM, jac_sf_grp, rhs_tbl, FEM_elens, sgs_sf, num_int,       &
     &    i_filter, i_vect, fem_wk, surf_wk, f_nl)
!
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
      call cal_ff_2_vector(node%numnod, node%istack_nod_smp,            &
     &    f_nl%ff, m_lump%ml, nod_fld%ntot_phys, i_sgs, nod_fld%d_fld)
!
      end subroutine cal_rotation_commute
!
!-----------------------------------------------------------------------
!
      end module commute_error_gradient
