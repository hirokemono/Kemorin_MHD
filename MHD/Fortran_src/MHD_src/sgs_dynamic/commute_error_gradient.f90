!commute_error_gradient.f90
!     module commute_error_gradient
!
!     Written by H. Matsui
!
!      subroutine cal_grad_commute(ngrp_sf, id_grp_sf, i_filter,        &
!     &          i_sgs, i_scalar)
!      subroutine cal_grad_commute_fluid(ngrp_sf, id_grp_sf,            &
!     &          i_filter, i_sgs, i_scalar)
!      subroutine cal_grad_commute_conduct(ngrp_sf, id_grp_sf,          &
!     &          i_filter, i_sgs, i_scalar)
!         i_filter: ID for filter function
!         i_sgs: field ID for obtained difference term
!         i_scalar: field ID for origianl scalar field
!
      module commute_error_gradient
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
      use m_group_data
      use m_node_phys_data
      use m_finite_element_matrix
      use m_int_vol_data
!
      use int_vol_commute_1st
      use int_surf_grad_sgs
      use cal_ff_smp_to_ffs
      use cal_for_ffs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_grad_commute(ngrp_sf, id_grp_sf, i_filter,         &
     &          i_sgs, i_scalar)
!
      use m_geometry_data
      use m_jacobian_sf_grp
!
      integer(kind = kint), intent(in) :: ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint), intent(in) :: i_sgs, i_scalar
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_commute_grad(ele1%istack_ele_smp, intg_point_t_evo,  &
     &    i_filter, i_scalar)
!
      call int_surf_grad_commute_sgs(node1, ele1, surf1, sf_grp1,       &
     &    nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,              &
     &    intg_point_t_evo, ngrp_sf, id_grp_sf, i_filter, i_scalar,     &
     &    fem1_wk, f1_nl)
!
      call set_ff_nl_smp_2_ff(node1, rhs_tbl1, n_vector)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, m1_lump%ml, nod_fld1%ntot_phys,                     &
     &    i_sgs, nod_fld1%d_fld)
!
      end subroutine cal_grad_commute
!
!-----------------------------------------------------------------------
!
      subroutine cal_grad_commute_fluid(ngrp_sf, id_grp_sf,             &
     &          i_filter, i_sgs, i_scalar)
!
      use m_geometry_data
      use m_jacobian_sf_grp
      use m_geometry_data_MHD
!
      integer(kind = kint), intent(in) :: ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint), intent(in) :: i_sgs, i_scalar
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_commute_grad(iele_fl_smp_stack, intg_point_t_evo,    &
     &    i_filter, i_scalar)
!
      call int_surf_grad_commute_sgs(node1, ele1, surf1, sf_grp1,       &
     &    nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,              &
     &    intg_point_t_evo, ngrp_sf, id_grp_sf, i_filter, i_scalar,     &
     &    fem1_wk, f1_nl)
!
      call set_ff_nl_smp_2_ff(node1, rhs_tbl1, n_vector)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, mhd_fem1_wk%mlump_fl%ml, nod_fld1%ntot_phys,        &
     &    i_sgs, nod_fld1%d_fld)
!
      end subroutine cal_grad_commute_fluid
!
!-----------------------------------------------------------------------
!
      subroutine cal_grad_commute_conduct(ngrp_sf, id_grp_sf,           &
     &          i_filter, i_sgs, i_scalar)
!
      use m_geometry_data
      use m_jacobian_sf_grp
      use m_geometry_data_MHD
!
      integer(kind = kint), intent(in) :: ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint), intent(in) :: i_sgs, i_scalar
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_commute_grad(iele_cd_smp_stack, intg_point_t_evo,    &
     &    i_filter, i_scalar)
!
      call int_surf_grad_commute_sgs(node1, ele1, surf1, sf_grp1,       &
     &    nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,              &
     &    intg_point_t_evo, ngrp_sf, id_grp_sf, i_filter, i_scalar,     &
     &    fem1_wk, f1_nl)
!
      call set_ff_nl_smp_2_ff(node1, rhs_tbl1, n_vector)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, mhd_fem1_wk%mlump_cd%ml, nod_fld1%ntot_phys,        &
     &    i_sgs, nod_fld1%d_fld)
!
      end subroutine cal_grad_commute_conduct
!
!-----------------------------------------------------------------------
!
      end module commute_error_gradient
