!commute_error_gradient.f90
!     module commute_error_gradient
!
!     Written by H. Matsui
!
!!      subroutine cal_grad_commute(iele_fsmp_stack, m_lump,            &
!!     &          ngrp_sf, id_grp_sf, i_filter, i_sgs, i_scalar)
!!      subroutine cal_rotation_commute(iele_fsmp_stack, m_lump,        &
!!     &          sgs_sf, i_filter, i_sgs, i_vect)
!         i_filter: ID for filter function
!         i_sgs: field ID for obtained difference term
!         i_scalar: field ID for origianl scalar field
!
      module commute_error_gradient
!
      use m_precision
      use m_machine_parameter
!
      use m_control_parameter
      use m_geometry_data
      use m_group_data
      use m_phys_constants
      use m_node_phys_data
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
      use m_filter_elength
!
      use t_surface_bc_data
      use t_finite_element_mat
!
      use int_vol_commute_error
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
      subroutine cal_grad_commute(iele_fsmp_stack, m_lump,              &
     &          sgs_sf, i_filter, i_sgs, i_scalar)
!
      use m_geometry_data
      use m_jacobian_sf_grp
!
      use int_surf_grad_sgs
!
      type(scaler_surf_bc_data_type), intent(in) :: sgs_sf
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint), intent(in) :: i_sgs, i_scalar
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_commute_grad(iele_fsmp_stack, intg_point_t_evo,      &
     &    i_filter, i_scalar)
!
      call int_surf_grad_commute_sgs(node1, ele1, surf1, sf_grp1,       &
     &    nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,              &
     &    intg_point_t_evo, sgs_sf%ngrp_sf_dat, sgs_sf%id_grp_sf_dat,   &
     &    i_filter, i_scalar, fem1_wk, f1_nl)
!
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, m_lump%ml, nod_fld1%ntot_phys, i_sgs, nod_fld1%d_fld)
!
      end subroutine cal_grad_commute
!
!-----------------------------------------------------------------------
!
      subroutine cal_rotation_commute(iele_fsmp_stack, m_lump,          &
     &          sgs_sf, i_filter, i_sgs, i_vect)
!
      use int_surf_rot_sgs
!
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint), intent(in) :: i_sgs, i_vect
!
!
!  cal commute error using rotation
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_commute_rot(iele_fsmp_stack, intg_point_t_evo,      &
     &    i_filter, i_vect)
!
      call int_surf_rot_commute_sgs(node1, ele1, surf1, sf_grp1,        &
     &    nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen, sgs_sf,      &
     &    intg_point_t_evo, i_filter, i_vect, fem1_wk, f1_nl)
!
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, m_lump%ml, nod_fld1%ntot_phys, i_sgs, nod_fld1%d_fld)
!
      end subroutine cal_rotation_commute
!
!-----------------------------------------------------------------------
!
      end module commute_error_gradient
