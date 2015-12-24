!commute_error_rotation.f90
!     module commute_error_rotation
!
!     Written by H. Matsui
!
!      subroutine cal_rotation_commute(sgs_sf, i_filter, i_sgs, i_vect)
!      subroutine cal_rotation_commute_fluid(sgs_sf,                    &
!     &          i_filter, i_sgs, i_vect)
!      subroutine cal_rotation_commute_conduct(sgs_sf,                  &
!     &          i_filter, i_sgs, i_vect)
!
!         i_filter: ID for filter function
!
      module commute_error_rotation
!
      use m_precision
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
!
      use int_vol_commute_error
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use int_surf_rot_sgs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_rotation_commute(sgs_sf,                           &
     &          i_filter, i_sgs, i_vect)
!
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
!
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint), intent(in) :: i_sgs, i_vect
!
!
!  cal commute error using rotation
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_commute_rot(ele1%istack_ele_smp, intg_point_t_evo,   &
     &    i_filter, i_vect)
!
      call int_surf_rot_commute_sgs(node1, ele1, surf1, sf_grp1,        &
     &    nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen, sgs_sf,      &
     &    intg_point_t_evo, i_filter, i_vect, fem1_wk, f1_nl)
!
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, m1_lump%ml, nod_fld1%ntot_phys,                     &
     &    i_sgs, nod_fld1%d_fld)
!
      end subroutine cal_rotation_commute
!
!-----------------------------------------------------------------------
!
      subroutine cal_rotation_commute_fluid(sgs_sf,                     &
     &          i_filter, i_sgs, i_vect)
!
      use m_geometry_data_MHD
!
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
!
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint), intent(in) :: i_sgs, i_vect
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_commute_rot(fluid1%istack_ele_fld_smp,               &
     &    intg_point_t_evo, i_filter, i_vect)
      call int_surf_rot_commute_sgs(node1, ele1, surf1, sf_grp1,        &
     &    nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen, sgs_sf,      &
     &    intg_point_t_evo, i_filter, i_vect, fem1_wk, f1_nl)
!
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, mhd_fem1_wk%mlump_fl%ml, nod_fld1%ntot_phys,        &
     &    i_sgs, nod_fld1%d_fld)
!
      end subroutine cal_rotation_commute_fluid
!
!-----------------------------------------------------------------------
!
      subroutine cal_rotation_commute_conduct(sgs_sf,                   &
     &          i_filter, i_sgs, i_vect)
!
      use m_geometry_data_MHD
!
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
!
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint), intent(in) :: i_sgs, i_vect
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_commute_rot                                          &
     &   (conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    i_filter, i_vect)
      call int_surf_rot_commute_sgs(node1, ele1, surf1, sf_grp1,        &
     &    nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen, sgs_sf,      &
     &    intg_point_t_evo, i_filter, i_vect, fem1_wk, f1_nl)
!
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, mhd_fem1_wk%mlump_cd%ml, nod_fld1%ntot_phys,        &
     &    i_sgs, nod_fld1%d_fld)
!
      end subroutine cal_rotation_commute_conduct
!
!-----------------------------------------------------------------------
!
      end module commute_error_rotation
