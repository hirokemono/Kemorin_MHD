!commute_error_scalar.f90
!     module commute_error_scalar
!
!     Written by H. Matsui
!
!
!       subroutine cal_commute_error_temp(i_filter, i_sgs)
!       subroutine cal_commute_error_f_temp(i_filter, i_sgs)
!       subroutine cal_commute_error_press(i_filter, i_sgs)
!       subroutine cal_commute_error_f_press(i_filter, i_sgs)
!       subroutine cal_commute_error_magne_p(i_filter, i_sgs)
!       subroutine cal_commute_error_f_magne_p(i_filter, i_sgs)
!
      module commute_error_scalar
!
      use m_precision
!
      use commute_error_gradient
      use m_geometry_data_MHD
!
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_filter_elength
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_temp(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_temp
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
!
      call cal_grad_commute                                             &
     &   (fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,              &
     &    node1, ele1, surf1, sf_grp1, nod_fld1,                        &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    sf_sgs1_grad_t, i_filter, i_sgs, iphys%i_sgs_temp,            &
     &    fem1_wk, f1_l, f1_nl)
!
      end subroutine cal_commute_error_temp
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_f_temp(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_temp
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
      call cal_grad_commute                                             &
     &   (fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,              &
     &    node1, ele1, surf1, sf_grp1, nod_fld1,                        &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    sf_sgs1_grad_t, i_filter, i_sgs, iphys%i_filter_temp,         &
     &    fem1_wk, f1_l, f1_nl)
!
      end subroutine cal_commute_error_f_temp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_press(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_press
!
       integer(kind = kint), intent(in) :: i_filter, i_sgs
!
      call cal_grad_commute                                             &
     &   (fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,              &
     &    node1, ele1, surf1, sf_grp1, nod_fld1,                        &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    sf_sgs1_grad_p, i_filter, i_sgs, iphys%i_press,               &
     &    fem1_wk, f1_l, f1_nl)
!
      end subroutine cal_commute_error_press
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_f_press(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_press
!
       integer(kind = kint), intent(in) :: i_filter, i_sgs
!
      call cal_grad_commute                                             &
     &   (fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,              &
     &    node1, ele1, surf1, sf_grp1, nod_fld1,                        &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    sf_sgs1_grad_p, i_filter, i_sgs, i_sgs, fem1_wk, f1_l, f1_nl)
!
      end subroutine cal_commute_error_f_press
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_magne_p(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_magne_p
!
       integer(kind = kint), intent(in) :: i_filter, i_sgs
!
      call cal_grad_commute(ele1%istack_ele_smp, m1_lump,               &
     &    node1, ele1, surf1, sf_grp1, nod_fld1,                        &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    sf_sgs1_grad_f, i_filter, i_sgs, iphys%i_mag_p,               &
     &    fem1_wk, f1_l, f1_nl)
!
      end subroutine cal_commute_error_magne_p
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_f_magne_p(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_magne_p
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
      call cal_grad_commute(ele1%istack_ele_smp, m1_lump,               &
     &    node1, ele1, surf1, sf_grp1, nod_fld1,                        &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    sf_sgs1_grad_f, i_filter, i_sgs, i_sgs, fem1_wk, f1_l, f1_nl)
!
      end subroutine cal_commute_error_f_magne_p
!
!-----------------------------------------------------------------------
!
      end module commute_error_scalar
