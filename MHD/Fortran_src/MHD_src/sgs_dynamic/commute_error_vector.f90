!commute_error_vector.f90
!     module commute_error_vector
!
!     Written by H. Matsui
!
!       subroutine cal_commute_error_velo(i_filter, i_sgs)
!       subroutine cal_commute_error_f_velo(i_filter, i_sgs)
!       subroutine cal_commute_error_magne(i_filter, i_sgs)
!       subroutine cal_commute_error_f_magne(i_filter, i_sgs)
!       subroutine cal_commute_error_vector_p(i_filter, i_sgs)
!       subroutine cal_commute_error_f_vector_p(i_filter, i_sgs)
!         i_filter: ID for filter function
!
      module commute_error_vector
!
      use m_precision
!
      use m_geometry_data_MHD
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_filter_elength
!
      use commute_error_gradient
!
      implicit none
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_velo(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_torque
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
      call cal_rotation_commute                                         &
     &   (fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,              &
     &    node1, ele1, surf1, sf_grp1, nod_fld1,                        &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    sf_sgs1_grad_v, i_filter, i_sgs, iphys%i_velo,                &
     &    fem1_wk, f1_l, f1_nl)
!
      end subroutine cal_commute_error_velo
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_f_velo(i_filter, i_sgs)
!
      use m_surf_data_torque
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
      call cal_rotation_commute                                         &
     &   (fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,              &
     &    node1, ele1, surf1, sf_grp1, nod_fld1,                        &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    sf_sgs1_grad_v, i_filter, i_sgs, i_sgs,                       &
     &    fem1_wk, f1_l, f1_nl)
!
      end subroutine cal_commute_error_f_velo
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_magne(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_magne
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
      call cal_rotation_commute(ele1%istack_ele_smp, m1_lump,           &
     &    node1, ele1, surf1, sf_grp1, nod_fld1,                        &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    sf_sgs1_grad_b, i_filter, i_sgs, iphys%i_magne,               &
     &    fem1_wk, f1_l, f1_nl)
!
      end subroutine cal_commute_error_magne
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_f_magne(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_magne
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
      call cal_rotation_commute(ele1%istack_ele_smp, m1_lump,           &
     &    node1, ele1, surf1, sf_grp1, nod_fld1,                        &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    sf_sgs1_grad_b, i_filter, i_sgs, i_sgs, fem1_wk, f1_l, f1_nl)
!
      end subroutine cal_commute_error_f_magne
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_vector_p(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_vector_p
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
      call cal_rotation_commute(ele1%istack_ele_smp, m1_lump,           &
     &    node1, ele1, surf1, sf_grp1, nod_fld1,                        &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    sf_sgs1_grad_a, i_filter, i_sgs, iphys%i_vecp,                &
     &    fem1_wk, f1_l, f1_nl)
!
      end subroutine cal_commute_error_vector_p
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_f_vector_p(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_vector_p
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
      call cal_rotation_commute(ele1%istack_ele_smp, m1_lump,           &
     &    node1, ele1, surf1, sf_grp1, nod_fld1,                        &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    sf_sgs1_grad_a, i_filter, i_sgs, i_sgs, fem1_wk, f1_l, f1_nl)
!
       end subroutine cal_commute_error_f_vector_p
!
!-----------------------------------------------------------------------
!
      end module commute_error_vector
