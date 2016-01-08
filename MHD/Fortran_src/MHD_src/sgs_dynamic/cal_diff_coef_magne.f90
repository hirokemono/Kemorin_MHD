!
!     module cal_diff_coef_magne
!
!     Written by H. Matsui
!
!     subroutine s_cal_diff_coef_magne(layer_tbl)
!        type(layering_tbl), intent(in) :: layer_tbl
!
      module cal_diff_coef_magne
!
      use m_precision
!
      use t_layering_ele_list
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_diff_coef_magne(layer_tbl)
!
      use m_machine_parameter
      use m_control_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use m_element_phys_data
      use m_phys_constants
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_geometry_data_MHD
      use m_filter_elength
      use m_SGS_address
      use m_surf_data_magne
      use m_surf_data_magne_p
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_filtering_vectors
      use cal_filtering_tensors
      use cal_rotation
      use cal_divergence
      use cal_gradient
      use commute_error_gradient
      use cal_model_diff_coefs
      use clear_work_4_dynamic_model
!
      type(layering_tbl), intent(in) :: layer_tbl
!
      integer (kind=kint) :: i_sgs_grad_p, i_sgs_grad_fp, i_sgs_simi_p
!
!
      i_sgs_grad_p =  iphys%i_sgs_grad +   3
      i_sgs_grad_fp = iphys%i_sgs_grad_f + 3
      i_sgs_simi_p =  iphys%i_sgs_simi +   3
!
!    reset model coefficients
!
      call reset_diff_model_coefs(iak_diff_b, ele1%istack_ele_smp)
      call s_clear_work_4_dynamic_model(node1, iphys, nod_fld1)
!
!    get filtered scalar potential(to iphys%i_sgs_grad_f)
!
      call copy_vector_component(node1, nod_fld1,                       &
     &    iphys%i_filter_magne, iphys%i_sgs_grad_f)
      call cal_filtered_scalar(nod_comm, node1,                         &
     &    i_sgs_grad_fp, iphys%i_mag_p, nod_fld1)
!
!   take rotation and gradient of filtered B (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_rotation_whole', iphys%i_sgs_simi, iphys%i_sgs_grad_f
      call choose_cal_rotation                                          &
     &   (iflag_mag_supg, iphys%i_sgs_grad_f, iphys%i_sgs_simi,         &
     &    ele1%istack_ele_smp, m1_lump,                                 &
     &    nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,        &
     &    rhs_tbl1, fem1_wk, f1_nl, nod_fld1)
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'cal_gradent_whole', i_sgs_simi_p, i_sgs_grad_fp
      call cal_gradent_whole(iflag_mag_supg,                            &
     &    i_sgs_simi_p, i_sgs_grad_fp)
!      call choose_cal_divergence(node1%istack_nod_smp, m1_lump,         &
!     &    iflag_mag_supg, iphys%i_sgs_grad_f, iphys%i_sgs_simi+6)
!
!   take rotation and gradient of B (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_rotation_whole',            &
     &                     iphys%i_sgs_grad, iphys%i_magne
      call choose_cal_rotation                                          &
     &   (iflag_mag_supg, iphys%i_magne, iphys%i_sgs_grad,              &
     &    ele1%istack_ele_smp, m1_lump,                                 &
     &    nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,        &
     &    rhs_tbl1, fem1_wk, f1_nl, nod_fld1)
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'cal_gradent_in_fluid', i_sgs_grad_p, iphys%i_mag_p
      call cal_gradent_in_fluid(iflag_mag_supg,                         &
     &    i_sgs_grad_p, iphys%i_mag_p)
!      call choose_cal_divergence(node1%istack_nod_smp, m1_lump,         &
!     &    iflag_mag_supg, iphys%i_magne, iphys%i_sgs_grad+6)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_sym_tensor(nod_comm, node1,                     &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, nod_fld1)
!
!    take difference (to iphys%i_sgs_simi)
!
      call subtract_2_nod_tensors(node1, nod_fld1,                      &
     &    iphys%i_sgs_grad, iphys%i_sgs_simi, iphys%i_sgs_simi)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_sym_tensor, iphys%i_sgs_simi)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad_f)
!
      call cal_rotation_commute(ele1%istack_ele_smp, m1_lump,           &
     &    node1, ele1, surf1, sf_grp1, jac1_3d_q, jac1_sf_grp_2d_q,     &
     &    rhs_tbl1, FEM1_elen, sf_sgs1_grad_b, ifilter_4delta,          &
     &    iphys%i_sgs_grad_f, iphys%i_sgs_grad_f,                       &
     &    fem1_wk, f1_l, f1_nl, nod_fld1)
      call cal_grad_commute(ele1%istack_ele_smp, m1_lump,               &
     &    node1, ele1, surf1, sf_grp1, jac1_3d_q, jac1_sf_grp_2d_q,     &
     &    rhs_tbl1, FEM1_elen, sf_sgs1_grad_f, ifilter_4delta,          &
     &    i_sgs_grad_fp, i_sgs_grad_fp, fem1_wk, f1_l, f1_nl, nod_fld1)
!
      call sym_tensor_send_recv                                         &
     &   (iphys%i_sgs_grad_f, node1, nod_comm, nod_fld1)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_sym_tensor, iphys%i_sgs_grad_f)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad)
!
      call cal_rotation_commute(ele1%istack_ele_smp, m1_lump,           &
     &    node1, ele1, surf1, sf_grp1, jac1_3d_q, jac1_sf_grp_2d_q,     &
     &    rhs_tbl1, FEM1_elen, sf_sgs1_grad_b, ifilter_2delta,          &
     &    iphys%i_sgs_grad, iphys%i_magne,                              &
     &    fem1_wk, f1_l, f1_nl, nod_fld1)
      call cal_grad_commute(ele1%istack_ele_smp, m1_lump,               &
     &    node1, ele1, surf1, sf_grp1, jac1_3d_q, jac1_sf_grp_2d_q,     &
     &    rhs_tbl1, FEM1_elen, sf_sgs1_grad_f, ifilter_2delta,          &
     &    i_sgs_grad_p, iphys%i_mag_p, fem1_wk, f1_l, f1_nl, nod_fld1)
!
      call sym_tensor_send_recv                                         &
     &   (iphys%i_sgs_grad, node1, nod_comm, nod_fld1)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_sym_tensor(nod_comm, node1,                     &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, nod_fld1)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_sym_tensor, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_sym_tensor, iak_diff_b, icomp_diff_b
      call cal_diff_coef_fluid(layer_tbl,                               &
     &    node1, ele1, iphys, nod_fld1, jac1_3d_q, jac1_3d_l,           &
     &    n_sym_tensor, iak_diff_b, icomp_diff_b, intg_point_t_evo)
!
      iflag_diff_coefs(iak_diff_b) = 1
!
      end subroutine s_cal_diff_coef_magne
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_magne
