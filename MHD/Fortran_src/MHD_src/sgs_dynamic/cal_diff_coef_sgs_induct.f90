!
!     module cal_diff_coef_sgs_induct
!
!     Written by H. Matsui
!
!     subroutine s_cal_diff_coef_sgs_induct(layer_tbl)
!        type(layering_tbl), intent(in) :: layer_tbl
!
      module cal_diff_coef_sgs_induct
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
      subroutine s_cal_diff_coef_sgs_induct(layer_tbl)
!
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_machine_parameter
      use m_control_parameter
      use m_phys_constants
      use m_node_phys_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_filter_elength
      use m_SGS_address
      use m_geometry_data_MHD
      use m_int_vol_data
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_vectors
      use cal_sgs_fluxes_simi
      use commute_error_h_flux
      use cal_div_sgs_induct_4_simi
      use cal_sgs_inductions_grad
      use cal_model_diff_coefs
      use nod_phys_send_recv
      use clear_work_4_dynamic_model
!
      type(layering_tbl), intent(in) :: layer_tbl
!
!
!    reset model coefficients
!
      call reset_diff_model_coefs(iak_diff_uxb, ele1%istack_ele_smp)
      call s_clear_work_4_dynamic_model
!
!   gradient model by filtered field (to iphys%i_sgs_grad_f)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_induct_grad'
      call cal_sgs_filter_induct_grad
!
!   take divergence of filtered heat flux (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_div_sgs_filter_idct_simi'
       call cal_div_sgs_filter_idct_simi
!
!   take divergence of heat flux (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_induct_simi'
       call cal_div_sgs_induct_simi
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector(iphys%i_sgs_grad, iphys%i_sgs_grad)
!
!    take difference (to iphys%i_sgs_simi)
!
      call subtract_2_nod_vectors(node1, nod_fld1,                      &
     &    iphys%i_sgs_grad, iphys%i_sgs_simi, iphys%i_sgs_simi)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_vector, iphys%i_sgs_simi)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad_f)
!
      call cal_commute_error_4_idct                                     &
     &   (conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,            &
     &    node1, ele1, surf1, sf_grp1, nod_fld1,                        &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    ifilter_4delta, iphys%i_sgs_grad_f, iphys%i_sgs_grad_f,       &
     &    iphys%i_filter_velo, iphys%i_filter_magne,                    &
     &    fem1_wk, f1_l, f1_nl)
!
      call vector_send_recv                                             &
     &   (iphys%i_sgs_grad_f, node1, nod_comm, nod_fld1)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_vector, iphys%i_sgs_grad_f)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad)
!
      call cal_commute_error_4_idct                                     &
     &   (conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,            &
     &    node1, ele1, surf1, sf_grp1, nod_fld1,                        &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    ifilter_2delta, iphys%i_sgs_grad, iphys%i_SGS_induct_t,       &
     &    iphys%i_velo, iphys%i_magne, fem1_wk, f1_l, f1_nl)
!
      call vector_send_recv                                             &
     &   (iphys%i_sgs_grad, node1, nod_comm, nod_fld1)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector(iphys%i_sgs_grad, iphys%i_sgs_grad)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_vector, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_vector, iak_diff_uxb, icomp_diff_uxb
      call cal_diff_coef_fluid(layer_tbl,                               &
     &    n_vector, iak_diff_uxb, icomp_diff_uxb, intg_point_t_evo)
!
!
      end subroutine s_cal_diff_coef_sgs_induct
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_sgs_induct
