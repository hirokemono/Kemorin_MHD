!
!     module cal_diff_coef_sgs_hf
!
!     Written by H. Matsui
!
!     subroutine s_cal_diff_coef_sgs_hf(layer_tbl)
!        type(layering_tbl), intent(in) :: layer_tbl
!
      module cal_diff_coef_sgs_hf
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
      subroutine s_cal_diff_coef_sgs_hf(layer_tbl)
!
      use m_machine_parameter
      use m_control_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_phys_constants
      use m_node_phys_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_filter_elength
      use m_element_phys_data
      use m_SGS_address
      use m_bc_data_ene
      use m_surf_data_temp
      use m_geometry_data_MHD
      use m_int_vol_data
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use commute_error_h_flux
      use cal_div_sgs_flux_simi
      use cal_sgs_heat_fluxes_grad
      use cal_model_diff_coefs
      use set_boundary_scalars
      use nod_phys_send_recv
      use clear_work_4_dynamic_model
!
      type(layering_tbl), intent(in) :: layer_tbl
!
!    reset model coefficients
!
      call reset_diff_model_coefs(iak_diff_hf, ele1%istack_ele_smp)
      call s_clear_work_4_dynamic_model(node1, iphys, nod_fld1)
!
!   gradient model by filtered field (to iphys%i_sgs_grad_f)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_hf_grad'
      call cal_sgs_filter_hf_grad
!
!   take divergence of filtered heat flux (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_filter_hf_simi'
      call cal_div_sgs_hf_simi(iphys%i_sgs_simi, iphys%i_sgs_grad_f,    &
     &    iphys%i_filter_velo, iphys%i_filter_temp,                     &
     &    nod_comm, node1, ele1, fluid1, iphys_ele, fld_ele1,           &
     &    jac1_3d_q, rhs_tbl1, fem1_wk, mhd_fem1_wk,                    &
     &     f1_l, f1_nl, nod_fld1)
!
!   take divergence of heat flux (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_h_flux_simi'
      call cal_div_sgs_hf_simi(iphys%i_sgs_grad, iphys%i_SGS_h_flux,    &
     &    iphys%i_velo, iphys%i_sgs_temp,                               &
     &    nod_comm, node1, ele1, fluid1, iphys_ele, fld_ele1,           &
     &    jac1_3d_q, rhs_tbl1, fem1_wk, mhd_fem1_wk,                    &
     &    f1_l, f1_nl, nod_fld1)
!
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_scalar(nod_comm, node1,                         &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, nod_fld1)
!
!    take difference (to iphys%i_sgs_simi)
!
      call subtract_2_nod_scalars(node1, nod_fld1,                      &
     &    iphys%i_sgs_grad, iphys%i_sgs_simi, iphys%i_sgs_simi)
      call delete_field_by_fixed_t_bc                                   &
     &   (nod_bc1_t, iphys%i_sgs_simi, nod_fld1)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_scalar, iphys%i_sgs_simi)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad_f)
!
      call cal_commute_error_4_hf                                       &
     &   (fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,              &
     &    node1, ele1, surf1, sf_grp1, jac1_3d_q, jac1_sf_grp_2d_q,     &
     &    rhs_tbl1, FEM1_elen, sf_sgs1_grad_t, ifilter_4delta,          &
     &    iphys%i_sgs_grad_f, iphys%i_sgs_grad_f, iphys%i_filter_velo,  &
     &    iphys%i_filter_temp, fem1_wk, f1_l, f1_nl, nod_fld1)
      call delete_field_by_fixed_t_bc                                   &
     &   (nod_bc1_t, iphys%i_sgs_grad_f, nod_fld1)
!
      call scalar_send_recv                                             &
     &   (iphys%i_sgs_grad_f, node1, nod_comm, nod_fld1)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_scalar, iphys%i_sgs_grad_f)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad)
!
      call cal_commute_error_4_hf                                       &
     &   (fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,              &
     &    node1, ele1, surf1, sf_grp1, jac1_3d_q, jac1_sf_grp_2d_q,     &
     &    rhs_tbl1, FEM1_elen, sf_sgs1_grad_t, ifilter_2delta,          &
     &    iphys%i_sgs_grad, iphys%i_SGS_h_flux, iphys%i_velo,           &
     &    iphys%i_sgs_temp, fem1_wk, f1_l, f1_nl, nod_fld1)
!
      call scalar_send_recv                                             &
     &   (iphys%i_sgs_grad, node1, nod_comm, nod_fld1)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_scalar(nod_comm, node1,                         &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, nod_fld1)
      call delete_field_by_fixed_t_bc                                   &
     &   (nod_bc1_t, iphys%i_sgs_grad, nod_fld1)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_scalar, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_scalar, iak_diff_hf, icomp_diff_hf
      call cal_diff_coef_fluid(layer_tbl,                               &
     &    node1, ele1, iphys, nod_fld1, jac1_3d_q, jac1_3d_l,           &
     &    n_scalar, iak_diff_hf, icomp_diff_hf, intg_point_t_evo)
!
!
      end subroutine s_cal_diff_coef_sgs_hf
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_sgs_hf
