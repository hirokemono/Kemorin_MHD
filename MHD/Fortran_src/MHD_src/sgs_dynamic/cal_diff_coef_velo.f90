!
!     module cal_diff_coef_velo
!
!     Written by H. Matsui
!
!     subroutine s_cal_diff_coef_velo(layer_tbl)
!       type(layering_tbl), intent(in) :: layer_tbl
!
      module cal_diff_coef_velo
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
      subroutine s_cal_diff_coef_velo(layer_tbl)
!
      use m_machine_parameter
      use m_control_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_node_phys_data
      use m_geometry_data_MHD
      use m_SGS_address
      use m_phys_constants
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_filtering_vectors
      use cal_filtering_tensors
      use cal_gradient
      use cal_rotation
      use cal_divergence
      use commute_error_vector
      use commute_error_scalar
      use cal_model_diff_coefs
      use set_nodal_bc_id_data
      use clear_work_4_dynamic_model
!
      type(layering_tbl), intent(in) :: layer_tbl
!
      integer (kind=kint) :: i_sgs_grad_p, i_sgs_grad_fp, i_sgs_simi_p
!
!
      i_sgs_grad_p =  iphys%i_sgs_grad   + 3
      i_sgs_grad_fp = iphys%i_sgs_grad_f + 3
      i_sgs_simi_p =  iphys%i_sgs_simi   + 3
!
!    reset model coefficients
!
      call reset_diff_model_coefs(iak_diff_v, ele1%istack_ele_smp)
      call s_clear_work_4_dynamic_model
!
!    get filtered pressure(to iphys%i_sgs_grad_f)
!
      call copy_vector_component(node1, nod_fld1,                       &
     &    iphys%i_filter_velo, iphys%i_sgs_grad_f)
      call cal_filtered_scalar(i_sgs_grad_fp, iphys%i_press)
!
!   take rotation and gradient of filtered velocity(to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_rotation_in_fluid',        &
     &                      iphys%i_sgs_simi, iphys%i_sgs_grad_f
      call choose_cal_rotation(iflag_velo_supg,                         &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, node1, ele1, &
     &    iphys%i_filter_velo, iphys%i_sgs_simi)
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'cal_gradent_in_fluid', i_sgs_simi_p, i_sgs_grad_fp
      call cal_gradent_in_fluid(iflag_velo_supg,                        &
     &    i_sgs_simi_p, i_sgs_grad_fp)
!      if (iflag_debug.gt.0)   write(*,*)                               &
!     &    'cal_divergence_in_fluid', iphys%i_sgs_simi+6,               &
!     &    iphys%i_filter_velo
!      call cal_divergence_in_fluid(iflag_velo_supg, iphys%i_sgs_simi+6,&
!     &    iphys%i_filter_velo)
!
!   take rotation and gradient of velocity (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_rotation_in_fluid',         &
     &                     iphys%i_sgs_grad, iphys%i_velo
      call choose_cal_rotation(iflag_velo_supg,                         &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, node1, ele1, &
     &    iphys%i_velo, iphys%i_sgs_grad)
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'cal_gradent_in_fluid', i_sgs_grad_p, iphys%i_press
      call cal_gradent_in_fluid(iflag_velo_supg,                        &
     &    i_sgs_grad_p, iphys%i_press)
!      if (iflag_debug.gt.0)                                            &
!     &   write(*,*) 'cal_divergence_in_fluid', iphys%i_sgs_grad+6,     &
!     &               iphys%i_velo
!      call cal_divergence_in_fluid(iflag_velo_supg,                    &
!     &    iphys%i_sgs_grad+3, iphys%i_velo)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_sym_tensor(iphys%i_sgs_grad, iphys%i_sgs_grad)
!      call cal_filtered_scalar(iphys%i_sgs_grad+6, iphys%i_sgs_grad+6)
!
!    take difference (to iphys%i_sgs_simi)
!
      call subtract_2_nod_tensors(node1, nod_fld1,                      &
     &    iphys%i_sgs_grad, iphys%i_sgs_simi, iphys%i_sgs_simi)
!      call subtract_2_nod_scalars(node1, nod_fld1,                     &
!     &    iphys%i_sgs_grad+6, iphys%i_sgs_simi+6, iphys%i_sgs_simi+6)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_sym_tensor, iphys%i_sgs_simi)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad_f)
!
      call cal_commute_error_f_velo(ifilter_4delta, iphys%i_sgs_grad_f)
      call cal_commute_error_f_press(ifilter_4delta, i_sgs_grad_fp)
!
      call sym_tensor_send_recv                                         &
     &    (iphys%i_sgs_grad_f, node1, nod_comm, nod_fld1)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_sym_tensor, iphys%i_sgs_grad_f)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad)
!
      call cal_commute_error_velo(ifilter_2delta, iphys%i_sgs_grad)
      call cal_commute_error_press(ifilter_2delta, i_sgs_grad_p)
!
!      call sym_tensor_send_recv                                        &
!     &   (iphys%i_sgs_grad, node1, nod_comm, nod_fld1)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_sym_tensor(iphys%i_sgs_grad, iphys%i_sgs_grad)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_sym_tensor, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_sym_tensor, iak_diff_v, icomp_diff_v
      call cal_diff_coef_fluid(layer_tbl,                               &
     &    n_sym_tensor, iak_diff_v, icomp_diff_v, intg_point_t_evo)
!
      iflag_diff_coefs(iak_diff_v) = 1
!
      end subroutine s_cal_diff_coef_velo
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_velo
