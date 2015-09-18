!
!     module cal_diff_coef_vector_p
!
!     Written by H. Matsui
!
!     subroutine s_cal_diff_coef_vector_p
!
      module cal_diff_coef_vector_p
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
!
      subroutine s_cal_diff_coef_vector_p
!
      use m_geometry_data
      use m_machine_parameter
      use m_control_parameter
      use m_node_phys_address
      use m_node_phys_data
      use m_phys_constants
      use m_SGS_address
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_filtering_vectors
      use cal_filtering_tensors
      use cal_rotation
      use cal_gradient
      use cal_divergence
      use commute_error_vector
      use commute_error_scalar
      use cal_model_diff_coefs
      use set_vecp_boundary
      use clear_work_4_dynamic_model
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
      call s_clear_work_4_dynamic_model(node1%numnod)
!
!    get filtered scalar potential(to iphys%i_sgs_grad_f)
!
      call copy_vector_component                                        &
     &   (iphys%i_filter_vecp, iphys%i_sgs_grad_f)
      call cal_filtered_scalar(i_sgs_grad_fp, iphys%i_mag_p)
!
!   take rotation and gradient of filtered A (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_rotation_whole', iphys%i_sgs_simi, iphys%i_sgs_grad_f
      call cal_rotation_whole(iflag_mag_supg,                           &
     &    iphys%i_sgs_simi, iphys%i_sgs_grad_f)
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'cal_gradent_whole', i_sgs_simi_p, i_sgs_grad_fp
      call cal_gradent_whole(iflag_mag_supg,                            &
     &    i_sgs_simi_p, i_sgs_grad_fp)
!      call cal_divergence_whole(iflag_mag_supg,                        &
!     &    iphys%i_sgs_simi+6, iphys%i_sgs_grad_f)
!
!   take rotation and gradient of vector potential (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_rotation_whole',            &
     &                     iphys%i_sgs_grad, iphys%i_vecp
      call cal_rotation_whole(iflag_mag_supg,                           &
     &    iphys%i_sgs_grad, iphys%i_vecp)
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'cal_gradent_in_fluid', i_sgs_grad_p, iphys%i_mag_p
      call cal_gradent_in_fluid(iflag_mag_supg,                         &
     &    i_sgs_grad_p, iphys%i_mag_p)
!      call cal_divergence_whole(iflag_mag_supg,                        &
!     &    iphys%i_sgs_grad+6, iphys%i_vecp)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_sym_tensor(iphys%i_sgs_grad, iphys%i_sgs_grad)
!      call cal_filtered_scalar(iphys%i_sgs_grad+6, iphys%i_sgs_grad+6)
!
!    take difference (to iphys%i_sgs_simi)
!
      call subtract_2_nod_tensors                                       &
     &   (iphys%i_sgs_grad, iphys%i_sgs_simi, iphys%i_sgs_simi)
!      call subtract_2_nod_scalars                                      &
!     &   (iphys%i_sgs_grad+6, iphys%i_sgs_simi+6, iphys%i_sgs_simi+6)
!
!      call check_nodal_data(my_rank, n_sym_tensor, iphys%i_sgs_simi)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad_f)
!
      call cal_commute_error_f_vector_p(ifilter_4delta,                 &
     &    iphys%i_sgs_grad_f)
      call cal_commute_error_f_magne_p(ifilter_4delta, i_sgs_grad_fp)
!
      call sym_tensor_send_recv                                         &
     &   (nod_fld1%ntot_phys, iphys%i_sgs_grad_f, d_nod)
!
!      call check_nodal_data(my_rank, n_sym_tensor, iphys%i_sgs_grad_f)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad)
!
      call cal_commute_error_vector_p(ifilter_2delta, iphys%i_sgs_grad)
      call cal_commute_error_magne_p(ifilter_2delta, i_sgs_grad_p)
!
      call sym_tensor_send_recv                                         &
     &   (nod_fld1%ntot_phys, iphys%i_sgs_grad, d_nod)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_sym_tensor(iphys%i_sgs_grad, iphys%i_sgs_grad)
!
!      call check_nodal_data(my_rank, n_sym_tensor, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_sym_tensor, iak_diff_b, icomp_diff_b
      call cal_diff_coef_fluid(n_sym_tensor, iak_diff_b, icomp_diff_b,  &
     &    intg_point_t_evo)
!
      iflag_diff_coefs(iak_diff_b) = 1
!
      end subroutine s_cal_diff_coef_vector_p
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_vector_p
