!
!     module cal_diff_coef_sgs_induct
!
!     Written by H. Matsui
!
!     subroutine s_cal_diff_coef_sgs_induct
!
      module cal_diff_coef_sgs_induct
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
      subroutine s_cal_diff_coef_sgs_induct
!
      use m_geometry_data
      use m_machine_parameter
      use m_control_parameter
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
      use m_SGS_address
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_vectors
      use cal_sgs_fluxes_simi
      use commute_error_h_flux
      use commute_error_induct
      use cal_div_sgs_induct_4_simi
      use cal_sgs_inductions_grad
      use cal_model_diff_coefs
      use set_magne_boundary
      use nod_phys_send_recv
      use clear_work_4_dynamic_model
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
      call subtract_2_nod_vectors                                       &
     &   (iphys%i_sgs_grad, iphys%i_sgs_simi, iphys%i_sgs_simi)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_vector, iphys%i_sgs_simi)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad_f)
!
      call cal_commute_error_4_filter_idct(ifilter_4delta)
!
      call vector_send_recv                                             &
     &   (nod_fld1%ntot_phys, iphys%i_sgs_grad_f, nod_fld1%d_fld)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_vector, iphys%i_sgs_grad_f)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad)
!
      call cal_commute_error_4_induct(ifilter_2delta)
!
      call vector_send_recv                                             &
     &   (nod_fld1%ntot_phys, iphys%i_sgs_grad, nod_fld1%d_fld)
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
      call cal_diff_coef_fluid(n_vector, iak_diff_uxb, icomp_diff_uxb,  &
     &    intg_point_t_evo)
!
!
      end subroutine s_cal_diff_coef_sgs_induct
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_sgs_induct
