!
!     module cal_diff_coef_sgs_hf
!
!     Written by H. Matsui
!
!     subroutine s_cal_diff_coef_sgs_hf
!
      module cal_diff_coef_sgs_hf
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
      subroutine s_cal_diff_coef_sgs_hf
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
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use commute_error_h_flux
      use cal_div_sgs_hf_4_simi
      use cal_sgs_heat_fluxes_grad
      use cal_model_diff_coefs
      use set_boundary_scalars
      use nod_phys_send_recv
      use clear_work_4_dynamic_model
!
!    reset model coefficients
!
      call reset_diff_model_coefs(iak_diff_hf, ele1%istack_ele_smp)
      call s_clear_work_4_dynamic_model
!
!   gradient model by filtered field (to iphys%i_sgs_grad_f)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_hf_grad'
      call cal_sgs_filter_hf_grad
!
!   take divergence of filtered heat flux (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_filter_hf_simi'
       call cal_div_sgs_filter_hf_simi
!
!   take divergence of heat flux (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_h_flux_simi'
       call cal_div_sgs_h_flux_simi
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_scalar(iphys%i_sgs_grad, iphys%i_sgs_grad)
!
!    take difference (to iphys%i_sgs_simi)
!
      call subtract_2_nod_scalars(node1, nod_fld1,                      &
     &    iphys%i_sgs_grad, iphys%i_sgs_simi, iphys%i_sgs_simi)
      call delete_field_by_fixed_t_bc(iphys%i_sgs_simi)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_scalar, iphys%i_sgs_simi)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad_f)
!
      call cal_commute_error_4_filter_hf(ifilter_4delta)
      call delete_field_by_fixed_t_bc(iphys%i_sgs_grad_f)
!
      call scalar_send_recv                                             &
     &   (nod_fld1%ntot_phys, iphys%i_sgs_grad_f, nod_fld1%d_fld)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_scalar, iphys%i_sgs_grad_f)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad)
!
      call cal_commute_error_4_h_flux(ifilter_2delta)
!
      call scalar_send_recv                                             &
     &   (nod_fld1%ntot_phys, iphys%i_sgs_grad, nod_fld1%d_fld)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_scalar(iphys%i_sgs_grad, iphys%i_sgs_grad)
      call delete_field_by_fixed_t_bc(iphys%i_sgs_grad)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_scalar, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_scalar, iak_diff_hf, icomp_diff_hf
      call cal_diff_coef_fluid(n_scalar, iak_diff_hf, icomp_diff_hf,    &
     &    intg_point_t_evo)
!
!
      end subroutine s_cal_diff_coef_sgs_hf
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_sgs_hf
