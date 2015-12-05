!
!     module cal_diff_coef_sgs_mxwl
!
!     Written by H. Matsui
!
!     subroutine s_cal_diff_coef_sgs_mxwl(layer_tbl)
!        type(layering_tbl), intent(in) :: layer_tbl
!
      module cal_diff_coef_sgs_mxwl
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
      subroutine s_cal_diff_coef_sgs_mxwl(layer_tbl)
!
      use m_nod_comm_table
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
      use cal_filtering_tensors
      use cal_filtering_vectors
      use cal_sgs_fluxes_simi
      use commute_error_maxwell
      use cal_sgs_mom_fluxes_grad
      use cal_div_sgs_mf_4_simi
      use cal_model_diff_coefs
      use set_nodal_bc_id_data
      use nod_phys_send_recv
      use clear_work_4_dynamic_model
!
      type(layering_tbl), intent(in) :: layer_tbl
!
!    reset model coefficients
!
      call reset_diff_model_coefs(iak_diff_lor, ele1%istack_ele_smp)
      call s_clear_work_4_dynamic_model
!
!   gradient model by filtered field (to iphys%i_sgs_grad_f)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_filter_maxwell_grad'
       call cal_sgs_filter_maxwell_grad
!
!   take divergence of filtered heat flux (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_div_sgs_filter_mxwl_simi'
       call cal_div_sgs_filter_mxwl_simi
!
!   take divergence of heat flux (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_maxwell_simi'
       call cal_div_sgs_maxwell_simi
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector(iphys%i_sgs_grad, iphys%i_sgs_grad)
!
!    take difference (to iphys%i_sgs_simi)
!
      call subtract_2_nod_vectors(node1, nod_fld1,                      &
     &    iphys%i_sgs_grad, iphys%i_sgs_simi, iphys%i_sgs_simi)
      call delete_field_by_fixed_v_bc(iphys%i_sgs_simi)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_vector, iphys%i_sgs_simi)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad_f)
!
      call cal_commute_error_4_filter_mxwl(ifilter_4delta)
!
      call vector_send_recv                                             &
     &   (iphys%i_sgs_grad_f, node1, nod_comm, nod_fld1)
      call delete_field_by_fixed_v_bc(iphys%i_sgs_grad_f)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_vector, iphys%i_sgs_grad_f)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad)
!
      call cal_commute_error_4_maxwell(ifilter_2delta)
!
      call vector_send_recv                                             &
     &   (iphys%i_sgs_grad, node1, nod_comm, nod_fld1)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector(iphys%i_sgs_grad, iphys%i_sgs_grad)
      call delete_field_by_fixed_v_bc(iphys%i_sgs_grad)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_vector, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_vector, iak_diff_lor, icomp_diff_lor
      call cal_diff_coef_fluid(layer_tbl,                               &
     &    n_vector, iak_diff_lor, icomp_diff_lor, intg_point_t_evo)
!
!
      end subroutine s_cal_diff_coef_sgs_mxwl
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_sgs_mxwl
