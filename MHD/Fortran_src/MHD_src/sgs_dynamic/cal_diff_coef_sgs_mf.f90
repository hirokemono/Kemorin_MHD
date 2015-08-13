!
!     module cal_diff_coef_sgs_mf
!
!     Written by H. Matsui
!
!     subroutine s_cal_diff_coef_sgs_mf
!
      module cal_diff_coef_sgs_mf
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
      subroutine s_cal_diff_coef_sgs_mf
!
      use m_geometry_data
      use m_machine_parameter
      use m_control_parameter
      use m_node_phys_address
      use m_phys_constants
      use m_SGS_address
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_vectors
      use subtract_nodal_fields
      use cal_sgs_fluxes_simi
      use cal_div_sgs_mf_4_simi
      use commute_error_m_flux
      use cal_sgs_mom_fluxes_grad
      use cal_filtering_vectors
      use cal_model_diff_coefs
      use set_velocity_boundary
      use nod_phys_send_recv
      use clear_work_4_dynamic_model
!
!    reset model coefficients
!
      call reset_diff_model_coefs(iak_diff_mf, iele_smp_stack)
      call s_clear_work_4_dynamic_model(node1%numnod)
!
!   gradient model by filtered field (to iphys%i_sgs_grad_f)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_m_flux_grad'
      call cal_sgs_filter_m_flux_grad
!
!   take divergence of filtered heat flux (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_filter_mf_simi'
       call cal_div_sgs_filter_mf_simi
!
!   take divergence of heat flux (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_m_flux_simi'
       call cal_div_sgs_m_flux_simi
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector(iphys%i_sgs_grad, iphys%i_sgs_grad)
!
!    take difference (to iphys%i_sgs_simi)
!
      call subtract_2_nod_vectors(iphys%i_sgs_simi,                     &
     &    iphys%i_sgs_grad, iphys%i_sgs_simi)
      call delete_field_by_fixed_v_bc(iphys%i_sgs_simi)
!
!      call check_nodal_data(my_rank, n_vector, iphys%i_sgs_simi)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad_f)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_commute_error_4_filter_mf'
      call cal_commute_error_4_filter_mf(ifilter_4delta)
!
      if (iflag_debug.gt.0) write(*,*) 'delete_field_by_fixed_v_bc',    &
     &                     iphys%i_sgs_grad_f
      call delete_field_by_fixed_v_bc(iphys%i_sgs_grad_f)
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &      'vector_send_recv', iphys%i_sgs_grad_f
      call vector_send_recv(iphys%i_sgs_grad_f)
!
!      call check_nodal_data(my_rank, n_vector, iphys%i_sgs_grad_f)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0)   write(*,*) 'cal_commute_error_4_m_flux'
      call cal_commute_error_4_m_flux(ifilter_2delta)
!
      call vector_send_recv(iphys%i_sgs_grad)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector(iphys%i_sgs_grad, iphys%i_sgs_grad)
      call delete_field_by_fixed_v_bc(iphys%i_sgs_grad)
!
!      call check_nodal_data(my_rank, n_vector, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_vector, iak_diff_mf, icomp_diff_mf
      call cal_diff_coef_fluid(n_vector, iak_diff_mf, icomp_diff_mf,    &
     &    intg_point_t_evo)
!
!
      end subroutine s_cal_diff_coef_sgs_mf
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_sgs_mf
