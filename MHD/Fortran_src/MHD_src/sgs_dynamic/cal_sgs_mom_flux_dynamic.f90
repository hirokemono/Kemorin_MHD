!
!      module cal_sgs_mom_flux_dynamic
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!      subroutine cal_sgs_m_flux_dynamic
!
      module cal_sgs_mom_flux_dynamic
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_m_flux_dynamic
!
      use m_machine_parameter
      use m_control_parameter
      use m_phys_constants
      use m_node_phys_address
      use m_SGS_address
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_vectors
      use cal_sgs_fluxes_simi
      use cal_filtering_tensors
      use cal_sgs_mom_fluxes_grad
      use cal_model_diff_coefs
      use clear_work_4_dynamic_model
      use cvt_dynamic_scheme_coord
      use reduce_model_coefs
!
!    reset model coefficients
!
      call reset_tensor_sgs_model_coefs(icomp_sgs_mf)
      call s_clear_work_4_dynamic_model
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_mf_simi iphys%i_SGS_m_flux'
       call cal_sgs_mf_simi(iphys%i_SGS_m_flux, iphys%i_velo,           &
     &     iphys%i_filter_velo, icomp_sgs_mf)
!
!    copy to work array
!
       call copy_tensor_components(iphys%i_sgs_simi,                    &
     &     iphys%i_SGS_m_flux)
!       call check_nodal_data(my_rank, n_sym_tensor, iphys%i_sgs_simi)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_filter_mf_grad_4_dyn'
      call cal_sgs_filter_mf_grad_4_dyn
!       call check_nodal_data(my_rank, n_sym_tensor, iphys%i_sgs_grad_f)
!
!   gradient model by original field
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_m_flux_grad_4_dyn'
      call cal_sgs_m_flux_grad_4_dyn
!
!      filtering
!
      call cal_filtered_sym_tensor(iphys%i_sgs_grad,                    &
     &    iphys%i_SGS_m_flux)
!      call check_nodal_data(my_rank, n_sym_tensor, iphys%i_sgs_grad)
!
!   Change coordinate
!
      call cvt_tensor_dynamic_scheme_coord
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_model_coefs', n_sym_tensor, iak_sgs_mf, icomp_sgs_mf
      call cal_model_coefs(itype_SGS_m_flux_coef, n_sym_tensor,         &
     &    iak_sgs_mf, icomp_sgs_mf, intg_point_t_evo)
!
      call reduce_model_coefs_layer(SGS_mf_factor, n_layer_d,           &
     &    sgs_f_clip(1,iak_sgs_mf), sgs_f_whole_clip(iak_sgs_mf) )
      call reduce_ele_tensor_model_coefs(SGS_mf_factor,                 &
     &    ak_sgs(1,icomp_sgs_mf))
!
      end subroutine cal_sgs_m_flux_dynamic
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_mom_flux_dynamic
