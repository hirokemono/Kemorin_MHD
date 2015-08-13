!cal_sgs_heat_flux_dynamic.f90
!      module cal_sgs_heat_flux_dynamic
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!      subroutine cal_sgs_hf_dynamic
!
      module cal_sgs_heat_flux_dynamic
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
      subroutine cal_sgs_hf_dynamic
!
      use m_geometry_data
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
      use cal_sgs_heat_fluxes_grad
      use cal_model_diff_coefs
      use clear_work_4_dynamic_model
      use cvt_dynamic_scheme_coord
      use reduce_model_coefs
!
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs(icomp_sgs_hf, iele_smp_stack)
      call s_clear_work_4_dynamic_model(node1%numnod)
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_hf_simi'
      call cal_sgs_hf_simi(iphys%i_SGS_h_flux, iphys%i_sgs_temp,        &
     &    iphys%i_filter_temp, icomp_sgs_hf)
!
!    copy to work array
!
      call copy_vector_component(iphys%i_sgs_simi, iphys%i_SGS_h_flux)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_hf_grad_4_dyn'
      call cal_sgs_filter_hf_grad_4_dyn
!
!   gradient model by original field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_h_flux_grad_4_dyn'
      call cal_sgs_h_flux_grad_4_dyn
!
!      filtering
!
      call cal_filtered_vector(iphys%i_sgs_grad, iphys%i_SGS_h_flux)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_model_coefs', n_vector, iak_sgs_hf, icomp_sgs_hf
      call cal_model_coefs(itype_SGS_h_flux_coef, n_vector,             &
     &    iak_sgs_hf, icomp_sgs_hf, intg_point_t_evo)
!
      call reduce_model_coefs_layer(SGS_hf_factor, nlayer_SGS,          &
     &    sgs_f_clip(1,iak_sgs_hf), sgs_f_whole_clip(iak_sgs_hf) )
      call reduce_ele_vect_model_coefs(SGS_hf_factor,                   &
     &    ak_sgs(1,icomp_sgs_hf))
!
      end subroutine cal_sgs_hf_dynamic
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_heat_flux_dynamic
