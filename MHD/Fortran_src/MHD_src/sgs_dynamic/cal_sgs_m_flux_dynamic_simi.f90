!cal_sgs_m_flux_dynamic_simi.f90
!      module cal_sgs_m_flux_dynamic_simi
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!      subroutine s_cal_sgs_m_flux_dynamic_simi
!      subroutine cal_sgs_maxwell_dynamic_simi
!
      module cal_sgs_m_flux_dynamic_simi
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_phys_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_sgs_m_flux_dynamic_simi
!
      use m_node_phys_address
      use m_SGS_model_coefs
      use m_SGS_address
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_sgs_fluxes_simi
      use cal_filtering_tensors
      use cal_model_diff_coefs
      use int_element_field_2_node
      use cal_similarity_terms
!
      use clear_work_4_dynamic_model
      use cvt_dynamic_scheme_coord
!
!
!    reset model coefficients
!
      call reset_tensor_sgs_model_coefs(icomp_sgs_mf)
      call reset_tensor_sgs_nod_m_coefs(icomp_sgs_mf)
      call s_clear_work_4_dynamic_model
!
!   similarity model with wider filter
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_mf_simi_wide i_wide_fil_velo'
      call cal_sgs_mf_simi_wide(iphys%i_sgs_grad_f,                     &
     &    iphys%i_filter_velo, iphys%i_wide_fil_velo, icomp_sgs_mf)
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
     &    'cal_model_coefs', n_sym_tensor, iak_sgs_mf, icomp_sgs_mf
      call cal_model_coefs(itype_SGS_m_flux_coef, n_sym_tensor,         &
     &    iak_sgs_mf, icomp_sgs_mf, intg_point_t_evo)
!
      call cal_ele_sym_tensor_2_node(ak_sgs_nod(1,icomp_sgs_mf),        &
     &    ak_sgs(1,icomp_sgs_mf) )
!
      end subroutine s_cal_sgs_m_flux_dynamic_simi
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_maxwell_dynamic_simi
!
      use m_node_phys_address
      use m_SGS_model_coefs
      use m_SGS_address
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_sgs_fluxes_simi
      use cal_filtering_tensors
      use cal_model_diff_coefs
      use int_element_field_2_node
      use cal_similarity_terms
!
      use clear_work_4_dynamic_model
      use cvt_dynamic_scheme_coord
!
!
!    reset model coefficients
!
      call reset_tensor_sgs_model_coefs(icomp_sgs_lor)
      call reset_tensor_sgs_nod_m_coefs(icomp_sgs_lor)
      call s_clear_work_4_dynamic_model
!
!   similarity model with wider filter
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_mf_simi_wide i_wide_fil_magne'
      call cal_sgs_mf_simi_wide(iphys%i_sgs_grad_f,                     &
     &    iphys%i_filter_magne, iphys%i_wide_fil_magne, icomp_sgs_lor)
!       call check_nodal_data(my_rank, n_sym_tensor, iphys%i_sgs_grad_f)
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_mf_simi iphys%i_SGS_maxwell'
      call cal_sgs_mf_simi(iphys%i_SGS_maxwell, iphys%i_magne,          &
     &    iphys%i_filter_magne, icomp_sgs_lor)
!
!    copy to work array
!
       call copy_tensor_components(iphys%i_sgs_simi,                    &
     &     iphys%i_SGS_maxwell)
!
!    filtering
!
      call cal_filtered_sym_tensor(iphys%i_sgs_grad,                    &
     &    iphys%i_SGS_maxwell)
!
!   Change coordinate
!
      call cvt_tensor_dynamic_scheme_coord
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_model_coefs', n_sym_tensor, iak_sgs_lor, icomp_sgs_lor
      call cal_model_coefs(itype_SGS_maxwell_coef, n_sym_tensor,        &
     &    iak_sgs_lor, icomp_sgs_lor, intg_point_t_evo)
!
      call cal_ele_sym_tensor_2_node(ak_sgs_nod(1,icomp_sgs_lor),       &
     &    ak_sgs(1,icomp_sgs_lor) )
!
      end subroutine cal_sgs_maxwell_dynamic_simi
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_m_flux_dynamic_simi
