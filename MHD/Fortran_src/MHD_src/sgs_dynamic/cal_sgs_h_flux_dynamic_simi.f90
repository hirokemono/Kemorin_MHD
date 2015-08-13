!cal_sgs_h_flux_dynamic_simi.f90
!      module cal_sgs_h_flux_dynamic_simi
!
!     Written by H. Matsui on May, 2009
!
!      subroutine s_cal_sgs_h_flux_dynamic_simi
!
      module cal_sgs_h_flux_dynamic_simi
!
      use m_precision
!
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
      subroutine s_cal_sgs_h_flux_dynamic_simi
!
      use m_geometry_data
      use m_machine_parameter
      use m_control_parameter
      use m_node_phys_address
      use m_SGS_model_coefs
      use m_SGS_address
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_vectors
      use cal_sgs_fluxes_simi
      use cal_model_diff_coefs
      use int_element_field_2_node
      use cal_similarity_terms
      use clear_work_4_dynamic_model
      use cvt_dynamic_scheme_coord
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs(icomp_sgs_hf, iele_smp_stack)
      call reset_vector_sgs_nod_m_coefs                                 &
     &   (icomp_sgs_hf, node1%istack_nod_smp)
      call s_clear_work_4_dynamic_model(node1%numnod)
!
!   similarity model with wider filter
!
      if (iflag_debug.eq.1)                                             &
     &     write(*,*) 'cal_sgs_hf_simi_wide i_wide_fil_temp'
      call cal_sgs_hf_simi_wide(iphys%i_sgs_grad_f,                     &
     &    iphys%i_filter_temp, iphys%i_wide_fil_temp, icomp_sgs_hf)
!       call check_nodal_data(my_rank, n_vector, iphys%i_sgs_grad_f)
!
!    SGS term by similarity model
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sgs_hf_simi'
      call cal_sgs_hf_simi(iphys%i_SGS_h_flux, iphys%i_sgs_temp,        &
     &    iphys%i_filter_temp, icomp_sgs_hf)
!
!    copy to work array
!
       call copy_vector_component(iphys%i_sgs_simi, iphys%i_SGS_h_flux)
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
      if (iflag_debug.eq.1)  write(*,*)' cal_model_coefs',              &
     &   n_vector, iak_sgs_hf, icomp_sgs_hf
      call cal_model_coefs(itype_SGS_h_flux_coef, n_vector,             &
     &    iak_sgs_hf, icomp_sgs_hf, intg_point_t_evo)
!
      call cal_ele_vector_2_node(ak_sgs_nod(1,icomp_sgs_hf),            &
     &    ak_sgs(1,icomp_sgs_hf) )
!
      end subroutine s_cal_sgs_h_flux_dynamic_simi
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_h_flux_dynamic_simi
