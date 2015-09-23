!cal_sgs_uxb_dynamic_simi.f90
!      module cal_sgs_uxb_dynamic_simi
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!      subroutine s_cal_sgs_uxb_dynamic_simi
!      subroutine cal_sgs_induct_t_dynamic_simi
!
      module cal_sgs_uxb_dynamic_simi
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
      use m_control_parameter
      use m_finite_element_matrix
      use m_SGS_model_coefs
      use m_SGS_address
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_sgs_uxb_dynamic_simi
!
      use m_geometry_data
      use m_node_phys_address
      use m_finite_element_matrix
!
      use reset_dynamic_model_coefs
      use cal_filtering_vectors
      use cal_sgs_fluxes_simi
      use cal_model_diff_coefs
      use clear_work_4_dynamic_model
      use cvt_dynamic_scheme_coord
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (icomp_sgs_uxb, ele1%istack_ele_smp)
      call s_clear_work_4_dynamic_model
!
!   similarity model with wider filter
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_uxb_simi_wide i_wide_fil_velo'
      call cal_sgs_uxb_simi_wide(iphys%i_sgs_grad_f,                    &
     &    iphys%i_filter_velo, iphys%i_filter_magne,                    &
     &    iphys%i_wide_fil_velo, iphys%i_wide_fil_magne)
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_vector, iphys%i_sgs_simi)
!
!    SGS term by similarity model (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_uxb_simi'
      call cal_sgs_uxb_simi(iphys%i_sgs_simi, iphys%i_velo,             &
     &    iphys%i_magne, iphys%i_filter_velo, iphys%i_filter_magne)
!
!      filtering
!
      call cal_filtered_vector(iphys%i_sgs_grad, iphys%i_sgs_simi)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     & 'cal_model_coefs', n_vector, iak_sgs_uxb, icomp_sgs_uxb
      call cal_model_coefs(itype_SGS_uxb_coef, n_vector,                &
     &    iak_sgs_uxb, icomp_sgs_uxb, intg_point_t_evo)
!
      end subroutine s_cal_sgs_uxb_dynamic_simi
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_dynamic_simi
!
      use m_geometry_data
      use m_node_phys_data
      use m_node_phys_address
!
      use reset_dynamic_model_coefs
      use cal_filtering_vectors
      use cal_sgs_fluxes_simi
      use cal_model_diff_coefs
      use copy_nodal_fields
      use int_element_field_2_node
      use cal_similarity_terms
      use clear_work_4_dynamic_model
      use cvt_dynamic_scheme_coord
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (icomp_sgs_uxb, ele1%istack_ele_smp)
      call reset_vector_sgs_nod_m_coefs                                 &
     &   (icomp_sgs_uxb, node1%istack_nod_smp)
      call s_clear_work_4_dynamic_model
!
!   similarity model with wider filter
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_induct_t_simi_wide i_wide_fil_velo'
      call cal_sgs_induct_t_simi_wide(iphys%i_sgs_grad_f,               &
     &    iphys%i_filter_velo, iphys%i_filter_magne,                    &
     &    iphys%i_wide_fil_velo, iphys%i_wide_fil_magne, icomp_sgs_uxb)
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_vector, iphys%i_sgs_grad_f)
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_induct_t_simi'
      call cal_sgs_induct_t_simi(iphys%i_SGS_induct_t,                  &
     &    iphys%i_velo, iphys%i_magne, iphys%i_filter_velo,             &
     &    iphys%i_filter_magne, icomp_sgs_uxb)
!
!    copy to work array
!
       call copy_vector_component(node1, nod_fld1,                      &
     &     iphys%i_SGS_induct_t, iphys%i_sgs_simi)
!
!      filtering
!
      call cal_filtered_vector(iphys%i_sgs_grad, iphys%i_SGS_induct_t)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     & 'cal_model_coefs', n_asym_tensor, iak_sgs_uxb, icomp_sgs_uxb
      call cal_model_coefs(itype_SGS_uxb_coef, n_asym_tensor,           &
     &    iak_sgs_uxb, icomp_sgs_uxb, intg_point_t_evo)
!
      call cal_ele_vector_2_node(ak_sgs_nod(1,icomp_sgs_uxb),           &
     &    ak_sgs(1,icomp_sgs_uxb) )
!
      end subroutine cal_sgs_induct_t_dynamic_simi
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_uxb_dynamic_simi
