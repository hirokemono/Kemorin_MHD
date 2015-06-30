!cal_sgs_induction_dynamic.f90
!      module cal_sgs_induction_dynamic
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!      subroutine cal_sgs_induct_t_dynamic
!      subroutine cal_sgs_uxb_dynamic
!
      module cal_sgs_induction_dynamic
!
      use m_precision
!
      use calypso_mpi
      use m_phys_constants
      use m_machine_parameter
      use m_control_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_dynamic
!
      use m_finite_element_matrix
      use m_node_phys_address
      use m_SGS_address
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_vectors
      use cal_sgs_fluxes_simi
      use cal_sgs_uxb_grad
      use cal_model_diff_coefs
      use clear_work_4_dynamic_model
      use cvt_dynamic_scheme_coord
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs(icomp_sgs_uxb)
      call s_clear_work_4_dynamic_model
!
!    SGS term by similarity model (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_uxb_simi'
      call cal_sgs_uxb_simi(iphys%i_sgs_simi, iphys%i_velo,             &
     &    iphys%i_magne, iphys%i_filter_velo, iphys%i_filter_magne)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_uxb_grad_4_dyn'
      call cal_sgs_filter_uxb_grad_4_dyn
!
!   gradient model by original field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_uxb_grad_4_dyn'
      call cal_sgs_uxb_grad_4_dyn
!
!      filtering
!
      call cal_filtered_vector(iphys%i_sgs_grad, iphys%i_SGS_vp_induct)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &        'cal_model_coefs', n_vector, iak_sgs_uxb, icomp_sgs_uxb
      call cal_model_coefs(itype_SGS_uxb_coef, n_vector,                &
     &    iak_sgs_uxb, icomp_sgs_uxb, intg_point_t_evo)
!
      end subroutine cal_sgs_uxb_dynamic
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_dynamic
!
      use m_node_phys_address
      use m_SGS_address
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_vectors
      use cal_sgs_fluxes_simi
      use cal_sgs_inductions_grad
      use cal_model_diff_coefs
      use clear_work_4_dynamic_model
      use cvt_dynamic_scheme_coord
      use reduce_model_coefs
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs(icomp_sgs_uxb)
      call s_clear_work_4_dynamic_model
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_induct_t_simi'
      call cal_sgs_induct_t_simi(iphys%i_SGS_induct_t, iphys%i_velo,    &
     &    iphys%i_magne, iphys%i_filter_velo, iphys%i_filter_magne,     &
     &    icomp_sgs_uxb)
!
!    copy to work array
!
       call copy_vector_component(iphys%i_sgs_simi,                     &
      &    iphys%i_SGS_induct_t)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_filter_idt_grad_4_dyn'
      call cal_sgs_filter_idt_grad_4_dyn
!
!   gradient model by original field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_induct_t_grad_4_dyn'
      call cal_sgs_induct_t_grad_4_dyn
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
      if (iflag_debug.gt.0 )  write(*,*)                                &
     &     'cal_model_coefs', n_asym_tensor, iak_sgs_uxb, icomp_sgs_uxb
      call cal_model_coefs(itype_SGS_uxb_coef, n_asym_tensor,           &
     &    iak_sgs_uxb, icomp_sgs_uxb, intg_point_t_evo)
!
      call reduce_model_coefs_layer(SGS_uxb_factor, nlayer_SGS,         &
     &    sgs_f_clip(1,iak_sgs_uxb), sgs_f_whole_clip(iak_sgs_uxb) )
      call reduce_ele_vect_model_coefs(SGS_uxb_factor,                  &
     &    ak_sgs(1,icomp_sgs_uxb))
!
      end subroutine cal_sgs_induct_t_dynamic
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_induction_dynamic
