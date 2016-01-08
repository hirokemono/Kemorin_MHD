!cal_sgs_h_flux_dynamic_simi.f90
!      module cal_sgs_h_flux_dynamic_simi
!
!     Written by H. Matsui on May, 2009
!
!      subroutine s_cal_sgs_h_flux_dynamic_simi(layer_tbl)
!        type(layering_tbl), intent(in) :: layer_tbl
!
      module cal_sgs_h_flux_dynamic_simi
!
      use m_precision
!
      use m_phys_constants
!
      use t_layering_ele_list
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_sgs_h_flux_dynamic_simi(layer_tbl)
!
      use m_nod_comm_table
      use m_geometry_data
      use m_machine_parameter
      use m_control_parameter
      use m_node_phys_data
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
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
      type(layering_tbl), intent(in) :: layer_tbl
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (layer_tbl, icomp_sgs_hf, ele1%istack_ele_smp)
      call reset_vector_sgs_nod_m_coefs                                 &
     &   (icomp_sgs_hf, node1%istack_nod_smp)
      call s_clear_work_4_dynamic_model(node1, iphys, nod_fld1)
!
!   similarity model with wider filter
!
      if (iflag_debug.eq.1)                                             &
     &     write(*,*) 'cal_sgs_hf_simi_wide i_wide_fil_temp'
      call cal_sgs_hf_simi_wide(iphys%i_sgs_grad_f,                     &
     &    iphys%i_filter_temp, iphys%i_wide_fil_temp, icomp_sgs_hf,     &
     &    nod_comm, node1, iphys, nod_fld1)
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_vector, iphys%i_sgs_grad_f)
!
!    SGS term by similarity model
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sgs_hf_simi'
      call cal_sgs_hf_simi(iphys%i_SGS_h_flux, iphys%i_sgs_temp,        &
     &    iphys%i_filter_temp, icomp_sgs_hf,                            &
     &    nod_comm, node1, iphys, nod_fld1)
!
!    copy to work array
!
      call copy_vector_component(node1, nod_fld1,                       &
     &    iphys%i_SGS_h_flux, iphys%i_sgs_simi)
!
!      filtering
!
      call cal_filtered_vector(nod_comm, node1,                         &
     &    iphys%i_sgs_grad, iphys%i_SGS_h_flux, nod_fld1)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord(node1, iphys, nod_fld1)
!
!     obtain model coefficient
!
      if (iflag_debug.eq.1)  write(*,*)' cal_model_coefs',              &
     &   n_vector, iak_sgs_hf, icomp_sgs_hf
      call cal_model_coefs(layer_tbl,                                   &
     &    node1, ele1, iphys, nod_fld1, jac1_3d_q, jac1_3d_l,           &
     &    itype_SGS_h_flux_coef, n_vector, iak_sgs_hf, icomp_sgs_hf,    &
     &    intg_point_t_evo)
!
      call cal_ele_vector_2_node                                        &
     &   (node1, ele1, jac1_3d_q, rhs_tbl1, m1_lump,                    &
     &    ak_sgs(1,icomp_sgs_hf), ak_sgs_nod(1,icomp_sgs_hf),           &
     &    fem1_wk, f1_l)
!
      end subroutine s_cal_sgs_h_flux_dynamic_simi
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_h_flux_dynamic_simi
