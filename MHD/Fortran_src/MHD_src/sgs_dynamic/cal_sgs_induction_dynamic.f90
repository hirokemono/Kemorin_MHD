!cal_sgs_induction_dynamic.f90
!      module cal_sgs_induction_dynamic
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!      subroutine cal_sgs_induct_t_dynamic(layer_tbl)
!      subroutine cal_sgs_uxb_dynamic(layer_tbl)
!        type(layering_tbl), intent(in) :: layer_tbl
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
      use m_geometry_data_MHD
      use m_nod_comm_table
      use m_geometry_data
      use m_node_phys_data
      use m_element_phys_data
      use m_element_id_4_node
      use m_jacobians
      use m_finite_element_matrix
      use m_int_vol_data
      use m_filter_elength
      use m_SGS_address
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
      subroutine cal_sgs_uxb_dynamic(layer_tbl)
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
      type(layering_tbl), intent(in) :: layer_tbl
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (layer_tbl, icomp_sgs_uxb, ele1%istack_ele_smp)
      call s_clear_work_4_dynamic_model
!
!    SGS term by similarity model (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_uxb_simi'
      call cal_sgs_uxb_simi(iphys%i_sgs_simi, iphys%i_velo,             &
     &    iphys%i_magne, iphys%i_filter_velo, iphys%i_filter_magne,     &
     &    nod_comm, node1, nod_fld1)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_uxb_grad_4_dyn'
      call cal_sgs_vp_induct_grad_no_coef(ifilter_4delta,               &
     &    iphys%i_sgs_grad_f, iphys%i_filter_magne, i_dfvx,             &
     &    nod_comm, node1, ele1, conduct1, iphys_ele, fld_ele1,         &
     &    jac1_3d_q, rhs_tbl1, FEM1_elen, mhd_fem1_wk,                  &
     &    fem1_wk, f1_l, nod_fld1)
!
!   gradient model by original field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_uxb_grad_4_dyn'
      call cal_sgs_vp_induct_grad_no_coef(ifilter_2delta,               &
     &    iphys%i_SGS_vp_induct, iphys%i_magne, i_dvx,                  &
     &    nod_comm, node1, ele1, conduct1, iphys_ele, fld_ele1,         &
     &    jac1_3d_q, rhs_tbl1, FEM1_elen, mhd_fem1_wk,                  &
     &    fem1_wk, f1_l, nod_fld1)
!
!      filtering
!
      call cal_filtered_vector(nod_comm, node1,                         &
     &    iphys%i_sgs_grad, iphys%i_SGS_vp_induct, nod_fld1)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord(node1, iphys, nod_fld1)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &        'cal_model_coefs', n_vector, iak_sgs_uxb, icomp_sgs_uxb
      call cal_model_coefs(layer_tbl,                                   &
     &    node1, ele1, iphys, nod_fld1, jac1_3d_q, jac1_3d_l,           &
     &    itype_SGS_uxb_coef, n_vector, iak_sgs_uxb, icomp_sgs_uxb,     &
     &    intg_point_t_evo)
!
      end subroutine cal_sgs_uxb_dynamic
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_dynamic(layer_tbl)
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
      type(layering_tbl), intent(in) :: layer_tbl
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (layer_tbl, icomp_sgs_uxb, ele1%istack_ele_smp)
      call s_clear_work_4_dynamic_model
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_induct_t_simi'
      call cal_sgs_induct_t_simi(iphys%i_SGS_induct_t, iphys%i_velo,    &
     &    iphys%i_magne, iphys%i_filter_velo, iphys%i_filter_magne,     &
     &    icomp_sgs_uxb, nod_comm, node1, nod_fld1)
!
!    copy to work array
!
       call copy_vector_component(node1, nod_fld1,                      &
      &    iphys%i_SGS_induct_t, iphys%i_sgs_simi)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_filter_idt_grad_4_dyn'
      call cal_sgs_induct_t_grad_no_coef                                &
     &   (ifilter_4delta, iphys%i_sgs_grad_f,                           &
     &    iphys%i_filter_velo, iphys%i_filter_magne, i_dfvx, i_dfbx,    &
     &    nod_comm, node1, ele1, conduct1, iphys_ele, fld_ele1,         &
     &    jac1_3d_q, rhs_tbl1, FEM1_elen, fem1_wk, mhd_fem1_wk,         &
     &    f1_l, nod_fld1)
!
!   gradient model by original field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_induct_t_grad_4_dyn'
      call cal_sgs_induct_t_grad_no_coef                                &
     &   (ifilter_2delta,  iphys%i_SGS_induct_t,                        &
     &    iphys%i_velo, iphys%i_magne, i_dvx, i_dbx,                    &
     &    nod_comm, node1, ele1, conduct1, iphys_ele, fld_ele1,         &
     &    jac1_3d_q, rhs_tbl1, FEM1_elen, fem1_wk, mhd_fem1_wk,         &
     &    f1_l, nod_fld1)
!
!      filtering
!
      call cal_filtered_vector(nod_comm, node1,                         &
     &    iphys%i_sgs_grad, iphys%i_SGS_induct_t, nod_fld1)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord(node1, iphys, nod_fld1)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0 )  write(*,*)                                &
     &     'cal_model_coefs', n_asym_tensor, iak_sgs_uxb, icomp_sgs_uxb
      call cal_model_coefs(layer_tbl,                                   &
     &    node1, ele1, iphys, nod_fld1, jac1_3d_q, jac1_3d_l,           &
     &    itype_SGS_uxb_coef, n_asym_tensor, iak_sgs_uxb,               &
     &    icomp_sgs_uxb, intg_point_t_evo)
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
