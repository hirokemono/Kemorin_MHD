!
!      module cal_sgs_mom_flux_dynamic
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!      subroutine cal_sgs_m_flux_dynamic(layer_tbl)
!        type(layering_tbl), intent(in) :: layer_tbl
!
      module cal_sgs_mom_flux_dynamic
!
      use m_precision
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
      subroutine cal_sgs_m_flux_dynamic(layer_tbl)
!
      use m_machine_parameter
      use m_control_parameter
      use m_phys_constants
      use m_nod_comm_table
      use m_geometry_data_MHD
      use m_geometry_data
      use m_node_phys_data
      use m_element_phys_data
      use m_element_id_4_node
      use m_jacobians
      use m_filter_elength
      use m_finite_element_matrix
      use m_int_vol_data
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
      type(layering_tbl), intent(in) :: layer_tbl
!
!    reset model coefficients
!
      call reset_tensor_sgs_model_coefs                                 &
     &   (layer_tbl, icomp_sgs_mf, ele1%istack_ele_smp)
      call s_clear_work_4_dynamic_model
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_mf_simi iphys%i_SGS_m_flux'
      call cal_sgs_mf_simi(iphys%i_SGS_m_flux, iphys%i_velo,            &
     &    iphys%i_filter_velo, icomp_sgs_mf, nod_comm, node1, nod_fld1)
!
!    copy to work array
!
       call copy_tensor_component(node1, nod_fld1,                      &
     &     iphys%i_SGS_m_flux, iphys%i_sgs_simi)
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_sym_tensor, iphys%i_sgs_simi)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_filter_mf_grad_4_dyn'
      call cal_sgs_m_flux_grad_no_coef(ifilter_4delta,                  &
     &    iphys%i_sgs_grad_f, iphys%i_filter_velo, i_dfvx, nod_comm,    &
     &    node1, ele1, fluid1, iphys_ele, fld_ele1, jac1_3d_q,          &
     &    FEM1_elen, rhs_tbl1, fem1_wk, mhd_fem1_wk, nod_fld1)
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_sym_tensor, iphys%i_sgs_grad_f)
!
!   gradient model by original field
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_m_flux_grad_4_dyn'
      call cal_sgs_m_flux_grad_no_coef(ifilter_2delta,                  &
     &    iphys%i_SGS_m_flux, iphys%i_velo, i_dvx, nod_comm,            &
     &    node1, ele1, fluid1, iphys_ele, fld_ele1, jac1_3d_q,          &
     &    FEM1_elen, rhs_tbl1, fem1_wk, mhd_fem1_wk, nod_fld1)
!
!      filtering
!
      call cal_filtered_sym_tensor(nod_comm, node1,                     &
     &    iphys%i_sgs_grad, iphys%i_SGS_m_flux, nod_fld1)
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_sym_tensor, iphys%i_sgs_grad)
!
!   Change coordinate
!
      call cvt_tensor_dynamic_scheme_coord(node1, iphys, nod_fld1)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_model_coefs', n_sym_tensor, iak_sgs_mf, icomp_sgs_mf
      call cal_model_coefs(layer_tbl,                                   &
     &    node1, ele1, iphys, nod_fld1, jac1_3d_q, jac1_3d_l,           &
     &    itype_SGS_m_flux_coef, n_sym_tensor,                          &
     &    iak_sgs_mf, icomp_sgs_mf, intg_point_t_evo)
!
      call reduce_model_coefs_layer(SGS_mf_factor, nlayer_SGS,          &
     &    sgs_f_clip(1,iak_sgs_mf), sgs_f_whole_clip(iak_sgs_mf) )
      call reduce_ele_tensor_model_coefs(SGS_mf_factor,                 &
     &    ak_sgs(1,icomp_sgs_mf))
!
      end subroutine cal_sgs_m_flux_dynamic
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_mom_flux_dynamic
