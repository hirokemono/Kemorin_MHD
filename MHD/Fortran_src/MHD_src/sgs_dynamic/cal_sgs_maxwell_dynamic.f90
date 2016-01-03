!
!      module cal_sgs_maxwell_dynamic
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!     subroutine cal_sgs_maxwell_t_dynamic(layer_tbl)
!        type(layering_tbl), intent(in) :: layer_tbl
!
      module cal_sgs_maxwell_dynamic
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
      subroutine cal_sgs_maxwell_t_dynamic(layer_tbl)
!
      use m_machine_parameter
      use m_control_parameter
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_data
      use m_jacobians
      use m_SGS_address
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_vectors
      use cal_sgs_fluxes_simi
      use cal_sgs_mom_fluxes_grad
      use cal_filtering_tensors
      use cal_model_diff_coefs
      use clear_work_4_dynamic_model
      use cvt_dynamic_scheme_coord
!
      type(layering_tbl), intent(in) :: layer_tbl
!
!    reset model coefficients
!
      call reset_tensor_sgs_model_coefs                                 &
     &   (layer_tbl, icomp_sgs_lor, ele1%istack_ele_smp)
      call s_clear_work_4_dynamic_model
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &        'cal_sgs_mf_simi i_SGS_maxwell', iphys%i_SGS_maxwell
      call cal_sgs_mf_simi(iphys%i_SGS_maxwell, iphys%i_magne,          &
     &    iphys%i_filter_magne, icomp_sgs_lor)
!
!    copy to work array
!
       call copy_tensor_component(node1, nod_fld1,                      &
     &     iphys%i_SGS_maxwell, iphys%i_sgs_simi)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_filter_mxwl_grad_4_dyn'
      call cal_sgs_filter_mxwl_grad_4_dyn
!
!   gradient model by original field
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_maxwell_grad_4_dyn'
      call cal_sgs_maxwell_grad_4_dyn
!
!      filtering
!
      call cal_filtered_sym_tensor(nod_comm, node1,                     &
     &    iphys%i_sgs_grad, iphys%i_SGS_maxwell, nod_fld1)
!
!   Change coordinate
!
      call cvt_tensor_dynamic_scheme_coord(node1, iphys, nod_fld1)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     & 'cal_model_coefs', n_sym_tensor, iak_sgs_lor, icomp_sgs_lor
      call cal_model_coefs(layer_tbl,                                   &
     &    node1, ele1, iphys, nod_fld1, jac1_3d_q, jac1_3d_l,           &
     &    itype_SGS_maxwell_coef, n_sym_tensor,                         &
     &    iak_sgs_lor, icomp_sgs_lor, intg_point_t_evo)
!
      end subroutine cal_sgs_maxwell_t_dynamic
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_maxwell_dynamic
