!
!      module cal_sgs_maxwell_dynamic
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!!      subroutine cal_sgs_maxwell_t_dynamic(ie_dbx, ie_dfbx,           &
!!     &          nod_comm, node, ele, iphys, iphys_ele, fld_ele,       &
!!     &          fluid, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,        &
!!     &          FEM_elens, mhd_fem_wk, fem_wk, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: fld_ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_sgs_maxwell_dynamic
!
      use m_precision
!
      use m_control_parameter
      use m_machine_parameter
      use m_phys_constants
      use m_SGS_address
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_layering_ele_list
      use t_MHD_finite_element_mat
      use t_filter_elength
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_maxwell_t_dynamic(ie_dbx, ie_dfbx,             &
     &          nod_comm, node, ele, iphys, iphys_ele, fld_ele,         &
     &          fluid, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,          &
     &          FEM_elens, mhd_fem_wk, fem_wk, nod_fld)
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
      integer (kind=kint), intent(in) :: ie_dbx, ie_dfbx
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: fld_ele
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(phys_data), intent(inout) :: nod_fld
!
!
!    reset model coefficients
!
      call reset_tensor_sgs_model_coefs                                 &
     &   (layer_tbl, icomp_sgs_lor, ele%istack_ele_smp)
      call s_clear_work_4_dynamic_model(node, iphys, nod_fld)
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &        'cal_sgs_mf_simi i_SGS_maxwell', iphys%i_SGS_maxwell
      call cal_sgs_mf_simi(iphys%i_SGS_maxwell, iphys%i_magne,          &
     &    iphys%i_filter_magne, icomp_sgs_lor,                          &
     &    nod_comm, node, nod_fld)
!
!    copy to work array
!
       call copy_tensor_component(node, nod_fld,                        &
     &     iphys%i_SGS_maxwell, iphys%i_sgs_simi)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_filter_mxwl_grad_4_dyn'
      call cal_sgs_m_flux_grad_no_coef(ifilter_4delta,                  &
     &    iphys%i_sgs_grad_f, iphys%i_filter_magne, ie_dfbx, nod_comm,  &
     &    node, ele, fluid, iphys_ele, fld_ele, jac_3d_q,               &
     &    FEM_elens, rhs_tbl, fem_wk, mhd_fem_wk, nod_fld)
!
!   gradient model by original field
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_maxwell_grad_4_dyn'
      call cal_sgs_m_flux_grad_no_coef(ifilter_2delta,                  &
     &    iphys%i_SGS_maxwell, iphys%i_magne, ie_dbx, nod_comm,         &
     &    node, ele, fluid, iphys_ele, fld_ele, jac_3d_q,               &
     &    FEM_elens, rhs_tbl, fem_wk, mhd_fem_wk, nod_fld)
!
!      filtering
!
      call cal_filtered_sym_tensor(nod_comm, node,                      &
     &    iphys%i_sgs_grad, iphys%i_SGS_maxwell, nod_fld)
!
!   Change coordinate
!
      call cvt_tensor_dynamic_scheme_coord(node, iphys, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     & 'cal_model_coefs', n_sym_tensor, iak_sgs_lor, icomp_sgs_lor
      call cal_model_coefs(layer_tbl,                                   &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    itype_SGS_maxwell_coef, n_sym_tensor,                         &
     &    iak_sgs_lor, icomp_sgs_lor, intg_point_t_evo)
!
      end subroutine cal_sgs_maxwell_t_dynamic
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_maxwell_dynamic
