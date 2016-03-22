!cal_sgs_heat_flux_dynamic.f90
!      module cal_sgs_heat_flux_dynamic
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!!      subroutine cal_sgs_hf_dynamic                                   &
!!     &         (iak_sgs_hf, icomp_sgs_hf, ie_dvx, ie_dfvx,            &
!!     &          nod_comm, node, ele, iphys, iphys_ele, ele_fld,       &
!!     &          fluid, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,        &
!!     &          FEM_elens, filtering, mhd_fem_wk, fem_wk,             &
!!     &          f_l, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_sgs_heat_flux_dynamic
!
      use m_precision
!
      use m_control_parameter
      use m_machine_parameter
      use m_phys_constants
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
      use t_filtering_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_hf_dynamic                                     &
     &         (iak_sgs_hf, icomp_sgs_hf, ie_dvx, ie_dfvx,              &
     &          nod_comm, node, ele, iphys, iphys_ele, ele_fld,         &
     &          fluid, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,          &
     &          FEM_elens, filtering, mhd_fem_wk, fem_wk,               &
     &          f_l, nod_fld)
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use cal_sgs_heat_fluxes_grad
      use cal_model_diff_coefs
      use clear_work_4_dynamic_model
      use cvt_dynamic_scheme_coord
      use reduce_model_coefs
!
      integer(kind = kint), intent(in) :: iak_sgs_hf, icomp_sgs_hf
      integer(kind = kint), intent(in) :: ie_dvx, ie_dfvx
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(phys_data), intent(inout) :: nod_fld
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (layer_tbl, icomp_sgs_hf, ele%istack_ele_smp)
      call s_clear_work_4_dynamic_model(node, iphys, nod_fld)
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_hf_simi'
      call cal_sgs_hf_simi(iphys%i_SGS_h_flux, iphys%i_sgs_temp,        &
     &    iphys%i_filter_temp, icomp_sgs_hf,                            &
     &    nod_comm, node, iphys, filtering, nod_fld)
!
!    copy to work array
!
      call copy_vector_component(node, nod_fld,                         &
     &    iphys%i_SGS_h_flux, iphys%i_sgs_simi)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_hf_grad_4_dyn'
      call cal_sgs_h_flux_grad_no_coef(ifilter_4delta,                  &
     &    iphys%i_sgs_grad_f, iphys%i_filter_temp, ie_dfvx,             &
     &    nod_comm, node, ele, fluid, iphys_ele, ele_fld, jac_3d_q,     &
     &    rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_l, nod_fld)
!
!   gradient model by original field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_h_flux_grad_4_dyn'
      call cal_sgs_h_flux_grad_no_coef(ifilter_2delta,                  &
     &    iphys%i_SGS_h_flux, iphys%i_sgs_temp, ie_dvx,                 &
     &    nod_comm, node, ele, fluid, iphys_ele, ele_fld, jac_3d_q,     &
     &    rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_l, nod_fld)
!
!      filtering
!
      call cal_filtered_vector_whole(nod_comm, node, filtering,         &
     &    iphys%i_sgs_grad, iphys%i_SGS_h_flux, nod_fld)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord(node, iphys, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_model_coefs', n_vector, iak_sgs_hf, icomp_sgs_hf
      call cal_model_coefs(layer_tbl,                                   &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    itype_SGS_h_flux_coef, n_vector, iak_sgs_hf, icomp_sgs_hf,    &
     &    intg_point_t_evo)
!
      call reduce_model_coefs_layer(SGS_hf_factor, nlayer_SGS,          &
     &    sgs_f_clip(1,iak_sgs_hf), sgs_f_whole_clip(iak_sgs_hf) )
      call reduce_ele_vect_model_coefs(ele, SGS_hf_factor,              &
     &    ak_sgs(1,icomp_sgs_hf))
!
      end subroutine cal_sgs_hf_dynamic
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_heat_flux_dynamic
