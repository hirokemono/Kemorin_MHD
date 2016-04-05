!cal_sgs_induction_dynamic.f90
!      module cal_sgs_induction_dynamic
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!!      subroutine cal_sgs_uxb_dynamic                                  &
!!     &         (iak_sgs_uxb, icomp_sgs_uxb, ie_dvx, ie_dfvx,          &
!!     &          nod_comm, node, ele, iphys, iphys_ele, ele_fld,       &
!!     &          conduct, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,      &
!!     &          FEM_elens, filtering, wk_filter, mhd_fem_wk,          &
!!     &          fem_wk, f_l, nod_fld, sgs_coefs)
!!      subroutine cal_sgs_induct_t_dynamic(iak_sgs_uxb, icomp_sgs_uxb, &
!!     &          ie_dvx, ie_dbx, ie_dfvx, ie_dfbx,                     &
!!     &          nod_comm, node, ele, iphys, iphys_ele, ele_fld,       &
!!     &          conduct, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,      &
!!     &          FEM_elens, filtering, sgs_coefs_nod, wk_filter,       &
!!     &          mhd_fem_wk, fem_wk, f_l, nod_fld, sgs_coefs)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(MHD_coefficients_type), intent(inout) :: sgs_coefs
!
      module cal_sgs_induction_dynamic
!
      use m_precision
!
      use calypso_mpi
!
      use m_phys_constants
      use m_machine_parameter
      use m_control_parameter
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
      use t_material_property
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_dynamic                                    &
     &         (iak_sgs_uxb, icomp_sgs_uxb, ie_dvx, ie_dfvx,            &
     &          nod_comm, node, ele, iphys, iphys_ele, ele_fld,         &
     &          conduct, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,        &
     &          FEM_elens, filtering, wk_filter, mhd_fem_wk,            &
     &          fem_wk, f_l, nod_fld, sgs_coefs)
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use cal_sgs_uxb_grad
      use cal_model_diff_coefs
      use clear_work_4_dynamic_model
      use cvt_dynamic_scheme_coord
!
      integer(kind=kint), intent(in) :: iak_sgs_uxb, icomp_sgs_uxb
      integer(kind=kint), intent(in) :: ie_dvx, ie_dfvx
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(phys_data), intent(inout) :: nod_fld
      type(MHD_coefficients_type), intent(inout) :: sgs_coefs
!
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (ele, layer_tbl, icomp_sgs_uxb, sgs_coefs)
      call s_clear_work_4_dynamic_model(node, iphys, nod_fld)
!
!    SGS term by similarity model (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_uxb_simi'
      call cal_sgs_uxb_simi(iphys%i_sgs_simi, iphys%i_velo,             &
     &    iphys%i_magne, iphys%i_filter_velo, iphys%i_filter_magne,     &
     &    nod_comm, node, filtering, wk_filter, nod_fld)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_uxb_grad_4_dyn'
      call cal_sgs_vp_induct_grad_no_coef(ifilter_4delta,               &
     &    iphys%i_sgs_grad_f, iphys%i_filter_magne, ie_dfvx,            &
     &    nod_comm, node, ele, conduct, iphys_ele, ele_fld,             &
     &    jac_3d_q, rhs_tbl, FEM_elens, mhd_fem_wk,                     &
     &    fem_wk, f_l, nod_fld)
!
!   gradient model by original field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_uxb_grad_4_dyn'
      call cal_sgs_vp_induct_grad_no_coef(ifilter_2delta,               &
     &    iphys%i_SGS_vp_induct, iphys%i_magne, ie_dvx,                 &
     &    nod_comm, node, ele, conduct, iphys_ele, ele_fld,             &
     &    jac_3d_q, rhs_tbl, FEM_elens, mhd_fem_wk,                     &
     &    fem_wk, f_l, nod_fld)
!
!      filtering
!
      call cal_filtered_vector_whole(nod_comm, node, filtering,         &
     &    iphys%i_sgs_grad, iphys%i_SGS_vp_induct, wk_filter, nod_fld)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord(node, iphys, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &        'cal_model_coefs', n_vector, iak_sgs_uxb, icomp_sgs_uxb
      call cal_model_coefs(layer_tbl,                                   &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    itype_SGS_uxb_coef, n_vector, iak_sgs_uxb, icomp_sgs_uxb,     &
     &    intg_point_t_evo, sgs_coefs)
!
      end subroutine cal_sgs_uxb_dynamic
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_dynamic(iak_sgs_uxb, icomp_sgs_uxb,   &
     &          ie_dvx, ie_dbx, ie_dfvx, ie_dfbx,                       &
     &          nod_comm, node, ele, iphys, iphys_ele, ele_fld,         &
     &          conduct, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,        &
     &          FEM_elens, filtering, sgs_coefs_nod, wk_filter,         &
     &          mhd_fem_wk, fem_wk, f_l, nod_fld, sgs_coefs)
!
      use m_work_4_dynamic_model
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use cal_sgs_inductions_grad
      use cal_model_diff_coefs
      use clear_work_4_dynamic_model
      use cvt_dynamic_scheme_coord
      use reduce_model_coefs
!
      integer(kind=kint), intent(in) :: iak_sgs_uxb, icomp_sgs_uxb
      integer(kind=kint), intent(in) :: ie_dvx, ie_dfvx
      integer(kind=kint), intent(in) :: ie_dbx, ie_dfbx
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(MHD_coefficients_type), intent(in) :: sgs_coefs_nod
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(phys_data), intent(inout) :: nod_fld
      type(MHD_coefficients_type), intent(inout) :: sgs_coefs
!
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (ele, layer_tbl, icomp_sgs_uxb, sgs_coefs)
      call s_clear_work_4_dynamic_model(node, iphys, nod_fld)
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_induct_t_simi'
      call cal_sgs_induct_t_simi                                        &
     &   (iphys%i_SGS_induct_t, iphys%i_velo, iphys%i_magne,            &
     &    iphys%i_filter_velo, iphys%i_filter_magne, icomp_sgs_uxb,     &
     &    nod_comm, node, filtering, sgs_coefs_nod, wk_filter, nod_fld)
!
!    copy to work array
!
       call copy_vector_component(node, nod_fld,                        &
      &    iphys%i_SGS_induct_t, iphys%i_sgs_simi)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_filter_idt_grad_4_dyn'
      call cal_sgs_induct_t_grad_no_coef                                &
     &   (ifilter_4delta, iphys%i_sgs_grad_f,                           &
     &    iphys%i_filter_velo, iphys%i_filter_magne, ie_dfvx, ie_dfbx,  &
     &    nod_comm, node, ele, conduct, iphys_ele, ele_fld,             &
     &    jac_3d_q, rhs_tbl, FEM_elens, fem_wk, mhd_fem_wk,             &
     &    f_l, nod_fld)
!
!   gradient model by original field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_induct_t_grad_4_dyn'
      call cal_sgs_induct_t_grad_no_coef                                &
     &   (ifilter_2delta,  iphys%i_SGS_induct_t,                        &
     &    iphys%i_velo, iphys%i_magne, ie_dvx, ie_dbx,                  &
     &    nod_comm, node, ele, conduct, iphys_ele, ele_fld,             &
     &    jac_3d_q, rhs_tbl, FEM_elens, fem_wk, mhd_fem_wk,             &
     &    f_l, nod_fld)
!
!      filtering
!
      call cal_filtered_vector_whole(nod_comm, node, filtering,         &
     &    iphys%i_sgs_grad, iphys%i_SGS_induct_t, wk_filter, nod_fld)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord(node, iphys, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0 )  write(*,*)                                &
     &     'cal_model_coefs', n_asym_tensor, iak_sgs_uxb, icomp_sgs_uxb
      call cal_model_coefs(layer_tbl,                                   &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    itype_SGS_uxb_coef, n_asym_tensor, iak_sgs_uxb,               &
     &    icomp_sgs_uxb, intg_point_t_evo, sgs_coefs)
!
      call reduce_model_coefs_layer(SGS_uxb_factor,                     &
     &    wk_sgs1%nlayer, wk_sgs1%num_kinds, iak_sgs_uxb,               &
     &    wk_sgs1%fld_clip, wk_sgs1%fld_whole_clip)
      call reduce_ele_vect_model_coefs(ele, SGS_uxb_factor,             &
     &    sgs_coefs%ntot_comp, icomp_sgs_uxb, sgs_coefs%ak)
!
      end subroutine cal_sgs_induct_t_dynamic
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_induction_dynamic
