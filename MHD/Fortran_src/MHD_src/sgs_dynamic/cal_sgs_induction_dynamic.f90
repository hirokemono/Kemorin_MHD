!cal_sgs_induction_dynamic.f90
!      module cal_sgs_induction_dynamic
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!!      subroutine cal_sgs_uxb_dynamic                                  &
!!     &         (iak_sgs_uxb, icomp_sgs_uxb, ie_dvx, ie_dfvx, dt,      &
!!     &          FEM_prm, SGS_par, nod_comm, node, ele, iphys,         &
!!     &          iphys_ele, ele_fld, conduct, cd_prop, layer_tbl,      &
!!     &          jacobians, rhs_tbl, FEM_elens, filtering,             &
!!     &          wk_filter, wk_cor, wk_lsq, wk_sgs, mhd_fem_wk, fem_wk,&
!!     &          f_l, nod_fld, sgs_coefs)
!!      subroutine cal_sgs_induct_t_dynamic(iak_sgs_uxb, icomp_sgs_uxb, &
!!     &          ie_dvx, ie_dbx, ie_dfvx, ie_dfbx, dt,                 &
!!     &          FEM_prm, SGS_par, nod_comm, node, ele, iphys,         &
!!     &          iphys_ele, ele_fld, conduct, cd_prop, layer_tbl,      &
!!     &          jacobians, rhs_tbl, FEM_elens, filtering,             &
!!     &          sgs_coefs_nod, wk_filter,  wk_cor, wk_lsq, wk_sgs,    &
!!     &          mhd_fem_wk, fem_wk, f_l, nod_fld, sgs_coefs)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_type), intent(in) :: jacobians
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(dynamis_correlation_data), intent(inout) :: wk_cor
!!        type(dynamis_least_suare_data), intent(inout) :: wk_lsq
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
      module cal_sgs_induction_dynamic
!
      use m_precision
!
      use calypso_mpi
!
      use m_phys_constants
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_table_FEM_const
      use t_layering_ele_list
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_work_layer_correlate
      use t_material_property
      use t_SGS_model_coefs
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
     &         (iak_sgs_uxb, icomp_sgs_uxb, ie_dvx, ie_dfvx, dt,        &
     &          FEM_prm, SGS_par, nod_comm, node, ele, iphys,           &
     &          iphys_ele, ele_fld, conduct, cd_prop, layer_tbl,        &
     &          jacobians, rhs_tbl, FEM_elens, filtering,               &
     &          wk_filter, wk_cor, wk_lsq, wk_sgs, mhd_fem_wk, fem_wk,  &
     &          f_l, nod_fld, sgs_coefs)
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use cal_sgs_uxb_grad
      use cal_model_diff_coefs
      use cvt_dynamic_scheme_coord
!
      integer(kind=kint), intent(in) :: iak_sgs_uxb, icomp_sgs_uxb
      integer(kind=kint), intent(in) :: ie_dvx, ie_dfvx
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_type), intent(in) :: jacobians
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (ele, layer_tbl, icomp_sgs_uxb, sgs_coefs)
      call clear_work_4_dynamic_model(iphys, nod_fld)
!
!    SGS term by similarity model (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_uxb_simi'
      call cal_sgs_uxb_simi(iphys%i_sgs_simi, iphys%i_velo,             &
     &    iphys%i_magne, iphys%i_filter_velo, iphys%i_filter_magne,     &
     &    SGS_par%filter_p, nod_comm, node, filtering,                  &
     &    wk_filter, nod_fld)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_uxb_grad_4_dyn'
      call cal_sgs_vp_induct_grad_no_coef(ifilter_4delta,               &
     &    iphys%i_sgs_grad_f, iphys%i_filter_magne, ie_dfvx, dt,        &
     &    FEM_prm, nod_comm, node, ele, conduct, cd_prop,               &
     &    iphys_ele, ele_fld, jacobians%jac_3d, rhs_tbl, FEM_elens,     &
     &    mhd_fem_wk, fem_wk, f_l, nod_fld)
!
!   gradient model by original field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_uxb_grad_4_dyn'
      call cal_sgs_vp_induct_grad_no_coef(ifilter_2delta,               &
     &    iphys%i_SGS_vp_induct, iphys%i_magne, ie_dvx, dt,             &
     &    FEM_prm, nod_comm, node, ele, conduct, cd_prop,               &
     &    iphys_ele, ele_fld, jacobians%jac_3d, rhs_tbl, FEM_elens,     &
     &    mhd_fem_wk, fem_wk, f_l, nod_fld)
!
!      filtering
!
      call cal_filtered_vector_whole                                    &
     &   (SGS_par%filter_p, nod_comm, node, filtering,                  &
     &    iphys%i_sgs_grad, iphys%i_SGS_vp_induct, wk_filter, nod_fld)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord                              &
     &   (SGS_par%model_p, node, iphys, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &        'cal_model_coefs', n_vector, iak_sgs_uxb, icomp_sgs_uxb
      call cal_model_coefs(SGS_par, layer_tbl, node, ele,               &
     &    iphys, nod_fld, jacobians%jac_3d, jacobians%jac_3d_l,         &
     &    SGS_par%model_p%itype_Csym_uxb, n_vector,                     &
     &    iak_sgs_uxb, icomp_sgs_uxb, FEM_prm%npoint_t_evo_int, wk_cor, &
     &    wk_lsq, wk_sgs, sgs_coefs)
!
      end subroutine cal_sgs_uxb_dynamic
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_dynamic(iak_sgs_uxb, icomp_sgs_uxb,   &
     &          ie_dvx, ie_dbx, ie_dfvx, ie_dfbx, dt,                   &
     &          FEM_prm, SGS_par, nod_comm, node, ele, iphys,           &
     &          iphys_ele, ele_fld, conduct, cd_prop, layer_tbl,        &
     &          jacobians, rhs_tbl, FEM_elens, filtering,               &
     &          sgs_coefs_nod, wk_filter,  wk_cor, wk_lsq, wk_sgs,      &
     &          mhd_fem_wk, fem_wk, f_l, nod_fld, sgs_coefs)
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use cal_sgs_inductions_grad
      use cal_model_diff_coefs
      use cvt_dynamic_scheme_coord
      use reduce_model_coefs
!
      integer(kind=kint), intent(in) :: iak_sgs_uxb, icomp_sgs_uxb
      integer(kind=kint), intent(in) :: ie_dvx, ie_dfvx
      integer(kind=kint), intent(in) :: ie_dbx, ie_dfbx
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_type), intent(in) :: jacobians
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (ele, layer_tbl, icomp_sgs_uxb, sgs_coefs)
      call clear_work_4_dynamic_model(iphys, nod_fld)
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_induct_t_simi'
      call cal_sgs_induct_t_simi                                        &
     &   (iphys%i_SGS_induct_t, iphys%i_velo, iphys%i_magne,            &
     &    iphys%i_filter_velo, iphys%i_filter_magne, icomp_sgs_uxb,     &
     &    SGS_par%filter_p, nod_comm, node, filtering, sgs_coefs_nod,   &
     &    wk_filter, nod_fld)
!
!    copy to work array
!
       call copy_vector_component(nod_fld,                              &
      &    iphys%i_SGS_induct_t, iphys%i_sgs_simi)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_filter_idt_grad_4_dyn'
      call cal_sgs_induct_t_grad_no_coef                                &
     &   (ifilter_4delta, iphys%i_sgs_grad_f,                           &
     &    iphys%i_filter_velo, iphys%i_filter_magne, ie_dfvx, ie_dfbx,  &
     &    dt, FEM_prm, nod_comm, node, ele, conduct, cd_prop,           &
     &    iphys_ele, ele_fld, jacobians%jac_3d, rhs_tbl, FEM_elens,     &
     &    fem_wk, mhd_fem_wk, f_l, nod_fld)
!
!   gradient model by original field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_induct_t_grad_4_dyn'
      call cal_sgs_induct_t_grad_no_coef                                &
     &   (ifilter_2delta,  iphys%i_SGS_induct_t,                        &
     &    iphys%i_velo, iphys%i_magne, ie_dvx, ie_dbx, dt,              &
     &    FEM_prm, nod_comm, node, ele, conduct, cd_prop,               &
     &    iphys_ele, ele_fld, jacobians%jac_3d, rhs_tbl, FEM_elens,     &
     &    fem_wk, mhd_fem_wk, f_l, nod_fld)
!
!      filtering
!
      call cal_filtered_vector_whole                                    &
     &   (SGS_par%filter_p, nod_comm, node, filtering,                  &
     &    iphys%i_sgs_grad, iphys%i_SGS_induct_t, wk_filter, nod_fld)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord                              &
     &   (SGS_par%model_p, node, iphys, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0 )  write(*,*)                                &
     &     'cal_model_coefs', n_asym_tensor, iak_sgs_uxb, icomp_sgs_uxb
      call cal_model_coefs(SGS_par, layer_tbl, node, ele,               &
     &    iphys, nod_fld, jacobians%jac_3d, jacobians%jac_3d_l,         &
     &    SGS_par%model_p%itype_Csym_uxb, n_asym_tensor,                &
     &    iak_sgs_uxb, icomp_sgs_uxb, FEM_prm%npoint_t_evo_int,         &
     &    wk_cor, wk_lsq, wk_sgs, sgs_coefs)
!
      call reduce_model_coefs_layer(SGS_par%model_p%SGS_uxb_factor,     &
     &    wk_sgs%nlayer, wk_sgs%num_kinds, iak_sgs_uxb,                 &
     &    wk_sgs%fld_clip, wk_sgs%fld_whole_clip)
      call reduce_ele_vect_model_coefs                                  &
     &   (ele, SGS_par%model_p%SGS_uxb_factor,                          &
     &    sgs_coefs%ntot_comp, icomp_sgs_uxb, sgs_coefs%ak)
!
      end subroutine cal_sgs_induct_t_dynamic
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_induction_dynamic
